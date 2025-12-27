;;;; method.lisp --- Implement ObjC method wrapper

(in-package :coca.objc)

;; Dev Note:
;; An ObjC method calling lambda should be like
;;
;;     (lambda (#:object0 #:sel1 #:struct2 #:type3)
;;       (declare (type ... ...))
;;       ({coerce-to-objc-object | coerce-to-objc-class | ... }
;;        (cffi:foreign-funcall-pointer objc-method-implementation
;;          :pointer (objc-object-pointer #:object0) ;; objc-method-calling-arg-form
;;          :pointer (objc-object-pointer #:sel1)
;;          :double  (struct-aref #:struct0 0)
;;          :double  (struct-aref #:struct0 1)
;;          ...      ...
;;          <return>)))
;;
;; Ref:
;; Friday Q&A 2012-11-16: Let's Build objc_msgSend
;; https://www.mikeash.com/pyblog/friday-qa-2012-11-16-lets-build-objc_msgsend.html

(defun objc-class-class-method (class sel)
  "Get function to call CLASS and SEL. "
  (declare (type objc-class class)
           (type sel        sel))
  (the function
    (with-slots (objc-class-methods) class
      (with-cached (sel objc-class-methods)
        (let ((method (class_getClassMethod (objc-object-pointer class)
                                            (objc-object-pointer sel))))
          (when (null-pointer-p method)
            (error "Unknown ObjC class method ~A for ~A. "
                   (sel-name        sel)
                   (objc-class-name class)))
          (let ((imp (method_getImplementation method))
                (enc (method_getTypeEncoding   method)))
            (compile-objc-method-calling imp enc)))))))

(defun objc-class-instance-method (class sel)
  "Get function to call instance of CLASS and SEL. "
  (declare (type objc-class class)
           (type sel        sel))
  (the function
    (with-slots (objc-instance-methods) class
      (with-cached (sel objc-instance-methods)
        (let ((method (class_getInstanceMethod (objc-object-pointer class)
                                               (objc-object-pointer sel))))
          (when (null-pointer-p method)
            (error "Unknown ObjC instance method ~A for ~A. "
                   (sel-name        sel)
                   (objc-class-name class)))
          (let ((imp (method_getImplementation method))
                (enc (method_getTypeEncoding   method)))
            (compile-objc-method-calling imp enc)))))))

(defparameter *methods* (make-hash-table :test 'equal)
  "Cache for `compile-objc-method-calling'.

KEY: method encoding string
VAL: list of lambda-list, type hints, foreign-funcall typed-args")

(defun objc-method-calling-arg-form (encoding arg)
  "Return values of list as `cffi:foreign-funcall' avaliable arguments,
type hint declaration.

Dev Note:
this should expand the ARG as:

    (CFFI-TYPE ARG
     CFFI-TYPE ARG...)

See `:struct' case branch. "
  (case (atomize encoding)
    ((:union :bits)
     (error "Unknown how to put ~S to funcall. " encoding))
    (:struct
     (list (loop :with info  := (objc-struct-info (second encoding))
                 :for idx :from 0
                 :for slot  :in (objc-struct-info-slots info)
                 :for type* :in (objc-struct-info-types info)
                 :for coerce := (eql (atomize type*) :coerce)
                 :for type   := (if coerce (cdr type*) type*)
                 :collect (objc-encoding-cffi-type type)
                 :collect (if coerce
                              `(coerce (struct-aref ,arg ,idx)
                                       ',(objc-encoding-lisp-type type))
                              `(the ,(objc-encoding-lisp-type type)
                                 (struct-aref ,arg ,idx))))
           `(or ,(second encoding) simple-vector)))
    (:object
     (list `(:pointer (the foreign-pointer (objc-object-pointer ,arg)))
           '(or standard-objc-object objc-class foreign-pointer)))
    (:class
     (list `(:pointer (the foreign-pointer (objc-object-pointer ,arg)))
           '(or objc-class foreign-pointer)))
    (:sel
     (list `(:pointer (the foreign-pointer (objc-object-pointer ,arg)))
           '(or sel foreign-pointer)))
    (:coerce
     (let ((encoding (cdr encoding)))
       (case (atomize encoding)
         ;; ignore `:coerce'
         ((:object :class :sel :string :pointer :char :unsigned-char)
          (objc-method-calling-arg-form encoding arg))
         (otherwise
          (list `(,(objc-encoding-cffi-type encoding)
                  (coerce ,arg ',(objc-encoding-lisp-type encoding)))
                t)))))
    (otherwise
     (list (list (objc-encoding-cffi-type encoding) arg)
           (objc-encoding-lisp-type encoding)))))

(defun objc-method-calling-form (encoding)
  "Generate lambda expression to call foreign function of ENCODING.
Return a list of VARS, HINTS, CALL-TYPED-ARGS, WRAP. "
  (with-cached (encoding *methods*)
    (multiple-value-bind (args-encodings ret)
        (decode-objc-type-encoding encoding)
      (loop :with *gensym-counter* := 0
            :for enc :in args-encodings
            :for var := (gensym (symbol-name (atomize enc)))
            :for (arg hint) := (objc-method-calling-arg-form enc var)
            :collect var                :into vars
            :collect arg                :into args
            :collect `(type ,hint ,var) :into hints
            :finally (return (list vars
                                   hints
                                   (apply #'append args)
                                   (objc-encoding-cffi-type ret)
                                   (atomize ret)))))))

(defun compile-objc-method-calling (implementation encoding)
  "Coerce ENCODING to a function calling foreign IMPLEMENTATION.
Return the compiled function.

Parameters:
+ IMPLEMENTATION: foreign pointer to ObjC IMP
+ ENCODING: ObjC type encoding string

Return a compiled lambda function:

    (lambda (ARGS...)
      (WRAP
        (foreign-funcall-pointer IMPLEMENTATION
         ,@TYPE-ARGS...
         ,@OBJC-METHOD-CALLING-FORM-RET)))
"
  (destructuring-bind (vars hints call ret wrap)
      (objc-method-calling-form encoding)
    (let ((call `(foreign-funcall-pointer ,implementation () ,@call ,ret)))
      (compile nil `(lambda ,vars
                      (declare ,@hints)
                      ,(case wrap
                         ((:union :bits)
                          (error "Unknown how to wrap return type ~S. " ret))
                         (:object   `(coerce-to-objc-object (the foreign-pointer ,call)))
                         (:class    `(coerce-to-objc-class* (the foreign-pointer ,call)))
                         (:sel      `(coerce-to-selector    (the foreign-pointer ,call)))
                         (otherwise call)))))))

(defun coerce-to-objc-class* (ptr)
  "PTR could be NULL pointer for `coerce-to-objc-class'. "
  (if (null-pointer-p ptr) nil (coerce-to-objc-class ptr)))

(defun objc-class-of (object)
  "Get the ObjC class of OBJECT.
Return `objc-class'. "
  (declare (type (or symbol string objc-class foreign-pointer objc-pointer) object))
  (etypecase object
    (standard-objc-object          (class-of object))
    ((or symbol string objc-class) (coerce-to-objc-class object))
    (foreign-pointer               (if (object_isClass object)
                                       (coerce-to-objc-class  object)
                                       (class-of (coerce-to-objc-object object))))))

(defun can-invoke-p (object method)
  "Test if OBJECT can invoke METHOD or not.

Parameter:
+ OBJECT:
  + `objc-class' like: test both class and instance method
  + `standard-objc-object' like: test only instance method
+ METHOD: `sel' or things can be `coerce-to-selector'"
  (etypecase object
    (standard-objc-object
     (and (ignore-errors
           (objc-class-instance-method (class-of           object)
                                       (coerce-to-selector method)))
          t))
    ((or symbol string objc-class)
     (let ((class (coerce-to-objc-class object))
           (sel   (coerce-to-selector   method)))
       (or (ignore-errors (and (objc-class-class-method    class sel) t))
           (ignore-errors (and (objc-class-instance-method class sel) t)))))
    (foreign-pointer
     (if (object_isClass object)
         (can-invoke-p (coerce-to-objc-class  object) method)
         (can-invoke-p (coerce-to-objc-object object) method)))))

(defun objc-class-method-signature (object method)
  "Tries to find the relevant method, and returns its signature. "
  (declare (type (or symbol string objc-class foreign-pointer objc-pointer) object)
           (type (or string sel) method))
  (etypecase object
    ((or symbol string objc-class)
     (let* ((class  (coerce-to-objc-class object))
            (class* (objc-object-pointer  class))
            (sel    (coerce-to-selector   method))
            (sel*   (objc-object-pointer  sel))
            (method (class_getClassMethod class* sel*))
            (method (if (null-pointer-p method)
                        (class_getInstanceMethod class* sel*)
                        method)))
       (when (null-pointer-p method)
         (error "Unknown ObjC method ~A for ~A. " sel class))
       (decode-objc-type-encoding (method_getTypeEncoding method))))
    (standard-objc-object
     (let* ((class  (class-of            object))
            (sel    (coerce-to-selector  method))
            (class* (objc-object-pointer class))
            (sel*   (objc-object-pointer sel))
            (method (class_getInstanceMethod class* sel*)))
       (when (null-pointer-p method)
         (error "Unknown ObjC method ~A for ~A. " sel class))
       (decode-objc-type-encoding (method_getTypeEncoding method))))
    (foreign-pointer
     (let ((class (if (object_isClass object)
                      (coerce-to-objc-class object)
                      (coerce-to-objc-class (object_getClassName object)))))
       (objc-class-method-signature class method)))))

(defmacro send (object &rest sending-form)
  "CCL's objc:send style macro for `invoke'.

Example:

    (send object :message-like-this) ;; => (invoke object \"messageLikeThis\")
    (send object :foo-abc a :bar b)  ;; => (invoke object \"fooAbc:Bar:\" a b)

Dev Note:
Just a compability layer.
Use with caution. "
  (let ((len (length (the list sending-form))))
    (when (zerop len)
      (error "Malformed sending form, expecting (send ~A [METHOD-ARG...]). " object))
    (if (= len 1)
        `(invoke ,object ,(str:camel-case (first sending-form)))
        (loop :with sel  := (str:concat (str:camel-case (pop sending-form)) ":")
              :with args := (list (pop sending-form))
              :for (key . rest) :on (cddr sending-form) :by #'cddr
              :for arg := (if (endp rest)
                              (error "Missing argument after ~S. " key)
                              (first rest))
              :do (setf sel (str:concat sel (str:pascal-case key) ":"))
                  (push arg args)
              :finally (return `(invoke ,object ,sel ,@(reverse args)))))))

(defun invoke (object method &rest args)
  "Call METHOD on OBJECT by ARGS.
Return value is warpped as lisp value.

Parameters:
+ OBJECT: object, class to call
+ METHOD: sel, string as function

Return Value:
+ if Class:  wrap as `objc-class'
+ if Object: wrap as `standard-objc-object'
+ if SEL:    wrap as `sel'
+ if struct: as lisp struct (`define-objc-struct')
+ if CFFI compatible value: as CFFI behavior

Example:

    ;; [NSWindow alloc];
    (invoke \"NSWindow\" \"alloc\")

    ;; [NSString initWithUTF8String:\"Hello World\"];
    (invoke \"NSString\" \"initWithUTF8String:\" \"Hello World\")

    ;; [[NSWindow alloc] initWithContentRect:rect
    ;;                             styleMask:style
    ;;                               backing:backing
    ;;                                 defer:defer]
    (invoke (invoke \"NSWindow\" \"alloc\")
            \"initWithContentRect:styleMask:backing:defer:\"
            rect style backing defer)
"
  (declare (type (or symbol string objc-class foreign-pointer objc-pointer) object)
           (type (or string sel) method))
  (etypecase object
    (standard-objc-object
     (let* ((class (class-of object))
            (sel   (coerce-to-selector method))
            (imp   (objc-class-instance-method class sel)))
       (declare (type function imp))
       (apply imp (cons object (cons sel args)))))
    ((or symbol string objc-class)
     (let* ((class  (coerce-to-objc-class object))
            (sel    (coerce-to-selector   method))
            (imp    (objc-class-class-method class sel)))
       (declare (type function imp))
       (apply imp (cons class (cons sel args)))))
    (foreign-pointer
     (if (object_isClass object)
         (let* ((class (coerce-to-objc-class object))
                (sel   (coerce-to-selector   method))
                (imp   (objc-class-class-method class sel)))
           (declare (type function imp))
           (within-objc-call (apply imp (cons class (cons sel args)))))
         (let* ((class (coerce-to-objc-class (objc_getClass object)))
                (sel   (coerce-to-selector   method))
                (imp   (objc-class-instance-method class sel)))
           (declare (type function imp))
           (apply imp (cons object (cons sel args))))))))

;;;; method.lisp end here
