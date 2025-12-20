;;;; method.lisp --- Implement ObjC method wrapper

(in-package :coca.objc)

(defparameter *methods* (make-hash-table :test 'equal))

;; Dev Note:
;; An ObjC method calling lambda should be like
;;
;;     (lambda (#:object0 #:sel1 #:struct2 #:type3)
;;       (declare (type ... ...))
;;       ({coerce-to-objc-object | coerce-to-objc-class | ... }
;;        (cffi:foreign-funcall \"objc_msgSend\"
;;          :pointer (objc-object-pointer #:object0) ;; objc-method-calling-arg-form
;;          :pointer (objc-object-pointer #:sel1)
;;          :double  (struct-aref #:struct0 0)
;;          :double  (struct-aref #:struct0 1)
;;          ...      ...
;;          <return>)))

(defun compile-objc-method-calling (encoding)
  "Coerce ENCODING to a FFI calling function.
Return the compiled function. "
  (declare (type string encoding))
  (with-cached (encoding *methods*)
    (multiple-value-bind (args-encodings ret)
        (decode-objc-type-encoding encoding)
      (compile nil (objc-method-calling-lambda-form args-encodings ret)))))

(defun objc-method-calling-arg-form (encoding arg)
  "Return values of list as `cffi:foreign-funcall' avaliable arguments,
type hint declaration. "
  (case (atomize encoding)
    ((:union :bits)
     (error "Unknown how to put ~S to funcall. " encoding))
    (:struct
     (values (loop :with info     := (objc-struct-info (second encoding))
                   :for idx :from 0
                   :for slot  :in (objc-struct-info-slots info)
                   :for type* :in (objc-struct-info-types info)
                   :for coerce := (eql (atomize type*) :coerce)
                   :for type   := (if coerce (cdr type*) type*)
                   :collect (objc-encoding-cffi-type type)
                   :collect (if coerce
                                `(coerce (struct-aref ,arg ,idx) ',(objc-encoding-lisp-type type))
                                `(struct-aref ,arg ,idx)))
             `(or ,(second encoding) simple-vector)))
    (:object
     (values `(:pointer (objc-object-pointer ,arg))
             '(or standard-objc-object foreign-pointer)))
    (:class
     (values `(:pointer (objc-object-pointer ,arg))
             '(or objc-class foreign-pointer)))
    (:sel
     (values `(:pointer (objc-object-pointer ,arg))
             '(or sel foreign-pointer)))
    (:coerce
     (let ((encoding (cdr encoding)))
       (case (atomize encoding)
         ;; ignore `:coerce'
         ((:object :class :sel :string :pointer :char :unsigned-char)
          (objc-method-calling-arg-form encoding arg))
         (otherwise
          (values `(,(objc-encoding-cffi-type encoding)
                    (coerce ,arg ',(objc-encoding-lisp-type encoding)))
                  t)))))
    (otherwise
     (values (list (objc-encoding-cffi-type encoding) arg)
             (objc-encoding-lisp-type encoding)))))

;; TODO: need a cleaner code
(defun objc-method-calling-lambda-form (args-encodings ret)
  "Generate lambda expression to call foreign function of ARGS-ENCODINGS and RET."
  (loop :with *gensym-counter* := 0
        :for enc :in args-encodings
        :for var := (gensym (symbol-name (atomize enc)))
        :for (arg hint) := (multiple-value-list (objc-method-calling-arg-form enc var))
        :collect var                :into vars
        :collect arg                :into args
        :collect `(type ,hint ,var) :into hints
        :finally (return
                   `(lambda ,vars
                      (declare ,@hints)
                      (,(case (atomize ret)
                          ((:union :bits)
                           (error "Unknown how to wrap return type ~S. " ret))
                          (:object   'coerce-to-objc-object)
                          (:class    'coerce-to-objc-class)
                          (:sel      'coerce-to-selector)
                          (otherwise 'progn))
                       (foreign-funcall "objc_msgSend"
                                        ,@(apply #'append args)
                                        ,(objc-encoding-cffi-type ret)))))))

(defun objc-class-class-method (class sel)
  "Get function to call CLASS and SEL. "
  (declare (type objc-class class)
           (type sel        sel))
  (with-slots (objc-class-methods objc-object-pointer) class
    (the function
         (with-cached (sel objc-class-methods)
           (let* ((method   (class_getClassMethod objc-object-pointer
                                                  (objc-object-pointer sel)))
                  (encoding (method_getTypeEncoding method)))
             (with-cached (encoding *methods*)
               (compile-objc-method-calling encoding)))))))

(defun objc-class-instance-method (class sel)
  "Get function to call instance of CLASS and SEL. "
  (declare (type objc-class class)
           (type sel        sel))
  (with-slots (objc-instance-methods objc-object-pointer) class
    (the function
         (with-cached (sel objc-instance-methods)
           (let* ((method   (class_getInstanceMethod objc-object-pointer
                                                     (objc-object-pointer sel)))
                  (encoding (method_getTypeEncoding method)))
             (with-cached (encoding *methods*)
               (compile-objc-method-calling encoding)))))))

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

(defun can-invoke-p (object method)
  "Test if OBJECT can invoke METHOD or not. "
  (declare (type (or symbol string objc-class foreign-pointer objc-pointer) object)
           (type (or string sel) method))
  (etypecase object
    ((or symbol string objc-class)
     (let* ((class  (coerce-to-objc-class object))
            (class* (objc-object-pointer  class))
            (sel    (coerce-to-selector   method))
            (sel*   (objc-object-pointer  sel))
            (method (class_getClassMethod class* sel*)))
       (not (null-pointer-p method))))
    (standard-objc-object
     (let* ((class  (class-of            object))
            (sel    (coerce-to-selector  method))
            (class* (objc-object-pointer class))
            (sel*   (objc-object-pointer sel))
            (method (class_getInstanceMethod class* sel*)))
       (not (null-pointer-p method))))
    (foreign-pointer
     (if (object_isClass object)
         (can-invoke-p (coerce-to-objc-class  object) method)
         (can-invoke-p (coerce-to-objc-object object) method)))))

(defun invoke (object method &rest args)
  "Call METHOD on OBJECT by ARGS.

Parameters:
+ OBJECT: object, class to call
+ METHOD: sel, string as function"
  (declare (type (or symbol string objc-class foreign-pointer objc-pointer) object)
           (type (or string sel) method))
  (etypecase object
    ((or symbol string objc-class)
     (let ((class  (coerce-to-objc-class object))
           (sel    (coerce-to-selector   method)))
       (apply (the function (objc-class-class-method class sel))
              (cons class (cons sel args)))))
    (foreign-pointer
     (if (object_isClass object)
         (let ((class (coerce-to-objc-class object))
               (sel   (coerce-to-selector   method)))
           (apply (the function (objc-class-class-method class sel))
                  (cons class (cons sel args))))
         (let ((class (coerce-to-objc-class (objc_getClass object)))
               (sel   (coerce-to-selector   method)))
           (apply (the function (objc-class-instance-method class sel))
                  (cons object (cons sel args))))))
    (standard-objc-object
     (let ((class (class-of object))
           (sel   (coerce-to-selector method)))
       (apply (the function (objc-class-instance-method class sel))
              (cons object (cons sel args)))))))

;;; Trivial test

;; (trivial-main-thread:with-body-in-main-thread ()
;;   (sb-int:set-floating-point-modes :traps nil)
;;   (let ((app (invoke "NSApplication" "sharedApplication"))
;;         (win (invoke (invoke "NSWindow" "alloc")
;;                      "initWithContentRect:styleMask:backing:defer:"
;;                      (make-ns-rect :x 100 :y 100 :w 300 :h 300)
;;                      15
;;                      2
;;                      nil)))
;;     (print app)
;;     (print win)
;;     (invoke win "setIsVisible:" t)
;;     (invoke app "run")))

;;;; method.lisp end here
