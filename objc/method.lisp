;;;; method.lisp --- Implement ObjC method wrapper

(in-package :coca.objc)

(defparameter *methods* (make-hash-table :test 'equal))

(defun compile-objc-method-calling (encoding)
  "Coerce ENCODING to a FFI calling function.
Return the compiled function. "
  (declare (type string encoding))
  (with-cached (encoding *methods*)
    (eval (objc-method-calling-lambda-form encoding))))

;; TODO: need a cleaner code
(defun objc-method-calling-lambda-form (encoding)
  "Generate lambda expression to call foreign function of ENCODING.

Dev Note:
An ObjC method calling lambda should be like

    (lambda (#:object0 #:sel1 #:struct2 #:type3)
      \"SEL-NAME\"
      ({coerce-to-objc-object | coerce-to-objc-class | ... }
       (cffi:foreign-funcall \"objc_msgSend\"
         :pointer (objc-object-pointer #:object0)
         :pointer (objc-object-pointer #:sel1)
         :double  (struct-aref #:struct0 0)
         :double  (struct-aref #:struct0 1)
         ...      ...
         <return>)))

Struct would be expanded by `coca.objc::struct-len', `coca.objc::struct-aref'. "
  (multiple-value-bind (args-encodings ret)
      (decode-objc-type-encoding encoding)
    (let ((*gensym-counter* 0)
          (arg-plist        ())
          (vars             ())
          (declarations     ()))
      (labels ((put-arg-plist (type var &optional coerce-p (type-hint t))
                 "Push TYPE VAR to ARG-PLIST. "
                 (case (atomize type)
                  ((:union :bits)
                   (error "Unknown how to put ~S to funcall. " type))
                  (:struct
                   (push `(type (or ,(second type) simple-vector) ,var) declarations)
                   (loop :with info := (objc-struct-info (second type))
                         :for i :from 0
                         :for slot   :in (objc-struct-info-slots    info)
                         :for type   :in (objc-struct-info-types    info)
                         :do (put-arg-plist type `(struct-aref ,var ,i) t nil)))
                  (:object
                   (when type-hint
                     (push `(type (or standard-objc-object foreign-pointer) ,var) declarations))
                   (push (objc-encoding-cffi-type type)                         arg-plist)
                   (push `(objc-object-pointer ,var)                            arg-plist))
                  (:class
                   (when type-hint
                     (push `(type (or objc-class foreign-pointer) ,var) declarations))
                   (push (objc-encoding-cffi-type type)               arg-plist)
                   (push `(objc-object-pointer ,var)                  arg-plist))
                  (:sel
                   (when type-hint
                     (push `(type (or sel foreign-pointer) ,var) declarations))
                   (push (objc-encoding-cffi-type type)        arg-plist)
                   (push `(objc-object-pointer ,var)           arg-plist))
                  ((:string :pointer :char :unsigned-char) ; not effected by coerce
                   (when type-hint
                     (push `(type ,(objc-encoding-lisp-type type) ,var)  declarations))
                   (push (objc-encoding-cffi-type type)                arg-plist)
                   (push var                                           arg-plist))
                  (otherwise
                   ;; TODO: need a better coerce mechanics
                   (when type-hint
                     (push `(type ,(if coerce-p 'real (objc-encoding-lisp-type type)) ,var)  declarations))
                   (push (objc-encoding-cffi-type type)                                    arg-plist)
                   (push (if coerce-p `(coerce ,var ',(objc-encoding-lisp-type type)) var) arg-plist)))))
        (loop :for encoding :in args-encodings
              :for var  := (gensym (symbol-name (atomize encoding)))
              :do (put-arg-plist encoding var)
                  (push var vars)
              :finally (setf vars         (reverse vars)
                             arg-plist    (reverse arg-plist)
                             declarations (reverse declarations)))
        `(lambda ,vars
           (declare ,@declarations)
           (,(case (atomize ret)
               (:object   'coerce-to-objc-object)
               (:class    'coerce-to-objc-class)
               (:sel      'coerce-to-selector)
               ((:union :struct :bits)
                (error "Unknown how to wrap return type ~S. " ret))
               (otherwise 'progn))
            (foreign-funcall "objc_msgSend" ,@arg-plist ,(objc-encoding-cffi-type ret))))))))

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
       (apply (objc-class-class-method class sel)
              (cons (objc-object-pointer class)
                    (cons (objc-object-pointer sel) args)))))
    (foreign-pointer
     (if (object_isClass object)
         (let ((class (coerce-to-objc-class object))
               (sel   (coerce-to-selector   method)))
           (apply (objc-class-class-method class sel)
                  (cons (objc-object-pointer class)
                        (cons (objc-object-pointer sel) args))))
         (let ((class (coerce-to-objc-class (objc_getClass object)))
               (sel   (coerce-to-selector   method)))
           (apply (objc-class-instance-method class sel)
                  (cons object (cons (objc-object-pointer sel) args))))))
    (standard-objc-object
     (let ((class (class-of object))
           (sel   (coerce-to-selector method)))
       (apply (objc-class-instance-method class sel)
              (cons (objc-object-pointer object)
                    (cons (objc-object-pointer sel) args)))))))

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
