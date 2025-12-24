;;;; objc-class.lisp --- ObjC Class wrapper in Lisp

(in-package :coca.objc)

(defparameter *classes* (make-hash-table :test 'equal)
  "Cache of ObjC classes.

KEY: name of ObjC class
VAL: lisp class as ObjC class wrapper")

(defclass objc-class (standard-class objc-pointer)
  ((objc-class-name
    :type    string
    :initarg :objc-class-name
    :reader  objc-class-name)
   (objc-instance-methods
    :initform (make-hash-table :test 'equal)
    :documentation "Methods cache of instance methods.
KEY: `sel' of methods
VAL: a function for FFI calling, see `*methods*'.

See `coca.objc::objc-class-instance-method'. ")
   (objc-class-methods
    :initform (make-hash-table :test 'equal)
    :documentation "Methods cache of class methods.
Key: `sel' of methods
VAL: a function for FFI calling, see `*methods*'.

See `coca.objc::objc-class-class-method'. "))
  (:documentation
   "Metaclass of ObjC object.

Use `coerce-to-objc-class' to make an ObjC Class as lisp class.
All the ObjC class should be cached within `coca.objc::*classes*'.
"))

(defmethod c2mop:validate-superclass ((class objc-class) (super c2mop:standard-class))
  t)

(defmethod print-object ((class objc-class) stream)
  (print-unreadable-object (class stream)
    (format stream
            "~S ~A ~X"
            (class-name class)
            (slot-value class 'objc-class-name)
            (pointer-address (objc-object-pointer class)))))

(defun coerce-to-objc-class (class)
  "Coerces its argument to an Objective-C class pointer.
Return the lisp class of CLASS.


Parameters:
+ CLASS:
  + `objc-class':
  + symbol: find class and assert it's `objc-class'
  + string: of ObjC class
    this would make an `objc-class' instance class with name CLASS-NAME,
    by default the CLASS-NAME is parsed by `str:param-case' from CLASS,
    or you could set it via optional parameter CLASS-NAME

    this would using `coca.objc::objc_getClass' to get class in the ObjC
    runtime, if not found, raise error.
  + foreign-pointer of ObjC class
    the foreign-pointer would be checked via `coca.objc::object_isClass'
    to test if it's a valid foreign-pointer to ObjC Class

    the name of class would be fetched by `coca.objc::class_getName',
    the CLASS-NAME would be used to set the lisp class name if needed
    (see above as string CLASS input)
"
  (declare (type (or string symbol objc-class foreign-pointer) class))
  (flet ((wrap-ptr-as-objc-class (ptr name class-name)
           (declare (type foreign-pointer ptr)
                    (type string          name)
                    (type symbol          class-name))
           (let ((super (class_getSuperClass ptr)))
             (c2mop:ensure-finalized
              (c2mop:ensure-class
               class-name
               :name                class-name
               :metaclass           (find-class 'objc-class)
               :objc-class-name     name
               :objc-object-pointer ptr
               :direct-superclasses (if (null-pointer-p super)
                                        (list (find-class 'standard-objc-object))
                                        (list (coerce-to-objc-class super))))))))
    (the objc-class
      (etypecase class
        (objc-class class)
        (symbol
         (let ((class (find-class class)))
           (assert (typep class 'objc-class))
           class))
        (string
         (with-cached (class *classes*)
           (let ((ptr (objc_getClass class)))
             (when (null-pointer-p ptr)
               (error "Unknown ObjC class `~A' within current ObjC Runtime. " class))
             (wrap-ptr-as-objc-class ptr class (objc-intern class)))))
        (foreign-pointer
         (when (null-pointer-p class)
           (error "NULL pointer is not a pointer of ObjC class"))
         (unless (object_isClass class)
           (error "~S is not a pointer of ObjC class. " class))
         (let ((name (class_getName class)))
           (with-cached (name *classes*)
             (let ((class-name (objc-intern name)))
               (wrap-ptr-as-objc-class class name class-name)))))))))

;;; Foundamental Classes

;; (defun coerce-all-objc-classes ()
;;   "Coerces all ObjC Class within current ObjC runtime into lisp environment.
;; Return a list of all the `objc-class'. "
;;   (let ((counts (objc_getClassList (null-pointer) 0)))
;;     (with-foreign-object (classes :pointer counts)
;;       (when (= counts (objc_getClassList classes counts))
;;         (loop :for i   :from 0
;;               :for cls := (mem-aref classes :pointer i)
;;               :while (not (null-pointer-p cls))
;;               :collect (coerce-to-objc-class cls))))))

;; (coerce-to-objc-class "NSObject")       ; ns-object

;;;; objc-class.lisp ends here
