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
