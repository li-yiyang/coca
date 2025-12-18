;;;; objc-object.lisp --- Implement ObjC Object wrapper

(in-package :coca.objc)

(defparameter *objc-objects* (tg:make-weak-hash-table :weakness :value)
  "Cache of ObjC Objects in Lisp side. ")

(defun coerce-to-objc-object (pointer)
  "Coerce POINTER as ObjC object.

Parameter:
+ POINTER:
  + `standard-objc-object': return itself
  + `foreign-pointer': cached by pointer address, get object by pointer"
  (declare (type (or standard-objc-object foreign-pointer) pointer))
  (etypecase pointer
    (standard-objc-object pointer)
    (foreign-pointer
     (with-cached ((pointer-address pointer) *objc-objects*)
       (when (object_isClass pointer)
         (error "Pointer ~A pointing to ObjC Class is not an object. " pointer))
       (let ((class (coerce-to-objc-class (object_getClassName pointer))))
         (make-instance class :objc-object-pointer pointer))))))

;; TODO:
;; should or not finalize object?
;; (defmethod initialize-instance :after ((object ns-object) &key)
;;   "When initialize (wrap) NSObject in Lisp side, increase (retain) ARC counter of OBJECT.
;; And when finalize OBJCET in Lisp side, decrease (release) ARC counter of OBJECT.
;; This should ensure that OBJECT will never be free while Lisp is holding it,
;; and should be handled (GCed) when Lisp is not holding it. "
;;   (let ((pointer (objc-object-pointer object)))
;;     (tg:finalize object (lambda () (release pointer)))
;;     (retain object)))

;;;; objc-object.lisp ends here
