;;;; hierarchy.lisp --- Manages hierarchy relationship

(in-package :coca.appkit)

(defgeneric parent (obj)
  (:documentation
   "Return the parent `obj' of OBJ.
Return nil if OBJ is not not attached to any parent. ")
  (:method (obj) nil))

(defgeneric children (obj)
  (:documentation
   "Return a list of children `obj' of VIEW.

An OBJ with no child should return (). ")
  (:method (obj) ()))

(defgeneric add-child (parent child)
  (:documentation
   "Add CHILD to PARENT. "))

(defgeneric remove-child (parent child)
  (:documentation
   "Remove CHILD from PARENT if it's contained by PARENT. "))

(defgeneric remove-from-parent (child)
  (:documentation
   "Remove CHILD from its `parent' if it's attached. ")
  (:method (child)
    (alx:when-let ((parent (parent child)))
      (remove-child parent child))))

;;;; hierarchy.lisp ends here
