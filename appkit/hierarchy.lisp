;;;; hierarchy.lisp --- Manages hierarchy relationship

(in-package :coca.appkit)

(defgeneric parent (obj)
  (:documentation
   "Return the parent `obj' of OBJ.
Return nil if OBJ is not not attached to any parent. ")
  (:method (obj) nil))

(defmethod (setf parent) (parent child)
  (add-child parent child))

(defgeneric children (obj)
  (:documentation
   "Return a list of children `obj' of VIEW.

An OBJ with no child should return (). ")
  (:method (obj) ()))

(defgeneric add-child (parent child)
  (:documentation
   "Add CHILD to PARENT.

Side Effects:
+ if `parent' of CHILD is not PARENT,
  the CHILD is removed from PARENT first
")
  (:method :before (parent child)
    (unless (eq parent (parent child))
      (remove-child (parent child) child))))

(defgeneric remove-child (parent child)
  (:documentation
   "Remove CHILD from PARENT if it's contained by PARENT.
Return `t' if success, or `nil' if not.

Parameters:
+ PARENT:
+ CHILD: child should be `children' of PARENT,
  + if PARENT is not `parent' of CHILD, return `nil';
  + otherwise, the CHILD is `remove-from-parent',
    and return `t'
")
  (:method :around (parent child)
    (when (eq parent (parent child))
      (call-next-method)
      t)))

(defgeneric remove-from-parent (child)
  (:documentation
   "Remove CHILD from its `parent' if it's attached.

Dev Note:
+ implement `remove-child' for standard behavior")
  (:method (child)
    (alx:when-let ((parent (parent child)))
      (remove-child parent child))))

;;;; hierarchy.lisp ends here
