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
Return `t' if success, otherwise `nil' if fails.

Side Effects:
+ if `parent' of CHILD is not PARENT,
  the CHILD is removed from PARENT first
")
  (:method :around (parent child)
    (let ((child-parent (parent child)))
      (cond ((null child-parent)
             (call-next-method))
            ;; skip if CHILD is already child of PARENT
            ((not (eq parent child-parent))
             (remove-child child-parent child)
             (call-next-method))))))

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

(defgeneric item-list (container)
  (:documentation
   "Return a list of items in CONTAINER. "))

(defgeneric item-list-length (container)
  (:documentation
   "Return numbers of items in CONTAINER. ")
  (:method (container)
    (length (item-list container))))

(defgeneric add-item (container item)
  (:documentation
   "Add ITEM to CONTAINER. "))

(defgeneric add-nth-item (container nth item)
  (:documentation
   "Add ITEM to CONTAINER at NTH.

If NTH >= (length (item-list CONTAINER)),
this generic function would be equal to `add-item'.

Parameters:
+ CONTAINER
+ NTH:
+ ITEM
")
  (:method :around (container (nth integer) item)
    (if (>= nth (item-list-length container))
        (add-item container item)
        (call-next-method))))

(defgeneric nth-item (container nth)
  (:documentation
   "Get NTH item in CONTAINER.

This is equal to (nth NTH (item-list CONTAINER)). ")
  (:method (container nth)
    (nth nth (item-list container))))

(defgeneric item-position (container item)
  (:documentation
   "Return ITEM position in CONTAINER list.
Return `nil' if ITEM is not within CONTAINER.

This is equal to (position ITEM (item-list CONTAINER) :test #'equal). ")
  (:method (container item)
    (position item (item-list container) :test #'equal)))

(defgeneric remove-item (container item)
  (:documentation
   "Remove ITEM in CONTAINER.
Return `t' if success, otherwise, `nil'. ")
  (:method (container item)
    (alx:when-let ((nth (item-position container item)))
      (remove-nth-item container nth))))

(defgeneric remove-nth-item (container nth)
  (:documentation
   "Remove NTH item in CONTAINER.
Return `t' if success, otherwise, `nil'.

Parameters:
+ CONTAINER:
+ NTH:
  + if NTH >= (length (item-list CONTAINER)),
    return `nil'
")
  (:method :around (container (nth integer))
    (unless (>= nth (item-list-length container))
      (call-next-method)
      t)))

;;;; hierarchy.lisp ends here
