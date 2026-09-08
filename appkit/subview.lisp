;;;; subview.lisp --- Providing subviews abstraction

(in-package :coca.appkit)


;;;; base-view

(defclass base-view (obj
                     owned-mixin
                     find-obj-mixin
                     framed-mixin
                     hidden-mixin)
  ((parent
    :initarg  :parent
    :initform nil
    :type     (or base-view window null)
    :reader   parent))
  (:documentation
   "Base class for wrapping NSView instance. ")
  (:default-initargs
   :objc-class     "NSView"
   :objc-init      'init-view
   :init-in-main-p t
   :frame          #(0 0 100 100)))

(defun init-view (ptr)
  "Create a NSView. "
  (declare (type foreign-pointer ptr))
  (invoke ptr "initWithFrame:" :ns-rect #(0 0 100 100))
  ptr)

(defmethod initialize-instance :after ((view base-view) &key parent)
  (when parent
    (add-child parent view)))


;;;; subview-mixin

(defclass subview-mixin ()
  ((subviews
    :initform (make-array 0
                          :element-type 'base-view
                          :adjustable   t
                          :fill-pointer 0)))
  (:documentation
   "Mixin class providing subview functionalities.

The subview pointer should be accessed via

    (objc-ptr SUBVIEW-MIXIN :container-view)

Implement this by generic function

    (defmethod objc-ptr (subview-mixin (eql :container-view))
      )

Typically, this should be different than :ptr (default). "))

(defmethod objc-ptr ((view subview-mixin) (name (eql :container-view)))
  (objc-ptr view :ptr))

(defmethod children ((view subview-mixin))
  (with-slots (subviews) view
    (coerce subviews 'list)))

(defmethod add-child ((parent subview-mixin) (child base-view))
  (with-slots (subviews) parent
    (vector-push-extend child subviews))
  (setf (slot-value child 'parent) parent)
  (with-ptr parent (parent-ptr :container-view)
    (with-ptr child child-ptr
      (dispatch-main ()
        (invoke parent-ptr
                "addSubview:"
                :object child-ptr)))))

(defmethod remove-child ((parent subview-mixin) (child base-view))
  (with-slots (subviews) parent
    (setf subviews (delete child subviews :test #'eq)))
  (with-slots (parent) child
    (when parent
      (with-ptr child ptr
        (dispatch-main ()
          (invoke ptr "removeFromSuperview")))
      (setf parent nil))))

;;;; subview.lisp ends here
