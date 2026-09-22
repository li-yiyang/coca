;;;; subview.lisp --- Providing subviews abstraction

(in-package :coca.appkit)


;;;; base-view

(defclass base-view (obj
                     owned-mixin
                     find-obj-mixin
                     framed-mixin
                     hidden-mixin)
  ((parent
    :initform nil
    :type     (or base-view window null)
    :reader   parent))
  (:documentation
   "Base class for wrapping NSView instance. ")
  (:default-initargs
   :objc-class     "NSView"
   :objc-init      'init-view
   :init-in-main-p t))

(defun init-view (ptr)
  "Create a NSView. "
  (declare (type foreign-pointer ptr))
  (invoke ptr "initWithFrame:" :ns-rect #(0 0 100 100) :object))

(defmethod initialize-instance ((view base-view) &key parent)
  (call-next-method)
  (when parent (add-child parent view)))

(defmethod initialize-instance :after
    ((view base-view) &key (autoresizing-mask :not-sizable))
  (setf (autoresizing-mask view) autoresizing-mask))

(defmethod destroy ((view base-view))
  "Before destroy a VIEW, it should be removed from its parent.
And destroying the view should happen in main thread. "
  (dispatch-main (:throw-to-toplevel t)
    (remove-from-parent view)
    (call-next-method)))

(defgeneric window (obj)
  (:documentation
   "Return the `window' OBJ is in,
or `nil' if OBJ is not attached to any `window'. ")
  (:method ((view base-view))
    (alx:when-let ((parent (parent view)))
      (if (typep parent 'window)
          parent
          (window parent)))))


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
                :object child-ptr))))
  t)

(defmethod remove-child ((parent subview-mixin) (child base-view))
  (with-slots (subviews) parent
    (setf subviews (delete child subviews :test #'eq)))
  (with-slots (parent) child
    (when parent
      (with-ptr child ptr
        (dispatch-main ()
          (invoke ptr "removeFromSuperview")))
      (setf parent nil))))

(defmethod destroy :before ((obj subview-mixin))
  "For `subview-mixin' OBJ, before destroy, destroy its children first. "
  ;; ignore `already-destroyed' events
  (dispatch-main (:throw-to-toplevel t)
    (dolist (child (children obj))
      (destroy child))))


;;;; autoresizing-mask

(defgeneric autoresizing-mask (view)
  (:documentation
   "Autoresizing behaviors for VIEW.

Possible Values:
+ `:not-sizable'
  the VIEW cannot be resized
+ `:min-x-margin'
  the left margin between the VIEW and its superview is flexiable
+ `:max-x-margin'
  the right margin between the VIEW and its superview is flexible.
+ `:min-y-margin'
  the bottom margin between the VIEW and its superview is flexible.
+ `:max-y-margin'
  the top margin between the VIEW and its superview is flexible.
+ `:width-sizable'
  the VIEW's width is flexiable
+ `:height-sizable'
  the VIEW's height is flexiable
")
  (:method ((view base-view))
    (invoke (obj-ptr view)
            "autoresizingMask"
            :ns-autoresizing-mask-options)))

(defmethod (setf autoresizing-mask) (mask (view base-view))
  (let ((mask! (as-ns-autoresizing-mask-options mask)))
    (with-ptr view ptr
      (dispatch-main ()
        (invoke ptr
                "setAutoresizingMask:"
                :ns-autoresizing-mask-options mask!)))
    mask))

;;;; subview.lisp ends here
