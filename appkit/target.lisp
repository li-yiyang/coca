;;;; target.lisp ---- Target and Action mixins

(in-package :coca.appkit)

(defclass target-mixin ()
  ((target
    :initarg  :target
    :initform nil
    :type     (or null obj (eql :self))
    :reader   target
    :documentation
    "Target is internally stored as `obj' or `nil'. ")
   (action
    :initarg  :action
    :initform nil
    :type     (or null string sel function)
    :reader   action
    :documentation
    "Action is internally stored as `sel', `nil' or `function'. "))
  (:documentation
   "Mixin class for obj with target and action abstraction. "))

(defmethod initialize-instance :after ((obj target-mixin) &key)
  (with-slots (target action) obj
    (setf (target obj) target
          (action obj) action)))

(defgeneric enabled (obj)
  (:documentation
   "Whether or not OBJ is enabled. ")
  (:method ((obj target-mixin))
    (with-ptr obj ptr
      (invoke ptr "enabled" :bool))))

(defmethod (setf enabled) (enable (obj target-mixin)
                           &aux (enablep (and enable t)))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setEnabled:" :bool enablep))
    enablep))

(defgeneric target (target-mixin)
  (:documentation
   "Get/Set the target of TARGET-MIXIN.

The target could be:
+ `nil'
+ `:self'
+ `obj'
"))

(defmethod (setf target) ((none null) (obj target-mixin))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setTarget:" :object (null-pointer)))
    (setf (slot-value obj 'target) nil)))

(defmethod (setf target) ((target find-obj-mixin) (obj target-mixin))
  (with-ptr obj ptr
    (with-ptr target tar
      (dispatch-main ()
        (invoke ptr "setTarget:" :object tar))))
  (setf (slot-value obj 'target) target))

(defmethod (setf target) ((self (eql :self)) (obj target-mixin))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setTarget:" :object ptr))
    (setf (slot-value obj 'target) obj)))

(defgeneric action (target-mixin)
  (:documentation
   "Get/Set the action of TARGET-MIXIN.

The action could be:
+ `nil'
+ `string' of `sel' name
+ `sel'
+ `function'"))

(defmethod (setf action) ((sel string) (obj target-mixin))
  (setf (action obj) (coerce-to-selector sel)))

(defmethod (setf action) ((sel sel) (obj target-mixin))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setAction:" :sel sel))
    (setf (slot-value obj 'action) sel)))

(defmethod (setf action) ((none null) (obj target-mixin))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setAction" :pointer (null-pointer)))
    (setf (slot-value obj 'action) nil)))

;;;; target.lisp ends here
