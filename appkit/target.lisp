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
    :type     (or null string sel function symbol)
    :reader   action
    :documentation
    "Action is internally stored as `sel', `nil' or `function'. "))
  (:documentation
   "Mixin class for obj with target and action abstraction. "))

(defmethod initialize-instance :after ((obj target-mixin) &key)
  (with-slots (target action) obj
    (setf (target obj) target
          (action obj) action)))

(defgeneric enabledp (obj)
  (:documentation
   "Whether or not OBJ is enabled. ")
  (:method ((obj target-mixin))
    (with-ptr obj ptr
      (invoke ptr "enabled" :bool))))

(defmethod (setf enabledp) (enable (obj target-mixin)
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
+ `function'
  the function should be like: 

      (lambda (self sender)
        (declare (type obj self)
                 (type (or foreign-pointer obj) sender))
        ...)
  
  Parameters:
  + SELF: the `target-mixin' object itself;
  + SENDER: try to `find-obj' of the sender first, 
    if the sender is not findable, 
    it would be the foreign-pointer to sender directly

  the return value of function will be ignored, 
  the action will be invoked in main thread (GUI thread),
  so it is adviced to switch to background thread if
  the action contains some heavy computation.
+ `symbol' symbol to function

Dev Note: 
+ when action is set to be symbol or function, 
  it will use cocaRespondActionInLisp: SEL as target")
  (:method (obj) nil))

(define-objc-method
    ("NSObject" "cocaRespondActionInLisp:" :encoding "v@:@")
    :void ((sender :object))
  (alx:when-let* ((self   (find-obj self))
                  (action (action   self)))
    (when (or (functionp action)
              (symbolp   action))
      (funcall action self (or (find-obj sender) sender)))))

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
      (invoke ptr "setAction:" :pointer (null-pointer)))
    (setf (slot-value obj 'action) nil)))

(flet ((set-target-with-function (obj function)
         (declare (type target-mixin obj)
                  (type (or function symbol) function))
         (with-ptr obj ptr
           (dispatch-main ()
             (invoke ptr "setAction:" :sel "cocaRespondActionInLisp:"))
           (setf (slot-value obj 'action) function))))
  (defmethod (setf action) ((function symbol) (obj target-mixin))
    (set-target-with-function obj function))
  (defmethod (setf action) ((function function) (obj target-mixin))
    (set-target-with-function obj function)))

;;;; target.lisp ends here
