;;;; visible.lisp --- visible-mixin

(in-package :coca.appkit)

(defclass visible-mixin () ()
  (:documentation
   "Mixin class for obj support isVisible and setVisible: method. "))

(defmethod initialize-instance :after ((obj visible-mixin)
                                       &key (visible nil visible?))
  (when visible?
    (setf (visible obj) visible)))

(defclass hidden-mixin () ()
  (:documentation
   "Mixin class for obj support isHidden and setHidden: method. "))

(defmethod initialize-instance :after ((obj hidden-mixin)
                                       &key (hidden nil hidden?))
  (when hidden?
    (setf (visible obj) (not hidden))))

(defgeneric visible (obj)
  (:documentation
   "Test if OBJ is visible or not. ")
  (:method ((obj visible-mixin))
    (invoke (obj-ptr obj) "isVisible" :bool))
  (:method ((obj hidden-mixin))
    (not (invoke (obj-ptr obj) "isHidden" :bool))))

(defmethod (setf visible) (visible (obj visible-mixin)
                           &aux (vis (and visible t)))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setIsVisible:" :bool vis))
    vis))

(defmethod (setf visible) (visible (obj hidden-mixin)
                           &aux (vis (and visible t)))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setIsVisible:" :bool (not vis)))
    vis))

;;;; visible.lisp ends here
