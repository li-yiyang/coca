;;;; visible.lisp --- visible-mixin

(in-package :coca.appkit)

(defclass visible-mixin () ()
  (:documentation
   "Mixin class for obj support isVisible and setVisible: method"))

(defmethod initialize-instance :after ((obj visible-mixin)
                                       &key (visible nil visible?))
  (when visible?
    (setf (visible obj) visible)))

(defgeneric visible (obj)
  (:documentation
   "Test if OBJ is visible or not. ")
  (:method ((obj visible-mixin))
    (invoke (obj-ptr obj) "isVisible" :bool)))

(defmethod (setf visible) (visible (obj visible-mixin)
                           &aux (vis (and visible t)))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setIsVisible:" :bool vis))
    (and visible t)))

;;;; visible.lisp ends here
