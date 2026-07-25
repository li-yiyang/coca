;;;; titled.lisp --- Mixin for classes support title and setTitle: method

(in-package :coca.appkit)

(defclass titled-mixin () ()
  (:documentation
   "Title mixin for classes support title and setTitle: method.

Use `title' to get/set the title of the object's title. "))

(defmethod initialize-instance :after ((titled titled-mixin)
                                       &key (title "" title?))
  (when title? (setf (title titled) title)))

(defgeneric title (titled)
  (:documentation "Get/Set the title of the TITLED. ")
  (:method ((titled titled-mixin))
    (invoke (obj-ptr titled) "title" :ns-string)))

(defmethod (setf title) ((title string) (titled titled-mixin))
  (with-ptr titled ptr
    (dispatch-main ()
      (invoke ptr "setTitle:" :ns-string title))
    title))

;;;; titled.lisp ends here
