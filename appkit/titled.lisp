;;;; titled.lisp --- Mixin for classes support title and setTitle: method

(in-package :coca.appkit)

(defclass titled-mixin () ()
  (:documentation
   "Title mixin for classes support title and setTitle: method.

Use `title' to get/set the title of the object's title.

Initialize Parameter:
+ TITLE: set the title of obj
"))

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

(defclass alternate-titled-mixin () ()
  (:documentation
   "Alternate title mixin for classes support alternateTitle method.

Use `alternate-title' to get/set the title of the object's alternate title.

Initialize Parameter:
+ ALTERNATE-TITLE: set the alternate title of obj
"))

(defmethod initialize-instance :after ((titled alternate-titled-mixin)
                                       &key (alternate-title "" alternate-title?))
  (when alternate-title? (setf (alternate-title titled) alternate-title)))

(defgeneric alternate-title (alternate-titled)
  (:documentation "Get/Set the alternate title of the ALTERNATE-TITLED. ")
  (:method ((titled alternate-titled-mixin))
    (invoke (obj-ptr titled) "alternateTitle" :ns-string)))

(defmethod (setf alternate-title) ((title string) (titled alternate-titled-mixin))
  (with-ptr titled ptr
    (dispatch-main ()
      (invoke ptr "setAlternateTitle:" :ns-string title))
    title))

;;;; titled.lisp ends here
