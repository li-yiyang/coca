;;;; screen.lisp --- Screen

(in-package :coca.appkit)

(defclass screen (obj
                  framed-mixin
                  static-framed-mixin
                  find-obj-mixin
                  named-mixin)
  ()
  (:documentation
   "A wrapper for NSScreen.

`screen' should not be initalized via `make-instance'.

Use `main-screen' to find the main screen.
Use `screen-list' to get a list of avaliable screen. "))

(defmethod print-object ((screen screen) stream)
  (print-unreadable-object (screen stream :type t)
    (multiple-value-bind (w h) (size screen)
      (format stream "~Dx~D " w h))
    (write-string (name screen) stream)))

(defmethod parent ((screen screen))
  (error "~A of ~A has no parent"
         (class-name (class-of screen))
         screen))

(defmethod visible-frame ((screen screen))
  (with-ptr screen ptr
    (invoke ptr "visibleFrame" :ns-rect)))

(defun wrap-screen-ptr (ptr)
  "Wrap NSScreen pointer PTR. "
  (ensure-find-obj ptr
    (make-instance
     'screen
     :name (invoke ptr "localizedName" :ns-string)
     :ptr  ptr)))

(defun screen-list ()
  "Return a list of `screen'. "
  (mapcar #'wrap-screen-ptr (invoke "NSScreen" "screens" :ns-array)))

(defun main-screen ()
  "Return the main `screen'. "
  (wrap-screen-ptr (invoke "NSScreen" "mainScreen" :object)))

;;;; screen.lisp ends here
