;;;; screen.lisp --- Screen

(in-package :coca.appkit)

(defclass screen (obj
                  framed-mixin
                  static-framed-mixin
                  find-obj-mixin
                  named-mixin)
  ((dpi-x
    :initarg :dpi-x
    :reader  screen-dpi-x)
   (dpi-y
    :initarg :dpi-y
    :reader  screen-dpi-y)
   (backing-scale
    :initarg :backing-scale
    :reader  screen-backing-scale))
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
    (let* ((des (invoke ptr "deviceDescription" :object))
           (dpi (invoke des "objectForKey:"
                        :object (objc-symbol-value
                                 "NSDeviceResolution"
                                 :object)
                        :object))
           (scale (invoke ptr "backingScaleFactor" :double)))
      (multiple-value-bind (dpi-x dpi-y)
          (invoke dpi "sizeValue" :ns-size)
        (make-instance
         'screen
         :name  (invoke ptr "localizedName" :ns-string)
         :ptr   ptr
         :dpi-x dpi-x
         :dpi-y dpi-y
         :backing-scale scale)))))

(defun screen-list ()
  "Return a list of `screen'. "
  (dispatch-main ()
    (mapcar #'wrap-screen-ptr
            (invoke "NSScreen" "screens" :ns-array))))

(defun main-screen ()
  "Return the main `screen'. "
  (dispatch-main ()
    (wrap-screen-ptr (invoke "NSScreen" "mainScreen" :object))))

(defgeneric screen-dpi (screen)
  (:documentation
   "Return values are screen DPI x and DPI y. ")
  (:method ((screen screen))
    (values (screen-dpi-x screen)
            (screen-dpi-y screen))))

;;;; screen.lisp ends here
