;;;; screen.lisp

(in-package :coca.cocoa)

(defclass screen (objc-obj)
  ((ptr
    :documentation
    "The PTR is point to NSScreen. ")
   (dpi
    :type ns-size
    :documentation
    "DPI of screen. ")
   (backing-scale
    :reader backing-scale
    :documentation
    "Backing store scaling factor. ")
   (screen-size
    :type ns-size
    :reader screen-size
    :documentation
    "Size of NSScreen. ")
   (screen-origin
    :type   ns-point
    :reader screen-origin
    :documentation
    "Origin (lower-left) position of NSScreen. "))
  (:documentation
   "Wraps NSScreen. "))

(defmethod print-object ((screen screen) stream)
  (print-unreadable-object (screen stream :type t)
    (format stream
            "~Dx~D #x~X"
            (ns-size-w (screen-size screen))
            (ns-size-h (screen-size screen))
            (pointer-address (objc-ptr screen)))))

(defun %screen-update-infomation (screen)
  "Update SCREEN infomation and return SCREEN itself.

Dev Note:
+ The updated infomation are:
  + dpi-x, dpi-y
  + backing-scale
  + frame size
"
  (declare (type screen screen))
  (with-ptr screen
    (let* ((desc (invoke ptr "deviceDescription" :object))
           (dpi  (invoke desc "objectForKey:"
                         :object (objc-symbol-value "NSDeviceResolution" :object)
                         :object)))
      (setf (slot-value screen 'dpi) (invoke dpi "sizeValue" :ns-size)))

    (let ((scale (invoke ptr "backingScaleFactor" :double)))
      (setf (slot-value screen 'backing-scale) scale))

    (let ((frame (invoke ptr "frame" :ns-rect)))
      (setf (slot-value screen 'screen-size)   (ns-rect-size   frame)
            (slot-value screen 'screen-origin) (ns-rect-origin frame))))
  screen)

(defmethod initialize-instance :after ((screen screen) &key)
  (%screen-update-infomation screen)
  (regist-objc-obj screen))

(defun coerce-to-screen (ptr)
  "Convert PTR to `screen'.

Note that this is DANGEROUS if you don't know if PTR
is a ObjC pointer to NSScreen. "
  (declare (type foreign-pointer ptr))
  (let ((screen (find-objc-obj ptr)))
    (if screen
        (%screen-update-infomation (the screen screen))
        (make-instance 'screen :pointer ptr))))

(define-objc-global-variable main-screen
    (coerce-to-screen (invoke "NSScreen" "mainScreen" :object))
  "Return main screen as `screen'. ")

;;;; screen.lisp ends here
