;;;; color.lisp --- Wraps NSColor

(in-package :coca.appkit)

(define-objc-global-variable ns-color-space-sRGB
    (invoke "NSColorSpace" "sRGBColorSpace" :object)
  "[NSColorSpace sRGBColorSpace]")

(define-objc-global-variable ns-color-space-displayP3
    (invoke "NSColorSpace" "displayP3ColorSpace" :object)
  "[NSColorSpace displayP3ColorSpace]")

(defvar *colors* (tg:make-weak-hash-table :weakness :value))

(defstruct (color (:constructor make-ns-color))
  (ptr    (null-pointer) :type foreign-pointer)
  (cg-ptr (null-pointer) :type foreign-pointer)
  (red    0d0            :type (double-float 0d0 1d0) :read-only t)
  (green  0d0            :type (double-float 0d0 1d0) :read-only t)
  (blue   0d0            :type (double-float 0d0 1d0) :read-only t)
  (alpha  0d0            :type (double-float 0d0 1d0) :read-only t))

(defmethod print-object ((color color) stream)
  (print-unreadable-object (color stream :type t)
    (format stream "#x~2,'0X~2,'0X~2,'0X"
            (truncate (color-red   color) 1/255)
            (truncate (color-green color) 1/255)
            (truncate (color-blue  color) 1/255))))

(define-on-coca-app-finish-run renew-color-ptr-cg-ptr
  (let ((colors (alx:hash-table-values *colors*)))
    (clrhash *colors*)
    (dolist (color colors)
      (let* ((ptr (make-ns-color-ptr-srgb (color-red   color)
                                          (color-green color)
                                          (color-blue  color)
                                          (color-alpha color)))
             (cg-ptr (invoke ptr "CGColor" :pointer)))
        (setf (color-ptr    color) ptr
              (color-cg-ptr color) cg-ptr)
        (setf (gethash (pointer-address ptr) *colors*) color)))))

(defun ns-color-to-color (ns-color)
  "Convert NS-COLOR into `color' instance. "
  (alx:ensure-gethash
   (pointer-address ns-color)
   *colors*
   (let ((cg-ptr (invoke ns-color "CGColor" :pointer)))
     (with-foreign-objects ((red*   :double)
                            (green* :double)
                            (blue*  :double)
                            (alpha* :double))
       (invoke (invoke ns-color "colorUsingColorSpace:"
                       :object (ns-color-space-srgb)
                       :object)
               "getRed:green:blue:alpha:"
               :pointer red*
               :pointer green*
               :pointer blue*
               :pointer alpha*)
       (make-ns-color :ptr    ns-color
                      :cg-ptr cg-ptr
                      :red    (mem-ref red*   :double)
                      :green  (mem-ref green* :double)
                      :blue   (mem-ref blue*  :double)
                      :alpha  (mem-ref alpha* :double))))))

(define-objc-typing :ns-color
  :result (:pointer ns-color-to-color))

(defun make-ns-color-ptr-srgb (red green blue alpha)
  (declare (type (double-float 0d0 1d0) red green blue alpha))
  (invoke "NSColor"
          "colorWithSRGBRed:green:blue:alpha:"
          :double red
          :double green
          :double blue
          :double alpha
          :object))

;;;; color.lisp ends here
