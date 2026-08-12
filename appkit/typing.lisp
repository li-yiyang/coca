;;;; typing.lisp --- NSRect, NSSize, NSPoint

(in-package :coca.appkit)


;;; NSRect, NSPoint, NSSize

(defcstruct (%c-ns-rect :class c-ns-rect)
  (x :double)
  (y :double)
  (w :double)
  (h :double))

(defmethod translate-from-foreign (ptr (type c-ns-rect))
  (with-foreign-slots ((x y w h) ptr (:struct %c-ns-rect))
    (declare (type double-float x y w h))
    (values x y w h)))

(defmethod expand-from-foreign (ptr (type c-ns-rect))
  `(with-foreign-slots ((x y w h) ,ptr (:struct %c-ns-rect))
     (declare (type double-float x y w h))
     (values x y w h)))

(defcstruct (%c-ns-point :class c-ns-point)
  (x :double)
  (y :double))

(defmethod translate-from-foreign (ptr (type c-ns-point))
  (with-foreign-slots ((x y) ptr (:struct %c-ns-rect))
    (values x y)))

(defmethod expand-from-foreign (ptr (type c-ns-point))
  `(with-foreign-slots ((x y) ,ptr (:struct %c-ns-rect))
     (values x y)))

(defcstruct (%c-ns-size :class c-ns-size)
  (w :double)
  (h :double))

(defmethod translate-from-foreign (ptr (type c-ns-size))
  (with-foreign-slots ((w h) ptr (:struct %c-ns-size))
    (values w h)))

(defmethod expand-from-foreign (ptr (type c-ns-size))
  `(with-foreign-slots ((w h) ,ptr (:struct %c-ns-size))
     (values w h)))

(defun %expr-as-double (expr)
  (if (realp expr)
      (coerce expr 'double-float)
      `(coerce ,expr 'double-float)))

(define-objc-typing :ns-rect
  :result ((:struct %c-ns-rect))
  :arg    (((vector x y w h)
            `(:double ,(%expr-as-double x)
              :double ,(%expr-as-double y)
              :double ,(%expr-as-double w)
              :double ,(%expr-as-double h)))
           ((list x y w h)
            `(:double ,(%expr-as-double x)
              :double ,(%expr-as-double y)
              :double ,(%expr-as-double w)
              :double ,(%expr-as-double h)))))

(define-objc-typing :ns-point
  :result ((:struct %c-ns-point))
  :arg    (((vector x y)
            `(:double ,(%expr-as-double x)
              :double ,(%expr-as-double y)))
           ((list x y)
            `(:double ,(%expr-as-double x)
              :double ,(%expr-as-double y)))))

(define-objc-typing :ns-size
  :result ((:struct %c-ns-size))
  :arg    (((vector w h)
            `(:double ,(%expr-as-double w)
              :double ,(%expr-as-double h)))
           ((list w h)
            `(:double ,(%expr-as-double w)
              :double ,(%expr-as-double h)))))


;;;; NSWindow

(define-objc-mask :ns-window-style
  "Encode FLAGS as NSWindow style mask. "
  (:borderless                0)
  (:titled                    1)
  (:closable                  2)
  (:miniaturizable            4)
  (:resizable                 8)
  (:textured-background       256)
  (:unified-title-and-toolbar 4096)
  (:full-screen               16384)
  (:full-size-content-view    32768)
  (:utility-window            16)
  (:doc-modal-window          64)
  (:nonactivating-panel       128)
  (:hud-window                8192))


;;;; NSEventModifierMask

(define-objc-mask :ns-event-modifier
  "Flags that represent key states in an event object. "
  (:caps-lock          #x00010000)
  (:shift              #x00020000)
  (:control            #x00040000)
  (:option             #x00080000)
  (:command            #x00100000)
  (:numeric-pad        #x00200000)
  (:help               #x00400000)
  (:function           #x00800000)
  (:device-independent #xFFFF0000))

;;;; typing.lisp ends here
