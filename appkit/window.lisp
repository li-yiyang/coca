;;;; window.lisp --- Wrapper of NSWindow

(in-package :coca.appkit)

(define-objc-class "CocaWindowDelegate" "NSObject"
  "NSWindowDelegate")

(define-objc-global-variable window-delegate
    (init (alloc "CocaWindowDelegate"))
  "The NSWindowDelegate for all the `window'. ")

(defclass window (obj
                  owned-mixin
                  find-obj-mixin
                  titled-mixin
                  framed-mixin
                  visible-mixin)
  ((screen
    :initform (main-screen)
    :initarg  :screen
    :reader   screen
    :reader   parent))
  (:default-initargs
   :objc-class     "NSWindow"
   :init-function  'init-window
   :init-in-main-p t
   :visible        t
   :frame          #(0 0 100 100))
  (:documentation
   "Wrapper of NSWindow

Dev Note:
+ objc-ptr mapping:
  + :ptr      -> NSWindow
  + :frame    -> :ptr
  + :delegate -> (window-delegate)
"))

(defun init-window (ptr)
  "Create a NSWindow"
  (invoke ptr
          "initWithContentRect:styleMask:backing:defer:"
          :ns-rect        #(0 0 100 100)
          :ns-window-style (:titled
                            :closable
                            :resizable
                            :miniaturizable)
          :unsigned-long   2
          :bool            t
          :object))

(defmethod objc-ptr ((window window) (name (eql :delegate)))
  (window-delegate))

(defmethod set-frame
    ((window window) (x real) (y real) (w real) (h real))
  (with-ptr window ptr
    (dispatch-main ()
      (invoke ptr "setFrame:display:"
              :ns-rect (x y w h)
              :bool    nil))))

;;;; window.lisp ends here
