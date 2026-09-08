;;;; window.lisp --- Wrapper of NSWindow

(in-package :coca.appkit)

(define-objc-global-variable window-list ()
  "A list of all the `window' instance. ")

(define-objc-class "CocaWindowDelegate" "NSObject"
  "NSWindowDelegate")

(define-objc-global-variable window-delegate
    (init (alloc "CocaWindowDelegate"))
  "The NSWindowDelegate for all the `window'. ")

;; lisp side should manage window close, reuse, and destroy.

(defclass window (obj
                  owned-mixin
                  find-obj-mixin
                  titled-mixin
                  framed-mixin
                  visible-mixin
                  subview-mixin)
  ((screen
    :initform (main-screen)
    :initarg  :screen
    :reader   screen
    ;; TODO: (parent window) should be the screen...
    ;; :reader   parent
    )
   (content-view-ptr
    :documentation
    "Foreign pointer to [NSWindow contentView]. "))
  (:default-initargs
   :objc-class     "NSWindow"
   :objc-init      'init-window
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
  "Create a NSWindow. "
  (declare (type foreign-pointer ptr))
  (invoke ptr
          "initWithContentRect:styleMask:backing:defer:"
          :ns-rect        #(0 0 100 100)
          :ns-window-style (:titled
                            :closable
                            :resizable
                            :miniaturizable)
          :unsigned-long   2
          :bool            t)
  (invoke ptr "setReleasedWhenClosed:" :bool nil)
  ptr)

(defmethod initialize-instance :after ((window window) &key)
  (with-ptr window ptr
    (dispatch-main ()
      (invoke ptr "setDelegate:" :object (window-delegate))
      (setf (slot-value window 'content-view-ptr)
            (invoke ptr "contentView" :object))))
  (pushnew window *window-list* :test #'eq))

(defmethod objc-ptr ((window window) (name (eql :delegate)))
  (window-delegate))

(defmethod set-frame
    ((window window) (x real) (y real) (w real) (h real))
  (with-ptr window ptr
    (dispatch-main ()
      (invoke ptr "setFrame:display:"
              :ns-rect (x y w h)
              :bool    nil)))
  window)

(defmethod destroy :after ((window window))
  (setf *window-list* (delete window *window-list* :test #'eq)))

(defmethod objc-ptr ((window window) (name (eql :container-view)))
  (slot-value window 'content-view-ptr))

(defgeneric window-style (window)
  (:documentation
   "Get/Set window style of WINDOW. ")
  (:method ((window window))
    (with-ptr window ptr
      (invoke ptr "styleMask" :ns-window-style))))

(defmethod (setf window-style) (style-mask (window window))
  (with-ptr window ptr
    (dispatch-main ()
      (invoke ptr "setStyleMask:" :ns-window-style style-mask)))
  style-mask)

;;;; window.lisp ends here
