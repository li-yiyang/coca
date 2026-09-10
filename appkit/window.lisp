;;;; window.lisp --- Wrapper of NSWindow

(in-package :coca.appkit)

;; TODO: maybe fix:
;; there should be a list of window for currently visible window,
;; visible window should be prevented from releasing;
;; the closed window, if not hold by lisp, might be released
;; (destoryed) when releasing.
(define-objc-global-variable %window-list ()
  "A list of weak pointer to all the `window' instance. ")

(defun window-list ()
  "Return a list of `window' instance. 

The `window' instance is ordered by window creation
order. "
  (let ((window-list ()))
    (dolist (win* *%window-list* window-list)
      (push (tg:weak-pointer-value win*) window-list))))

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
                  minmax-framed-mixin
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
  (push (tg:make-weak-pointer window) *%window-list*))

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

(flet ((set-min-size (window)
         (declare (type window window))
         (with-slots (min-width min-height) window
           (with-ptr window ptr
             (dispatch-main ()
               (invoke ptr "setMinSize:" :ns-size (min-width min-height)))))))
  (defmethod (setf min-width) :after (min-width (window window))
    (declare (ignore min-width))
    (set-min-size window))
  (defmethod (setf min-height) :after (min-height (window window))
    (declare (ignore min-height))
    (set-min-size window)))

(flet ((set-max-size (window)
         (declare (type window window))
         (with-slots (max-width max-height) window
           (with-ptr window ptr
             (dispatch-main ()
               (invoke ptr "setMaxSize:" :ns-size (max-width max-height)))))))
  (defmethod (setf max-width) :after (max-width (window window))
    (declare (ignore max-width))
    (set-max-size window))
  (defmethod (setf max-height) :after (max-height (window window))
    (declare (ignore max-height))
    (set-max-size window)))

(defmethod destroy :after ((window window))
  (setf *%window-list* (delete-if (alx:curry #'eq window)
                                  *%window-list*
                                  :key  #'tg:weak-pointer-value)))

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
