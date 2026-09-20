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

(defun ns-notification-window (notification)
  (declare (type foreign-pointer notification))
  (let* ((object (invoke notification "object" :object))
         (window (find-obj object)))
    (when (typep window 'window)
      window)))


;; lisp side should manage window close, reuse, and destroy.

(defclass window (obj
                  owned-mixin
                  find-obj-mixin
                  titled-mixin
                  framed-mixin
                  minmax-framed-mixin
                  visible-mixin
                  subview-mixin
                  main-menu-mixin)
  ((screen
    :initform (main-screen)
    :initarg  :screen
    :reader   screen)
   (content-view-ptr
    :type (or null foreign-pointer)
    :documentation
    "Foreign pointer to [NSWindow contentView]. "))
  (:default-initargs
   :objc-class     "NSWindow"
   :objc-init      'init-window
   :init-in-main-p t
   :visible        t)
  (:documentation
   "Wrapper of NSWindow

Initialize Parameters:
+ SCREEN: specify which `screen' the window should be displayed on
  by default, the `screen' would be the `main-screen'

+ TITLE: window title (default \"\")
  see `titled-mixin'

+ X, Y, LOCATION, WIDTH, HEIGHT, SIZE, ORIGIN, FRAME,
  MIN-WIDTH, MAX-WIDTH, MIN-HEIGHT, MAX-HEIGHT
  see `framed-mixin' and `minmax-framed-mixin'

+ VISIBLE: visible or not (default `t')
  see `visible-mixin'

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

(defgeneric screen (obj)
  (:documentation "Return the `screen' OBJ is on. "))

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

(defmethod parent-frame ((window window))
  (frame (screen window)))

(defmethod parent-size ((window window))
  (size (screen window)))

(defmethod parent-width ((window window))
  (width (screen window)))

(defmethod parent-height ((window window))
  (height (screen window)))

(defmethod set-min-size ((window window) (w real) (h real))
  (declare (type framed-size w h))
  (with-slots (min-width min-height) window
    (with-ptr window ptr
      (dispatch-main ()
        (invoke ptr "setMinSize:" :ns-size (w h))))
    (setf min-width  w
          min-height h)))

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

(defmethod set-max-size ((window window) (w real) (h real))
  (declare (type framed-size w h))
  (with-slots (max-width max-height) window
    (with-ptr window ptr
      (dispatch-main ()
        (invoke ptr "setMaxSize:" :ns-size (w h))))
    (setf max-width  w
          max-height h)))

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

(defgeneric window-close (window)
  (:documentation
   "Close the WINDOW.
Return the WINDOW itself.

Dev Note:
+ the closed WINDOW is not destroyed,
  so you can use `window-select', `show' methods to
  make it reappear
")
  (:method ((window window))
    (with-ptr window ptr
      (dispatch-main ()
        (invoke ptr "close")))))

(defgeneric window-close-event-handler (window)
  (:documentation
   "Called when the WINDOW will be closed. ")
  (:method ((window window))))

(define-objc-method ("CocaWindowDelegate" "windowWillClose:")
                    :void ((notification :object))
  (alx:when-let ((window (ns-notification-window notification)))
    (window-close-event-handler window)))

(defgeneric window-should-close (window)
  (:documentation
   "Called when the WINDOW needs to be closed,
typically when a user clicks the close button of the WINDOW.

If the return value is non-nil, the window would be closed;
otherwise if the return value is `nil', the window would
not be closed. ")
  (:method ((window window)) t))

(define-objc-method ("CocaWindowDelegate" "windowShouldClose:")
                    :bool ((ns-window :object))
  (let ((window (find-obj ns-window)))
    (if window
        (and (window-should-close window) t)
        t)))

(defmethod destroy ((window window))
  (with-ptr window ptr
    (dispatch-main ()
      (invoke ptr "close")
      (call-next-method)))

  ;; clean up window object
  (setf (slot-value window 'content-view-ptr) nil)
  ;; clean up window-list
  (setf *%window-list*
        (delete-if (alx:curry #'eq window)
                   *%window-list*
                   :key  #'tg:weak-pointer-value)))

(defmethod objc-ptr ((window window) (name (eql :container-view)))
  (slot-value window 'content-view-ptr))

(defgeneric window-style (window)
  (:documentation
   "Get/Set window style of WINDOW.

Possible Values:
the `window-style' could be keyword or a list of keyword,
each could be:

+ `:borderless'
+ `:titled'
+ `:closable'
+ `:miniaturizable'
+ `:resizable'
+ `:textured-background'
+ `:unified-title-and-toolbar'
+ `:full-screen'
+ `:full-size-content-view'")
  (:method ((window window))
    (with-ptr window ptr
      (invoke ptr "styleMask" :ns-window-style))))

(defmethod (setf window-style) (style-mask (window window))
  (with-ptr window ptr
    (let ((style (as-ns-window-style style-mask)))
      (dispatch-main ()
        (invoke ptr "setStyleMask:" :unsigned-long style))))
  style-mask)


;;;; Events

(defgeneric window-select (window)
  (:documentation
   "Brings WINDOW to the front, activates it,
and shows it if it is hidden.
Return the WINDOW itself.

Note: The previously active window is deactivated.

Parameter:
+ WINDOW: a `window'")
  (:method ((window window))
    (with-ptr window ptr
      (dispatch-main ()
        (invoke ptr "makeKeyAndOrderFront:" :pointer (null-pointer))))
    window))

(defgeneric window-select-event-handler (window)
  (:documentation
   "The generic function `window-select-event-handler' is called
whenever the user clicks an inactive WINDOW.

The `window-select-event-handler' function maybe specialized,
for example, to make a window unselectable.

Parameter:
+ WINDOW: a `window'.
")
  (:method ((window window)))
  (:method :before ((obj main-menu-mixin))
    (set-main-menu (slot-value obj 'menu))))

(define-objc-method ("CocaWindowDelegate" "windowDidBecomeKey:")
                    :void ((notification :object))
  (alx:when-let ((window (ns-notification-window notification)))
    (window-select-event-handler window)))

;;;; window.lisp ends here
