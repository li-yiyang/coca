;;;; window.lisp

(in-package :coca.cocoa)


;;;; Window Protocol

(declaim (type ns-point *window-default-position*))
(defvar *window-default-position* (ns-point :x 6 :y 44)
  "The default position of a newly opened window. ")

(declaim (type ns-size *window-default-size*))
(defvar *window-default-size* (ns-size :w 502 :h 150)
  "The default size of a newly opened window. ")

(declaim (type boolean *set-window-position-animated-p*))
(defvar *set-window-position-animated-p* t
  "If `*set-window-position-animated-p*' is `t',
the window moves with animation when `set-view-position' for `window';
otherwise, it moves immediately. ")

(defgeneric window-close (window)
  (:documentation
   "Closes the WINDOW.

Dev Note:
This is triggered when user clicks a window's close bottom,
or chooses Close from the File menu. "))

(defgeneric window-close-event-handler (window)
  (:documentation
   "Called from ObjC side whenever a window will be closed.

See ObjC method for `windowWillClose:'. "))

#+coca.todo
(defgeneric window-size-parts (window)
  )

(defgeneric window-title (window)
  (:documentation
   "Get/Set the WINDOW title as string. "))

(declaim (inline set-window-title))
(defun set-window-title (window new-title)
  "Sets the WINDOW title to NEW-TITLE.

Parameter:
+ WINDOW:
+ NEW-TITLE:

Dev Note:
+ see (setf window-title)"
  (declare (type window window))
  (setf (window-title window) new-title))

(declaim (inline set-window-style))
(defun set-window-style (window style)
  "Set the WINDOW style to STYLE.

Parameter:
+ WINDOW:
+ STYLE:
window-close-event-handler
Dev Note:
+ see (setf window-style)"
  (declare (type window window)
           (type (or keyword (cons keyword t)) style))
  (setf (window-style window) style))

(declaim (inline set-window-level))
(defun set-window-level (window level)
  "Set the WINDOW level to LEVEL.

Parameter:
+ WINDOW:
+ LEVEL:

Dev Note:
+ see (setf window-level)"
  (declare (type window window)
           (type (or (integer 0)
                     (member :normal :floating :submenu
                             :torn-off :main-menu
                             :status :modal-panel
                             :screen-saver))
                 level))
  (setf (window-level window) level))

(defgeneric window-show (window)
  (:documentation
   "Make WINDOW visible on the screen. "))

(defgeneric window-hide (window)
  (:documentation
   "Make WINDOW invisible on the screen. "))

(defgeneric window-shown-p (window)
  (:documentation
   "Return `t' if WINDOW is visible, and `nil' if it's hidden. "))

(defgeneric window-ensure-on-screen (window &optional default-position default-size))

(defgeneric window-grow-rect (window))

(defun windows (&key (class 'window) include-invisibles include-windoids)
  "Returns a list of existing windows that are instances of CLASS.
The list is ordered from front to back. "
  (declare (ignore include-windoids))
  (let ((windows (invoke (app) "orderedWindows" :ns-array)))
    (remove-if-not (lambda (w)
                     (and (typep w class)
                          (if include-invisibles
                              t
                              (window-shown-p w))
                          #+coca.todo
                          (if include-windoids
                              t
                              (not (typep w 'windoid)))))
                   (mapcar #'find-objc-obj windows))))

(defun front-window (&key (class 'window) include-invisibles include-windoids)
  "Returns the frontmost satisfying the arguments.
If no windows satisfy the tests, `nil' is returned. "
  (first (windows :class class
                  :include-invisibles include-invisibles
                  :include-windoids include-windoids)))

(declaim (inline target))
(defun target ()
  "Returns the second window on the list of windows;
it is equivalent to (second (windows)). "
  (second (windows)))

(defun map-windows (function &key class include-invisibles include-windoids)
  "Calls FUNCTION, a function of one argument, on each window that
statisfies the arguments. "
  (map nil function (windows :class class
                             :include-invisibles include-invisibles
                             :include-windoids include-windoids)))


;;;; CocaWindow, CocaWindowDelegate

(define-objc-class "CocaWindow" "NSWindow"
  "NSWindowController" "NSWindowDelegate")
(define-objc-class "CocaWindowView" "NSView")

(defun ns-notification-window (notification)
  (declare (type foreign-pointer notification))
  (find-objc-obj (invoke notification "object" :object)))


;;;; window

(defclass window (view)
  ((ptr
    :documentation
    "PTR of `window' is window's container view. ")
   (objc-class
    :type       objc-class
    :initform   (coerce-to-objc-class "CocaWindowView"))
   (window-class
    :type       objc-class
    :reader     window-class
    :allocation :class
    :initform   (coerce-to-objc-class "CocaWindow")
    :documentation
    "ObjC class for window. ")
   (window-cursor
    :allocation :class
    ;; :initform   *arrow-cursor*
    :reader     window-cursor)
   (window-title
    :initform "Untitled"
    :initarg  :window-title
    :type     string
    :accessor window-title
    :documentation
    "A string specifying the title of the window. ")
   (window-style
    :initform '(:titled :closable :resizable :miniaturizable)
    :reader   window-style
    :documentation
    "A list or keyword for NSWindowStyleMask.

Possible values are:
+ :borderless
+ :titled
+ :closable
+ :miniaturizable
+ :resizable
+ :textured-background
+ :unified-title-and-toolbar
+ :full-screen
+ :full-size-content-view
+ :utility-window
+ :doc-modal-window
+ :nonactivating-panel
+ :hud-window
"))
  (:documentation
   "The class `window' is a wrapper of NSWindow. "))

(define-objc-mask as-ns-window-style
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

(define-objc-mask as-ns-autoresizing
  "Encode FLAGS as NSView autoresizing mask. "
  (:not-sizable  0)
  (:min-x-margin 1)
  (:width        2)
  (:max-x-margin 4)
  (:min-y-margin 8)
  (:height       16)
  (:max-y-margin 32))

;; initialize-instance

(defmethod alloc-init ((window window))
  (let* ((style (as-ns-window-style (window-style window)))
         (win*  (alloc (window-class window)))
         (view  (init (alloc (objc-class window)))))
    (dispatch-main ()
      (invoke win*
              "initWithContentRect:styleMask:backing:defer:"
              :ns-rect       (ns-rect :origin (view-position window)
                                      :size   (view-size     window))
              :unsigned-long style
              :unsigned-long 2          ; buffered
              :bool          t
              :object)

      (invoke win* "setAcceptsMouseMovedEvents:" :bool t)
      (invoke win* "setReleasedWhenClosed:"      :bool t)
      (invoke win* "setDelegate:" :object win*)

      ;; replace contentView with `coca-view'
      (invoke view "setAutoresizingMask:"
              :unsigned-long (as-ns-autoresizing '(:width :height)))
      (invoke win* "setContentView:" :object view))

    (setf (slot-value window 'wptr) win*)

    ;; set to PTR
    view))

(defmethod initialize-instance :after ((window window)
                                       &key
                                         (window-level :normal wlp)
                                         (window-show  t))
  "Called when (make-instance 'window ...).

Parameters:
+ WINDOW-TITLE
+ WINDOW-LEVEL: -> NSWindow level

  Possible Values:
  + :normal
  + :floating (top)
  + :submenu
  + :torn-off
  + :main-menu
  + :status
  + :modal-panel
  + :screen-saver
+ WINDOW-STYLE
"
  (when wlp (set-window-level window window-level))
  (set-window-title window (slot-value window 'window-title))
  (when window-show (window-show window))
  (with-wptr window
    (regist-objc-obj window wptr)
    (tg:finalize window
                 (lambda ()
                   (alx:when-let ((window (find-objc-obj wptr)))
                     (dealloc window))))))

(defmethod dealloc ((window window))
  (with-wptr window
    (remhash (pointer-address wptr) *objc-objects*)
    (setf (slot-value window 'wptr) nil))
  ;; remove PTR and unbound it, see `dealloc' for `view'
  (call-next-method))

;; position, size

(defmethod (setf view-size) ((size ns-size) (window window))
  (with-wptr window
    (dispatch-main () (invoke wptr "setContentSize:" :ns-size size))
    (setf (slot-value window 'view-size) size)))

(defmethod (setf view-position) ((pos ns-point) (window window))
  (with-wptr window
    (let ((frame (invoke wptr
                         "frameRectForContentRect:"
                         :ns-rect (ns-rect :size   (view-size window)
                                           :origin pos)
                         :ns-rect)))
      (dispatch-main ()
        (invoke wptr
                "setFrame:display:animate:"
                :ns-rect frame
                :bool    t
                :bool    *set-window-position-animated-p*))
      (setf (slot-value window 'view-position) pos))))

(defmethod view-default-position ((window window))
  *window-default-position*)

(defmethod view-default-size ((window window))
  *window-default-size*)

(define-objc-method ("CocaWindow" "windowDidMove:")
                    :void ((notification :object))
  (alx:when-let ((window (ns-notification-window notification)))
    (let* ((content (invoke (objc-ptr window) "bounds" :ns-rect))
           (in-win  (invoke (objc-ptr window)
                            "convertRect:toView:"
                            :ns-rect content
                            :pointer (null-pointer)
                            :ns-rect))
           (frame   (invoke (wptr window)
                            "convertRectToScreen:"
                            :ns-rect in-win
                            :ns-rect)))
      (setf (slot-value window 'view-size)     (ns-rect-size   frame)
            (slot-value window 'view-position) (ns-rect-origin frame)))))

;; view-nickname

(defmethod (setf view-nickname) :after (nickname (window window))
  "After setting nickname, update WINDOW's title. "
  (let ((title (princ-to-string nickname)) ; pretty print?
        (wptr  (wptr window)))
    (dispatch-main () (invoke wptr "setTitle:" :ns-string title))))

;; view-container

(defmethod (setf view-container) (container (window window))
  (unless (null container)
    (error "Container should always be `nil' for windows. ")))

;; window-show, window-hide

(defmethod window-show ((window window))
  (with-wptr window
    (dispatch-main ()
      (invoke wptr "makeKeyAndOrderFront:" :object wptr))))

(defmethod window-hide ((window window))
  (with-wptr window
    (dispatch-main ()
      (invoke wptr "orderOut:" :object wptr))))

(defmethod window-shown-p ((window window))
  (with-wptr window
    (invoke wptr "isVisible" :bool)))

(defmethod (setf window-shown-p) (shownp (window window))
  (if shownp
      (window-show window)
      (window-hide window)))

;; window-title

(defmethod (setf window-title) :after ((title string) (window window))
  (with-wptr window
    (dispatch-main ()
      (invoke wptr "setTitle:" :ns-string title))))

;; window-style

(defmethod (setf window-style) (style (window window))
  (let ((style (as-ns-window-style style)))
    (with-wptr window
      (dispatch-main ()
        (invoke wptr "setStyleMask:" :unsigned-long style)))
    (setf (slot-value window 'window-style) (alx:ensure-list style))))

(defmethod window-level ((window window))
  (let ((level (invoke (wptr window) "level" :long)))
    (case level
      (0    :normal)
      (3    :floating)
      (10   :submenu)
      (12   :torn-off)
      (24   :main-menu)
      (25   :status)
      (1000 :modal-panel)
      (2000 :screen-saver)
      (otherwise level))))

(defmethod (setf window-level) (level (window window))
  (let ((level (case level
                 (:normal 0)
                 (:floating 3)
                 (:submenu 10)
                 (:torn-off 12)
                 (:main-menu 24)
                 (:status 25)
                 (:modal-panel 1000)
                 (:screen-saver 2000)
                 (otherwise (the integer level)))))
    (dispatch-main ()
      (invoke (wptr window) "setLevel:" :long level))))

;; window-close

(define-objc-method ("CocaWindow" "windowWillClose:")
                    :void ((notification :object))
  (alx:when-let ((window (ns-notification-window notification)))
    (window-close window)))

(define-objc-method ("CocaWindow" "dealloc")
                    :void ((ns-window :object))
  (alx:when-let ((window (find-objc-obj ns-window)))
    (dealloc window)))

(defmethod window-close ((window window))
  (dispatch-main ()
    (invoke (wptr window) "close:" :object (wptr window)))
  (dealloc window))

;;;; window.lisp ends here
