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

(defgeneric window-select (window)
  (:documentation
   "Brings a window to the front, activates it, and shows it
if it is hidden. The previously active window is deactivated."))

(defgeneric window-shown-p (window)
  (:documentation
   "Return `t' if WINDOW is visible, and `nil' if it's hidden. "))

(defgeneric window-update-cursor (window point)
  (:documentation
   "The generic function `window-update-cursor' is called by
`update-cursor' whenever the cursor is over the window.

When the mouse is over the front window or any floating window, the
`window-update-cursor' method for the `window' class sets the variable
`*mouse-view*' to the view containing the mouse, using
`find-clicked-subview'.  The `window-null-event-handler' method for
the `window' class calls `update-cursor', which calls
`*cursorhook*'. The function that is the initial value of
`*cursorhook*' calls `window-update-cursor', which sets the cursor
using the value returned by `view-cursor'.

The method for `window' simply sets the cursor to the result of
calling the generic function `view-cursor' on the clicked subview of
`window' if there is one; otherwise it sets the cursor to the result
of calling `window-cursor' on the window.

The `null' method sets the cursor to the value of `*arrow-cursor*'.

The `window-update-cursor' function should be shadowed if the cursor
must change according to what part of the window it is over."))

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
   (window-screen
    :type       screen
    :initarg    :window-screen
    :initform   (main-screen)
    :reader     window-screen
    :documentation
    "The window is considered as a view contained in screen. ")
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
  (let* ((style  (as-ns-window-style (window-style window)))
         (screen (window-screen window))
         (origin (view-origin window))
         (win*   (alloc (window-class window)))
         (view   (init (alloc (objc-class window)))))
    (flip-ns-point! window (view-position window) origin (screen-size screen))
    (offset-ns-point! (screen-origin screen) origin)
    (dispatch-main ()
      (invoke win*
              "initWithContentRect:styleMask:backing:defer:"
              :ns-rect       (ns-rect :origin origin
                                      :size   (view-size window))
              :unsigned-long style
              :unsigned-long 2          ; buffered
              :bool          t
              :object)

      ;; ensure `window-update-cursor'
      (invoke win* "setAcceptsMouseMovedEvents:" :bool t)
      (invoke win* "setReleasedWhenClosed:"      :bool t)
      (invoke win* "setDelegate:" :object win*)

      ;; replace contentView with `coca-view'
      (invoke view "setAutoresizingMask:"
              :unsigned-long (as-ns-autoresizing '(:width :height)))
      (invoke win* "setContentView:" :object view)
      ;; balance alloc
      (autorelease view))

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
    (regist-objc-obj window wptr)))

(defmethod dealloc ((window window))
  (with-wptr window
    (remhash (pointer-address wptr) *objc-objects*)
    (dispatch-main () (autorelease wptr))
    (setf (slot-value window 'wptr) nil)))

;; view-position, view-size

(defun offset-ns-point! (p1 p2)
  "Offset P2 by P1.
Return modified P2.

Side Effect:
+ P2 is modified. "
  (incf (ns-point-x p2) (ns-point-x p1))
  (incf (ns-point-y p2) (ns-point-y p1))
  p2)

(defun %set-window-view-origin-and-size (window origin size)
  (declare (type window window)
           (type ns-point origin)
           (type ns-size size))
  (with-wptr window
    (let* ((content (ns-rect :origin origin :size size))
           (frame   (invoke wptr
                            "frameRectForContentRect:"
                            :ns-rect content
                            :ns-rect)))
      (dispatch-main ()
        (invoke wptr
                "setFrame:display:animate:"
                :ns-rect frame
                :bool    t
                :bool    *set-window-position-animated-p*)))))

(defmethod (setf view-size) ((size ns-size) (window window))
  (prog1 (copy-ns-size! size (view-size window))
    (let ((origin (view-origin window))
          (screen (window-screen window)))
      (flip-ns-point! window
                      (view-position window)
                      (view-origin   window)
                      (screen-size   screen))
      (offset-ns-point! (screen-origin screen) origin)
      (%set-window-view-origin-and-size window
                                        origin
                                        (view-size window)))))

(defmethod (setf view-position) ((pos ns-point) (window window))
  (prog1 (copy-ns-point! pos (view-position window))
    (let ((origin (view-origin window))
          (screen (window-screen window)))
      (flip-ns-point! window pos origin (screen-size screen))
      (offset-ns-point! (screen-origin screen) origin)
      (%set-window-view-origin-and-size window
                                        origin
                                        (view-size window)))))

(defmethod view-default-position ((window window))
  (copy-ns-point *window-default-position*))

(defmethod view-default-size ((window window))
  (copy-ns-size *window-default-size*))

(defun %window-move-or-resize (window)
  (declare (type window window))
  (with-ptr window
    (with-wptr window
      (let* ((content (invoke ptr "bounds" :ns-rect))
             (in-win  (invoke ptr
                              "convertRect:toView:"
                              :ns-rect content
                              :pointer (null-pointer)
                              :ns-rect))
             (frame   (invoke wptr
                              "convertRectToScreen:"
                              :ns-rect in-win
                              :ns-rect)))
        (copy-ns-size!  (ns-rect-size   frame) (view-size   window))
        (copy-ns-point! (ns-rect-origin frame) (view-origin window))
        (flip-ns-point! window
                        (view-origin   window)
                        (view-position window)
                        (screen-size   (window-screen window)))))))

(define-objc-method ("CocaWindow" "windowDidMove:")
                    :void ((notification :object))
  (alx:when-let ((window (ns-notification-window notification)))
    (%window-move-or-resize window)))

(define-objc-method ("CocaWindow" "windowDidResize:")
                    :void ((notification :object))
  (alx:when-let ((window (ns-notification-window notification)))
    (%window-move-or-resize window)
    ;; fix subviews view-position after setting window
    (loop :for subview :across (view-subviews window)
          :do (setf (view-position subview) (view-position subview)))))

;; view-title

(defmethod (setf window-title) :after ((title string) (window window))
  (with-wptr window
    (dispatch-main () (invoke wptr "setTitle:" :ns-string title))))

;; view-container

(defmethod (setf view-container) (container (window window))
  (unless (null container)
    (error "Container should always be `nil' for windows. ")))

;; window-show, window-hide, window-select

(defmethod window-show ((window window))
  (with-wptr window
    (dispatch-main ()
      (invoke wptr "setIsVisible:" :bool t))))

(defmethod window-hide ((window window))
  (with-wptr window
    (dispatch-main ()
      (invoke wptr "orderOut:" :object wptr))))

(defmethod window-select ((window window))
  (with-wptr window
    (dispatch-main ()
      (invoke wptr "makeKeyAndOrderFront:" :object wptr))))

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

(deftype window-level ()
  '(or
    (integer 0 2000)
    (member :normal :floating :submenu :torn-off
            :main-menu :status :modal-panel :screen-saver)))

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
  (declare (type window-level level))
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
      (invoke (wptr window) "setLevel:" :long level)))
  level)

;; window-close

(define-objc-method ("CocaWindow" "windowWillClose:")
                    :void ((notification :object))
  (alx:when-let ((window (ns-notification-window notification)))
    (window-close window)))

(define-objc-method ("CocaWindow" "dealloc")
                    :void ((ns-window :object))
  (alx:when-let ((window (find-objc-obj ns-window)))
    (dealloc window)
    (invoke-super self "dealloc")))

(defmethod window-close ((window window))
  (with-wptr window
    (dealloc window)
    (dispatch-main ()
      (invoke wptr "close"))))

;; window-update-cursor

(defun ns-event-pointer-location-in-window (event)
  (declare (type foreign-pointer event))
  (invoke event "locationInWindow" :ns-point))

(defmethod window-update-cursor ((window window) (point ns-point))
  (set-cursor (view-cursor window point)))

(define-objc-method ("CocaWindow" "mouseMoved:")
                    :void ((event :object))
  (alx:when-let ((window (find-objc-obj self)))
    (window-update-cursor window (ns-event-pointer-location-in-window event))))

;;;; window.lisp ends here
