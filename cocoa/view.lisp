;;;; view.lisp

(in-package :coca.cocoa)


;;;; View Protocol

(defgeneric focus-view (view &optional font-view)
  (:documentation
   "Select current drawing context to VIEW. (Focus on VIEW)

Parameters:
+ VIEW: a view installed in a window, or `nil'
  + if `nil', the current
+ FONT-VIEW: A view of `nil'
  + if `nil', the font is unchanged
  + if non-`nil', the `view-font' of `font-view' are installed
    after the rest of the focusing is completed.

Dev Note:
The `focus-view' function is not normally called directly.
In general, `with-focused-view' should be used when drawing
to views. "))

(defmacro with-focused-view (view &body body &environment env)
  "Executes BODY with the current CGContext set for drawing into VIEW.
This involves setting the current CGContext and setting the origin
and clip region so that drawing occurs in VIEW. When the BODY exit
(normally or abnormally), the old view is restored. "
  (let ((sym (if (and view (symbolp view)
                      (eq view (macroexpand view env)))
                 view
                 (gensym)))
        (fn  (gensym)))
    `(flet ((,fn (,sym)
              (declare (ignorable ,sym))
              ,@body))
       (declare (dynamic-extent ,fn))
       (call-with-focused-view ,view ,fn))))

(declaim (inline set-view-size))
(defun set-view-size (view w &optional h)
  "Set the size of VIEW.
Return `ns-size' of updated size.

Syntax:

    (set-view-size VIEW W H)     ;; => (setf (view-size VIEW) (ns-size :w W :h H))
    (set-view-size VIEW NS-SIZE) ;; => (setf (view-size VIEW) NS-SIZE)

Dev Note:
+ define method for (setf view-size)
"
  (if h
      (setf (view-size view) (ns-size :w w :h h))
      (setf (view-size view) w)))

(declaim (inline set-view-position))
(defun set-view-position (view x &optional y)
  "Set the position of VIEW.
Return `ns-point' of updated size.

Syntax:

    (set-view-position VIEW W H)     ;; => (setf (view-position VIEW) (ns-point :w W :h H))
    (set-view-position VIEW NS-SIZE) ;; => (setf (view-position VIEW) NS-SIZE)

Dev Note:
+ implement method for (setf view-position)
"
  (if y
      (setf (view-position view) (ns-point :x x :y y))
      (setf (view-position view) x)))

(defgeneric view-container (view)
  (:documentation
   "Returns the VIEW's containing view.

Parameter:
+ VIEW: a view or subview, but not a window.
  Instances of `window' cannot have containers. "))

(defmacro set-view-container (view new-container)
  "Sets VIEW's containing view to NEW-CONTAINER.
If VIEW's window is changed by giving it a new container,
`remove-view-from-window' is called on VIEW and the the
old window, and `install-view-in-window' is called on
VIEW and the new window.

Parameters:
+ VIEW: a view or subview, but not window.
  Instances of `window' cannot have containers.
  If `set-view-container' is called on a window,
  it signals an error.

+ NEW-CONTAINER: the new container of the view

Dev Note:
+ implement method for (setf view-container)
"
  `(setf (view-container ,view) ,new-container))

(defgeneric install-view-in-window (view window)
  (:documentation
   ""))

(defgeneric remove-view-from-window (view)
  (:documentation
   ""))

(defgeneric subviews (view &optional subview-type)
  (:documentation
   ""))

(defgeneric view-subviews (view)
  (:documentation
   ""))

(defgeneric map-subviews (view function &optional subview-type)
  (:documentation
   ""))

(defgeneric view-named (name view)
  (:documentation
   "Return the first subview of VIEW whose nickname is NAME.
The subviews are searched in the order in which they were
added to VIEW. "))


;;;; CocaView

(define-objc-class "CocaView" "NSView")


;;;; simple-view

(defclass simple-view (tgs:fundamental-output-stream
                       objc-obj)
  ((ptr
    :documentation
    "The foreign-pointer to NSView. ")
   (objc-class
    :initform (objc-class "CocaView")
    :documentation
    "The ObjC class of NSView. ")
   (wptr
    :initarg  :wptr
    :initform nil
    :type     (or null foreign-pointer)
    :reader   wptr
    :documentation
    "The `wptr' holds the foreign-pointer to NSWindow.
Or if the view is not contained in any window, return `nil'.

All views contained in a given window have the same `wptr'.

Dev Note:
+ use `with-wptr' to locally bind `wptr'
+ use `view-window' to get the `window' of view")
   (view-container
    :initform nil
    :type     (or null view)
    :reader   view-container
    :documentation
    "A `view' as container of the view. ")
   (view-position
    :initform (ns-point :x 0 :y 0)
    :type     ns-point
    :initarg  :view-position
    :reader   view-position
    :documentation
    "The position of the view in its container.

+ `set-view-position' sets the position of the view in its container.
+ `view-default-position' get the default position,
  used when initialize the view instance as default value
")
   (view-size
    :initform (ns-size :w 100 :h 100)
    :type     ns-size
    :initarg  :view-size
    :reader   view-size
    :documentation
    "The size of the view. ")
   (view-nickname
    :initform nil
    :initarg  :view-nickname
    :accessor view-nickname
    :documentation
    "The nickname of the view.
The nickname is used in conjunction with `view-named'. ")
   (view-alist
    :initform nil
    :accessor view-alist
    :documentation
    "Attributes of view.
Use `view-get' to get property of view;
Use `view-put' to update property of view;
Use `view-remprop' remove property from view. "))
  (:documentation
   "The class `simple-view' is the basic class of views,
from which all views inherit. A simple view does not have subviews
and thus can be drawn more easily. Views and dialog items are built
on simple views. "))

(defmethod print-object ((view simple-view) stream)
  (print-unreadable-object (view stream :type t)
    (if (slot-boundp view 'ptr)
        (if (view-nickname view)
            (format stream
                    "~S #x~X"
                    (view-nickname view)
                    (pointer-address (objc-ptr view)))
            (format stream
                    "#x~X"
                    (pointer-address (objc-ptr view))))
        (write-string "not init in ObjC" stream))))

;; view-size, view-position

(defmethod (setf view-size) ((size ns-size) (view simple-view))
  (let ((frame (ns-rect :origin (view-position view)
                        :size   size)))
    (with-ptr view
      (dispatch-main ()
        (invoke ptr "setFrame:" :ns-rect frame)))
    (setf (slot-value view 'view-size) size)))

(defmethod (setf view-position) ((pos ns-point) (view simple-view))
  (let ((frame (ns-rect :size   (view-size view)
                        :origin pos)))
    (with-ptr view
      (dispatch-main ()
        (invoke ptr "setFrame:" :ns-rect frame)))
    (setf (slot-value view 'view-position) pos)))

(defmethod view-default-size ((view simple-view))
  (ns-size :w 100 :h 100))

(defmethod view-default-position ((view simple-view))
  (ns-point :x 0 :y 0))

;; view-alist

(defun view-get (view key &optional default)
  "Get property KEY of VIEW.
Return property of KEY or return DEFAULT.

Parameters:
+ VIEW: the `simple-view'
+ KEY: the property name, possibly keyword
+ DEFAULT: fallback value"
  (declare (type simple-view view)
           (type symbol key))
  (let ((cell (assoc key (view-alist view))))
    (if cell
        (cdr cell)
        default)))

(defun (setf view-get) (value view key &optional default)
  (declare (type simple-view view)
           (type symbol key)
           (ignore default))
  (let ((cell (assoc key (view-alist view))))
    (if cell
        (setf (cdr cell) value)
        (unless (null value)
          (push (cons key value) (view-alist view))))
    value))

(declaim (inline view-put))
(defun view-put (view key value)
  "Set property KEY of VIEW to VALUE.
Return the VALUE. "
  (setf (view-get view key) value))

(defun view-remprop (view key)
  (setf (view-alist view)
        (delete key (view-alist view) :key #'car)))

;; initialize-instance

(defmethod alloc-init ((view simple-view))
  "`alloc-init' is called after WINDOW setted with `view-size'
and `view-position', and before other slot initialzation. "
  (invoke (setf (slot-value view 'ptr)
                (alloc (objc-class view)))
          "initWithFrame:"
          :ns-size (view-size view)
          :object))

(defmethod initialize-instance :after ((view simple-view)
                                       &key
                                         view-container
                                         help-spec
                                         view-size
                                         view-position)
  "The `initialize-instance' primary method for `simple-view'
initializes a simple view so that it can be used. (When instances
are actually made, the function used is `make-instance', which calls
`initialize-instance'.

Parameters:
+ WPTR: `foreign-pointer' or `null'
  A pointer to a NSWindow.
  The value is `nil' if the view is not contained in a window.

+ VIEW-POSITION: `ns-point'
  The position of the view in its container.
  The default is (ns-point :x 0 :y 0)

+ VIEW-SIZE: `ns-size'
  The size of the view.
  The default is (ns-size :w 100 :h 100)

+ VIEW-NICK-NAME:
  The nickname of the view.
  This keyword is used in conjunction with `view-named'.
  The default value is `nil'.

+ VIEW-FONT:
  The font specification used by the view.
  The default is `nil', which means that the view inherits its
  font from its container.

+ HELP-SPEC:
  A specification of a string for Balloon Help.
  The simplest specification is a string.
  For a description of the other possible `:help-spec' forms,
  see the file `help-manager.lisp'.

+ VIEW-CONTAINER: `view'
  A `view'. If this argument is specified and non-`nil',
  the instantiation procedure calls `set-view-container'
  to make this argument the container of the view being
  instantiated.
"
  (declare (type (or null ns-size) view-size)
           (type (or null ns-point) view-position)
           (type (or null (and view (not window))) view-container))
  (unless view-size
    (setf (slot-value view 'view-size) (view-default-size view)))
  (unless view-position
    (setf (slot-value view 'view-position) (view-default-position view)))
  (alloc-init view)
  (when help-spec
    (setf (view-get view :help-spec) help-spec))
  (when view-container
    (set-view-container view view-container)))


;;;; view

(defstruct (view-context (:conc-name ctx-))
  (ptr      (null-pointer) :type foreign-pointer)
  (origin   (ns-point)     :type ns-point)
  (size     (ns-size)      :type ns-size)
  ;; (fg-color (make-color)   :type color)
  ;; (bg-color (make-color)   :type color)
  )

(defclass view (simple-view)
  ((view-context
    :type   view-context
    :reader view-context)
   (view-valid
    :initform nil
    :accessor view-valid
    :documentation "For lazy clip-region updating. ")
   (view-scroll-position
    :initform (ns-point :x 0 :y 0)
    :initarg  :view-scroll-position
    :accessor view-scroll-position)
   (view-origin
    :initform (ns-point :x 0 :y 0)
    :accessor view-origin-slot)
   (view-subviews
    :initform (make-array 1 :adjustable t :fill-pointer 0)
    :reader view-subviews)
   (view-clip-region
    :initform nil
    :accessor view-clip-region-slot)))

(defmethod initialize-instance :after ((view view) &key view-subviews)
  (declare (type list view-subviews))
  (dolist (subview view-subviews)
    (setf (view-container subview) view)))

;; view-container

(defmethod (setf view-container) ((nothing null) (view simple-view))
  "Remove VIEW from previous container. "
  (declare (ignore nothing))
  (alx:when-let ((old-container (view-container view)))
    ;; remove in ObjC side
    (with-ptr view (invoke ptr "removeFromSuperview"))
    ;; remove in lisp side
    (setf (slot-value old-container 'view-subviews)
          (delete view (view-subviews view) :test #'eq))
    (setf (slot-value view 'view-container) nil)))

(defmethod (setf view-container) ((new-container view) (view simple-view))
  (setf (view-container view) nil)
  (invoke (objc-ptr view) "addSubview:" :object (objc-ptr new-container))
  new-container)

(defmethod view-contains-p ((view view) contained-view)
  (loop :for container := (view-container contained-view)
          :then (view-container container)
        :while container
        :if (eq container view)
          :return t))

(defmethod view-contains-p ((view null) contained-view)
  (declare (ignore contained-view))
  nil)


;;;; Dev Tools

(defmacro with-wptr (view &body body)
  "Bind `wptr' of VIEW with local variable `wptr' within BODY.

Dev Note:
+ use only within `coca.cocoa' package since `wptr'
  may not exported outside package
+ use `with-wptr' within `coca.coca' as much as possible
  to keep code clean"
  `(let ((wptr (wptr ,view)))
     ,@body))

;;;; view.lisp ends here
