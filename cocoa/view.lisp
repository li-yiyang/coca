;;;; view.lisp

(in-package :coca.cocoa)


;;;; View Protocol

(declaim (type (or null simple-view) *current-view*))
(defvar *current-view* nil
  "The view where drawing currently occurs.

See `focus-view' and `with-focused-view'.

It is initially bounded to `nil'. ")
(pushnew '*current-view* *global-objc-objects-variables*)

(declaim (type (or null simple-view) *mouse-view*))
(defvar *mouse-view* nil
  "The view that the mouse is over.
This variable is updated by the `window-update-cursor'.

The `*mouse-view*' view is one whose `view-cursor' method
decides which cursor to select.

It is initially bounded to `nil'. ")
(pushnew '*mouse-view* *global-objc-objects-variables*)

(defgeneric focus-view (view &optional font-view)
  (:documentation
   "Setup `view-context' as current CGContext
and sets the clip region and origin so that drawing
will occur in the coordinate system of `view'.

The `focus-view' function is not normally called directly.
In general, `with-focused-view' should be used when drawing
to `view'. "))

(defgeneric view-container (view)
  (:documentation
   "Returns the VIEW's containing `view'. "))

(defgeneric view-subviews (view)
  (:documentation
   "Returns a vector containing all of the VIEW's subviews.
This vector should never be changed directly. It is
updated automatically by calls to `set-view-container'."))

(defmacro do-subviews ((subview-var view
                        &optional (subview-type nil stp))
                       &body body)
  "For each subview of VIEW of the given SUBVIEW-TYPE,
the macro `do-subviews' executes BODY with SUBVIEW-VAR
bound to the subview.

Syntax:

    (do-subviews (SUBVIEW-VAR VIEW [SUBVIEW-TYPE])
      &body)

Parameters:
+ SUBVIEW-VAR: a variable
+ VIEW: a `view'
+ SUBVIEW-TYPE: a Common Lisp type specifier
+ FORM: Zero or more MCL forms
"
  (declare (type symbol subview-var))
  (if stp
      `(loop :for ,subview-var :across (view-subviews ,view)
             :if (typep ,subview-var ,subview-type)
               :do (progn ,@body))
      `(loop :for ,subview-var :across (view-subviews ,view)
             :do (progn ,@body))))

(defgeneric map-subviews (view function &optional subview-type)
  (:documentation
   "For each subview of VIEW of the given SUBVIEW-TYPE,
the generic function `map-subviews' calls function with subview
as its single argument.

Parameters:
+ VIEW: a `view'
+ FUNCTION: a function
+ SUBVIEW-TYPE: a Common Lisp type specifier"))

(defgeneric view-named (name view)
  (:documentation
   "Returns the first subview of VIEW whose nickname is NAME.
The subview are searched in the order in which they were added
to VIEW.

Parameters:
+ NAME: any object, but usually a symbol.
  Nicknames are compared using `eq'
+ VIEW: a `view'"))

(defgeneric find-named-sibling (view name)
  (:documentation
   "Performs a search in VIEW's container and
returns the first item in the container whose nickname is NAME.
For example, given a dialog item view, it performs a search in the
view that is VIEW's container to find another item with the nickname NAME.
The items are searched in the order in which they were added to VIEW's
container.

Parameters:
+ VIEW: a `simple-view'
+ NAME: any object, but usually a symbol
  Nicknames are compared using `eq'. "))

(defgeneric add-subviews (view &rest subviews)
  (:documentation
   "Sets the container of each of SUBVIEWS to VIEW.
If any of the subviews are already owned by VIEW,
`add-subviews' does nothing.

Parameters:
+ VIEW: a `view'
+ SUBVIEWS: a `view' or `simple-view', but not a `window';
  SUBVIEWS must be able to contained within VIEW"))

(defgeneric remove-subviews (view &rest subviews)
  (:documentation
   "Removes each of SUBVIEWS from VIEW.
If subview is not in VIEW, an error is signaled.

Parameters:
+ VIEW: a `view'
+ SUBVIEWS: a `view' or `simple-view', but not a `window';
  SUBVIEWS must be able to be contained within VIEW. "))

(defgeneric find-clicked-subview (view where)
  (:documentation
   "Returns the subview of VIEW that contains the `ns-point' WHERE
in its click region. The method for `nil' searches all windows for
a subview containing WHERE in its click region.

This function is similar to `find-view-containing-point',
but `find-clicked-subview' calls `point-in-click-region-p',
and `find-view-containing-point' calls `view-contains-point-p'.
The default method of `point-in-click-region-p' for views or
simple views simply calls `view-contains-point-p', but users
can write methods to make views invisible to mouse clicks.

Parameters:
+ VIEW: a view or subview
+ WHERE: `ns-point' in the local coordinate system of the VIEW container"))

(defgeneric wptr (view)
  (:documentation
   "Returns the foreign-pointer to a NSWindow.
Or `nil' if the VIEW is not contained in a `window'.

All views contained in a given window have the same `wptr'.

You can test if a VIEW's window has been closed by checking
whether the value of its `wptr' slot is `nil'. "))

(defgeneric view-window (view)
  (:documentation
   "Returns the `window' containing VIEW.
Or `nil' if the VIEW is not contained in a `window'.
If VIEW is a `window', `view-window' returns the window. "))

(defgeneric view-position (view)
  (:documentation
   "Upper-left coordinates of VIEW position.
Return `ns-point' for VIEW position. "))

(defgeneric view-size (view)
  (:documentation
   "Size of VIEW.
Return `ns-size' for VIEW size. "))

(defgeneric view-default-size (view)
  (:documentation
   "Return the default value of the `:view-size' initarg of VIEW. "))

(defgeneric view-default-position (view)
  (:documentation
   "Return the default value of the `:view-position' initarg of VIEW. "))

(defgeneric view-nickname (view)
  (:documentation
   "Returns the nickname of the VIEW.
The nickname is used in conjuection with `view-named'. "))

;; (defun find-view-containing-point (view point &optional direct-subviews-only))

(defgeneric point-in-click-region-p (view where)
  (:documentation
   "Called by `view-click-event-handler' to determine whether WHERE
is in VIEW. The default method calls `view-contains-point-p'.

Parameters:
+ VIEW: a simple view or view
+ WHERE:
  + for a view, the cursor position of the view in the local
    coordinate system when the mouse is clicked
  + for a simple view, the cursor position of the simple
    view in the local coordinate system of the view's container
    when the mouse is clicked"))

(defgeneric view-activate-event-handler (view)
  (:documentation
   "Called by the event system when the window containing the
VIEW is made active.

The definition for `simple-view' does nothing.
The definition for `view' calls `view-activate-event-handler'
on each subview.

Specialize this generic function if your view needs to indicate
visually that it is active. "))

(defgeneric view-deactivate-event-handler (view)
  (:documentation
   "Called by the event system to deactivate a VIEW.
It is called when the window containing the view is
active and a different window is made active.

The definition for `simple-view' does nothing.
The definition for `view' calls `view-deactivate-event-handler'
on each subview.

Specialize this generic function if your view needs to indicate
visually that it has been deactivated. "))

(defgeneric view-click-event-handler (view where)
  (:documentation
   "Called by the event system when a mouse click occurs.

The `simple-view' method does nothing.
The `view' method calls `view-convert-coordinates-and-click'
on the first subview for which `point-in-click-region-p'
returns `t'.

The function `view-click-event-handler' scans subviews in
the opposite order as does `view-draw-contents'.
The first view added is the first one drawn but the last
one to be queried during clicking.

If you define any `view-click-event-handler' for window,
they must call `call-next-method'.

Parameters:
+ VIEW: a simple view or view
+ WHERE:
  + for a `view', the mouse click position (the position
    when the mouse is cliced) of the view in the local
    coordinate system.
  + for a simple view, the mouse click position of the
    simple view in the local coordinate system of the
    view's container
"))

(defgeneric view-convert-coordinates-and-click (view where container)
  (:documentation
   "Runs `view-click-event-handler' on the cursor position
within the VIEW's CONTAINER. "))

(defgeneric view-focus-and-draw-contents (view &optional visrgn cliprgn)
  (:documentation
   "Used whenever a view needs to be focused on before any portion
of its contents is redrawn. The method for view focuses on the view,
then calls `view-draw-contents' if the VISRGN and CLIPRGN region records
overlap.

The method for `simple-view' focuses on the view's container,
then calls `view-draw-contents'."))

(defgeneric view-font (view)
  (:documentation
   "Returns the font spec used for drawing text in the window.
Due to an idiosyncrasy of the Macintosh computer, a
font size of 0 points may appear as a font size of 12 points."))

(declaim (inline set-view-font))
(defun set-view-font (view font-spec)
  "Sets the font spec of VIEW to FONT-SPEC.

Parameters:
+ VIEW
+ FONT-SPEC: a font specifier

Dev Note:
+ implement method for (setf view-font)"
  (setf (view-font view) font-spec))

(defgeneric view-default-font (view)
  (:documentation
   "If a `:view-font' initialzation argument is not specified when
a view is created, the generic function `view-default-font' is
called to determine its font. "))

(defgeneric view-cursor (view point)
  (:documentation
   "Returns the `cursor' to display when the mouse is at POINT,
a `ns-point' in VIEW. It is called by `window-update-cursor'
as part of the default `window-null-event-handler'.

Specialize the `view-cursor' generic function to change your view's
cursor to one of the following predefined cursors:

+ `*arrow-cursor*'
+ `*i-beam-cursor*'
+ `*pointing-hand-cursor*'
+ `*closed-hand-cursor*'
+ `*open-hand-cursor*'
+ `*cross-hair-cursor*'

or to a user-defined cursor value that could be found by `find-cursor'."))


;;;; CocaView

(define-objc-class "CocaView" "NSView")


;;;; simple-view

(defclass simple-view (tgs:fundamental-output-stream
                       objc-obj)
  ((ptr
    :documentation
    "The foreign-pointer to NSView. ")
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
    "A container as parent of the view. ")
   (view-origin
    :initform (ns-point :x 0d0 :y 0d0)
    :type     ns-point
    :reader   view-origin
    :documentation
    "The frame origin of NSView (bottom-left) in its container.

Dev Note:
+ use `view-position' to get upper-left coordinates
")
   (%position
    :type    ns-point
    :reader  view-position
    :documentation
    "Cache of `position'.

Dev Note:
+ this is updated after `view-origin' is updated
+ this is used to reduce allocation of `ns-point'
+ never refer this slot unless you know what are you doing")
   (view-size
    :initform (ns-size :w 100 :h 100)
    :type     ns-size
    :initarg  :view-size
    :reader   view-size
    :documentation
    "The size of the view.

Dev Note:
+ setting `size' of view should keep `view-position'")
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

;; view-size, view-position, view-origin

(defun flip-ns-point! (view p1 p2
                       &optional (container-size (view-size (view-container view))))
  "Flip `ns-point' of VIEW from P1 to P2.
Return modified P2.

Side Effects:
+ P2 will be modified"
  (declare (type simple-view view)
           (type ns-point p1 p2))
  (setf (ns-point-x p2) (ns-point-x p1)
        (ns-point-y p2) (- (ns-size-h  container-size)
                           (ns-size-h  (view-size view))
                           (ns-point-y p1)))
  p2)

(defun copy-ns-point! (p1 p2)
  "Copy `ns-point' value from P1 to P2.
Return modified P2.

Side Effects:
+ P2 will be modified"
  (declare (type ns-point p1 p2))
  (setf (ns-point-x p2) (ns-point-x p1)
        (ns-point-y p2) (ns-point-y p1))
  p2)

(defun copy-ns-size! (s1 s2)
  "Copy `ns-size' value from S1 to S2.
Return modified S2.

Side Effects:
+ S2 will be modified"
  (declare (type ns-size s1 s2))
  (setf (ns-size-w s2) (ns-size-w s1)
        (ns-size-h s2) (ns-size-h s1))
  s2)

(defmethod (setf view-origin) ((origin ns-point) (view simple-view))
  (let ((frame (ns-rect :size   (view-size view)
                        :origin origin)))
    (with-ptr view
      (dispatch-main () (invoke ptr "setFrame:" :ns-rect frame)))
    (copy-ns-point! origin (view-origin view))
    (setf (slot-value view '%position)
          (flip-ns-point! view origin (slot-value view '%position)))))

(declaim (inline set-view-position))
(defun set-view-position (view x &optional y)
  "Set the position of VIEW.
Return `ns-point' of updated position.

Syntax:

    (set-view-position VIEW X Y)
    ;; => (setf (view-position VIEW) (ns-point :x X :y Y))

    (set-view-position VIEW POS)
    ;; => (setf (view-position VIEW) POS)

Dev Note:
+ see (setf view-position)"
  (if y
      (setf (view-position view) (ns-point :x x :y y))
      (setf (view-position view) x)))

(defmethod (setf view-position) ((pos ns-point) (view simple-view)
                                 &aux (container (view-container view)))
  (when container
    (let* ((origin (flip-ns-point! view pos (view-origin view)))
           (frame  (ns-rect :size   (view-size view)
                            :origin origin)))
      (with-ptr view
        (dispatch-main ()
          (invoke ptr "setFrame:" :ns-rect frame)))))
  (copy-ns-point! pos (slot-value view '%position)))

(declaim (inline set-view-size))
(defun set-view-size (view w &optional h)
  "Set the size of VIEW.
Return `ns-size' of updated size.

Syntax:

    (set-view-size VIEW W H)
    ;; => (setf (view-size VIEW) (ns-size :w W :h H))

    (set-view-size VIEW SIZE)
    ;; => (setf (view-size VIEW) SIZE)

Dev Note:
+ see (setf view-size)"
  (if h
      (setf (view-size view) (ns-size :w w :h h))
      (setf (view-size view) w)))

(defmethod (setf view-size) ((size ns-size) (view simple-view)
                             &aux (container (view-container view)))
  (prog1 (copy-ns-size! size (view-size view))
    (when container
      (setf (slot-value view 'view-origin)
            (flip-ns-point! view (view-position view) (view-origin view)))
      (let ((frame (ns-rect :size   (view-size   view)
                            :origin (view-origin view))))
        (with-ptr view
          (dispatch-main () (invoke ptr "setFrame:" :ns-rect frame)))))))

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
          :ns-rect (ns-rect :origin (view-origin view)
                            :size   (view-size   view))
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
           (type (or null view) view-container))
  (unless view-size
    (setf (slot-value view 'view-size) (view-default-size view)))
  (setf (slot-value view '%position)
        (or view-position (view-default-position view)))
  (alloc-init view)
  (when view-container
    (set-view-container view view-container))
  (when help-spec
    (setf (view-get view :help-spec) help-spec))
  (with-ptr view
    (tg:finalize view (lambda () (alx:when-let ((view (find-objc-obj ptr)))
                                   (dealloc view))))))

(defmethod dealloc ((view simple-view))
  (with-ptr view
    (release ptr))
  (setf (slot-value view 'wptr) nil))

;; view-cursor

(defmethod view-cursor ((view simple-view) (point ns-point))
  (let ((container (view-container view)))
    (if container
        ;; TODO: should POINT be modified?
        (view-cursor container (offset-ns-point! (view-position view) point))
        *arrow-cursor*)))

(defmethod view-mouse-enter-event-handler ((view simple-view))
  "The method for `simple-view' do nothing. "
  (declare (ignore view)))

(defmethod view-mouse-leave-event-handler ((view simple-view))
  "The method for `simple-view' do nothing. "
  (declare (ignore view)))

;; view-nickname

(declaim (inline set-view-nickname))
(defun set-view-nickname (view new-name)
  "Sets the nickname of the VIEW to NEW-NAME and return NEW-NAME.

Parameters:
+ VIEW: a view or simple view
+ NEW-NAME: a name, usually a symbol or string"
  (declare (type simple-view view))
  (setf (view-nickname view) new-name))


;;;; contexted-view-mixin

(defstruct (view-context (:conc-name ctx-))
  (ptr      (null-pointer) :type foreign-pointer)
  (origin   (ns-point)     :type ns-point)
  (size     (ns-size)      :type ns-size)
  ;; (fg-color (make-color)   :type color)
  ;; (bg-color (make-color)   :type color)
  )

(defclass context-view-mixin ()
  ((view-context
    :type   view-context
    :reader view-context)))


;;;; scroller-view-mixin

(defclass scroller-view-mixin ()
  ((scroller-view-ptr
    :documentation
    "The foreign-pointer to NSScrollerView. ")
   (view-scroll-position
    :initform (ns-point :x 0 :y 0)
    :initarg  :view-scroll-position
    :accessor view-scroll-position
    :documentation
    "Current position of NSScrollerView scroll position. ")))


;;;; view

(defclass view (simple-view)
  ((objc-class
    :initform (coerce-to-objc-class "CocaView")
    :documentation
    "The ObjC class of NSView. ")
   (view-valid
    :initform nil
    :accessor view-valid
    :documentation
    "For lazy clip-region updating. ")
   (view-subviews
    :initform (make-array 1 :adjustable t :fill-pointer 0)
    :reader   view-subviews))
  (:documentation
   "The `view' class is the class of views that can include subviews.
It is built on `simple-view'. "))

(defmethod initialize-instance :after ((view view) &key view-subviews)
  (declare (type list view-subviews))
  (dolist (subview view-subviews)
    (setf (view-container subview) view)))

;; TODO: should subviews of VIEW be released when view is released?
(defmethod dealloc :after ((view view))
  (loop :for subview :across (view-subviews view)
        :do (dealloc subview)))

;; view-position fix for subview after setting view-size

(defmethod (setf view-size) :after ((size ns-size) (view view))
  (loop :for subview :across (view-subviews view)
        :do (setf (view-position subview) (view-position subview))))

;; view-container

(defun set-view-container (view new-container)
  "Sets VIEW's containing view to NEW-CONTAINER.
This will always set `view-container' of VIEW to nil first,
then set as NEW-CONTAINER.

Parameters:
+ VIEW: a `simple-view'
+ NEW-CONTAINER: a `view' or `nil'

Dev Note:
+ implemento (setf view-container)"
  (declare (type simple-view view)
           (type (or null view) new-container))
  (setf (view-container view) new-container))

(defmethod (setf view-container) ((nothing null) (view simple-view))
  "Remove VIEW from previous container. "
  (declare (ignore nothing))
  (alx:when-let ((old-container (view-container view)))
    ;; remove in ObjC side
    (with-ptr view
      (dispatch-main () (invoke ptr "removeFromSuperview")))
    ;; remove in lisp side
    (setf (slot-value old-container 'view-subviews)
          (delete view (view-subviews view) :test #'eq))
    (setf (slot-value view 'view-container) nil)))

(defmethod (setf view-container) ((new-container view) (view simple-view))
  (when (view-container view)
    (setf (view-container view) nil))
  (let ((container* (objc-ptr new-container))
        (subview*   (objc-ptr view)))
    (dispatch-main ()
      (invoke container* "addSubview:" :object subview*)))
  (prog1 (setf (slot-value view 'view-container) new-container)
    ;; set position correctly
    (set-view-position view (view-position view))
    (vector-push-extend view (slot-value new-container 'view-subviews))))

(defmethod view-contains-p ((view view) contained-view)
  (loop :for container := (view-container contained-view)
          :then (view-container container)
        :while container
        :if (eq container view)
          :return t))

(defmethod view-contains-p ((view null) contained-view)
  (declare (ignore contained-view))
  nil)

(defun subviews (view &optional (subview-type 'simple-view))
  "Returns a list of subviews of VIEW.
If SUBVIEW-TYPE is present, only subviews matching that type are returned.

Parameters:
+ VIEW: a `view'
+ SUBVIEW-TYPE: a Common Lisp type specifier"
  (declare (type view view))
  (loop :for subview :across (view-subviews view)
        :if (typep subview subview-type)
          :collect subview))

(defmethod map-subviews ((view view) function &optional (subview-type nil stp))
  (if stp
      (do-subviews (subview view subview-type)
        (funcall function subview))
      (do-subviews (subview view)
        (funcall function subview))))

(defmethod view-named (name (view view))
  (do-subviews (subview view)
    (when (eq (view-nickname subview) name)
      (return-from view-named subview))))

(defmethod find-named-sibling ((view simple-view) name)
  (let ((container (view-container view)))
    (and container (view-named name container))))

(defmethod add-subviews ((view view) &rest subviews)
  (dolist (subview subviews)
    (set-view-container subview view)))

(defmethod remove-subviews ((view view) &rest subviews)
  (dolist (subview subviews)
    (cond ((typep subview 'window)
           (error "SUBVIEW ~A should not be `window'" subview))
          ((view-contains-p view subview)
           (set-view-container subview nil))
          (t
           (error "SUBVIEW ~A is not in VIEW ~A" subview view)))))


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
