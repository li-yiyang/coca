;;;; framed.lisp --- Mixin for classes support frame and setFrame: method

(in-package :coca.appkit)

(defconstant +flt-max+ 3.4028235d38
  "FLT_MAX in ObjC side. ")

(deftype framed-size ()
  "Range of `size' for `framed' should be limited between 0 and float max. "
  `(real 0 ,+flt-max+))

(defclass framed-mixin () ()
  (:documentation
   "Titled mixin for classes support frame and setFrame: method.

Use `frame' to get the frame (`ns-rect');
Use `set-frame' to set the frame size;
The default frame size is `default-frame'.

ObjC use coordinates where lower-left is origin:

    Y
    ^ |<- W ->|
    | +-------+--
    | |       | ^
    | |       | H
    | |       | V
    | +-------+--
    *-------------> X
   (0, 0)

Initialize Parameters:
+ FRAME: if set to be `nil' will skip frame initalize
  otherwise, should be like:
  + a list of (X Y W H)
  + a vector of #(X Y W H)
+ SIZE: should be like:
  + a list of (WIDTH HEIGHT)
  + a vector of #(WIDTH HEIGHT)
  + a number setting both WIDTH and HEIGHT
+ WIDTH, MIN-WIDTH, MAX-WIDTH
+ HEIGHT, MIN-HEIGHT, MAX-HEIGHT
+ ORIGIN: should be like:
  + a list of (X Y)
  + a vector of #(X Y)

Only avaliable if `obj' has `parent':
+ LOCATION:
+ X, Y: `location'

Dev Note:
+ subclass of `framed-mixin' should define their
  own (objc-ptr framed-mixin :frame) method,
  to make sure frame and setFrame: use the
  correct ObjC object pointer
"))

(defmethod initialize-instance :after
    ((framed framed-mixin)
     &key x y location width height size origin (frame nil frame?)
       (min-width  0) (max-width  +flt-max+)
       (min-height 0) (max-height +flt-max+))
  (declare (type framed-size
                 min-width min-height
                 max-width max-height))
  (assert (<= min-width  max-width))
  (assert (<= min-height max-height))
  (unless (and frame? (null frame))
    (multiple-value-bind (x* y* w* h*)
        (default-frame framed)
      (m:match frame
        ((list x y w h)
         (setf x* x
               y* y
               w* w
               h* h))
        ((vector x y w h)
         (setf x* x
               y* y
               w* w
               h* h)))
      (m:match size
        ((list w h)
         (setf w* w
               h* h))
        ((vector w h)
         (setf w* w
               h* h))
        ((and (type real) w)
         (setf w* w
               h* w)))
      (m:match origin
        ((list x y)
         (setf x* x
               y* y))
        ((vector x y)
         (setf x* x
               y* y)))
      (when width  (setf w* width))
      (when height (setf h* height))
      (alx:when-let ((ph (parent-height framed)))
        (m:match location
          ((list x y)
           (setf x* x
                 y* (- ph y h*)))
          ((vector x y)
           (setf x* x
                 y* (- ph y h*))))
        (when x (setf x* x))
        (when y (setf y* (- ph y h*))))
      (flet ((limit (low x high) (min (max low x) high)))
        (set-frame framed
                   x*
                   y*
                   (limit min-width  w* max-width)
                   (limit min-height h* max-height))))))

(defmethod objc-ptr ((framed framed-mixin) (name (eql :frame)))
  "By default `objc-ptr' of `framed-mixin' use default ObjC pointer. "
  (objc-ptr framed :ptr))

(defgeneric frame (framed)
  (:documentation
   "Get the frame of FRAMED.
Return values X, Y, W, H. ")
  (:method ((framed framed-mixin))
    (invoke (obj-ptr framed :frame) "frame" :ns-rect)))

(defgeneric set-frame (framed x y w h)
  (:documentation
   "Set the frame of FRAMED.
Return `framed'. ")
  (:method ((framed framed-mixin) (x real) (y real) (w real) (h real))
    (declare (type framed-size w h))
    (with-ptr framed (ptr :frame)
      (dispatch-main ()
        (invoke ptr "setFrame:" :ns-rect (x y w h))))
    framed))

(defgeneric visible-frame (framed)
  (:documentation
   "Get the visible frame of FRAMED.
Return values X, Y, W, H. ")
  (:method (framed) (frame framed)))

(defgeneric set-visible-frame (framed x y w h)
  (:documentation
   "Set the visible frame of FRAMED.
Return `framed'. ")
  (:method (framed x y w h)
    (set-frame framed x y w h)))

(defgeneric width (framed)
  (:documentation "Get/Set FRAMED width. ")
  (:method ((framed framed-mixin))
    (multiple-value-bind (x y w h) (frame framed)
      (declare (ignore x y h))
      w)))

(defmethod (setf width) ((width real) (framed framed-mixin))
  (multiple-value-bind (x y w h) (frame framed)
    (declare (ignore w))
    (set-frame framed x y width h)))

(defgeneric height (framed)
  (:documentation "Get/Set FRAMED height. ")
  (:method ((framed framed-mixin))
    (multiple-value-bind (x y w h) (frame framed)
      (declare (ignore x y w))
      h)))

(defmethod (setf height) ((height real) (framed framed-mixin))
  (multiple-value-bind (x y w h) (frame framed)
    (declare (ignore h))
    (set-frame framed x y w height)))

(defgeneric size (framed)
  (:documentation
   "Get the size of FRAMED.
Return values W, H. ")
  (:method ((framed framed-mixin))
    (multiple-value-bind (x y w h) (frame framed)
      (declare (ignore x y))
      (values w h))))

(defgeneric set-size (framed w h)
  (:documentation
   "Set the size of FRAMED.
Return `framed'. ")
  (:method ((framed framed-mixin) (width real) (height real))
    (multiple-value-bind (x y w h) (frame framed)
      (declare (ignore w h))
      (set-frame framed x y width height))))

(defgeneric origin (framed)
  (:documentation
   "Get the origin position of FRAMED.
Return values X, Y.

The ObjC origin of FRAMED is origined bottom-left:

    Y
    ^
    |
    |
    |
    *---------> X
")
  (:method ((framed framed-mixin))
    (multiple-value-bind (x y w h) (frame framed)
      (declare (ignore w h))
      (values x y))))

(defgeneric set-origin (framed x y)
  (:documentation
   "Set the origin position of FRAMED.
Return `framed'. ")
  (:method ((framed framed-mixin) (x real) (y real))
    (multiple-value-bind (x0 y0 w h) (frame framed)
      (declare (ignore x0 y0))
      (set-frame framed x y w h))))

(defgeneric parent-frame (framed)
  (:documentation
   "Get frame of FRAMED parent.
Return values X, Y, W, H or `nil' if OBJ has no parent. ")
  (:method (framed)
    (alx:when-let ((parent (parent framed)))
      (frame parent))))

(defgeneric parent-size (obj)
  (:documentation
   "Get size of OBJ parent.
Return values W, H or `nil' if OBJ has no parent. ")
  (:method (obj)
    (alx:when-let ((parent (parent obj)))
      (size parent))))

(defgeneric parent-width (obj)
  (:documentation
   "Get the `width' of OBJ.
Return `nil' if OBJ has no parent or width of OBJ parent. ")
  (:method (obj)
    (alx:when-let ((parent (parent obj)))
      (width parent))))

(defgeneric parent-height (obj)
  (:documentation
   "Get the `height' of OBJ.
Return `nil' if OBJ has no parent or height of OBJ parent. ")
  (:method (framed)
    (alx:when-let ((parent (parent framed)))
      (height parent))))

(defgeneric parent-origin (obj)
  (:documentation
   "Get origin of OBJ parent.
Return values X, Y or `nil' if OBJ has no parent. ")
  (:method (framed)
    (alx:when-let ((parent (parent framed)))
      (origin parent))))

(defgeneric location (framed)
  (:documentation
   "Get location of FRAMED.
Return values X, Y.

The position of FRAMED is origined upper-left:

   +---------------------+ parent
   |*---------> X        |
   ||                    |
   ||                    |
   ||        * (X, Y)    |
   |V Y                  |
   |                     |
   +---------------------+

And the location is relative to its `parent' by default. ")
  (:method ((framed framed-mixin))
    (multiple-value-bind (px py pw ph) (parent-frame framed)
      (declare (ignore px py pw))
      (multiple-value-bind (ox oy ow oh) (frame framed)
        (declare (ignore ow))
        (values ox (- ph oy oh))))))

(defgeneric set-location (framed x y)
  (:documentation
   "Set `location' of FRAMED.
Return FRAMED. ")
  (:method ((framed framed-mixin) (x real) (y real))
    (multiple-value-bind (px py pw ph) (parent-frame framed)
      (declare (ignore px py pw))
      (multiple-value-bind (ox oy ow oh) (frame framed)
        (declare (ignore ox oy))
        (set-frame framed x (- ph y oh) ow oh)))))

(defgeneric default-frame (framed)
  (:documentation
   "Return default frame configuration. ")
  (:method ((framed framed-mixin))
    (values 0d0 0d0 100d0 100d0)))


;;;; static-framed-mixin

(defclass static-framed-mixin ()
  ((static-frame
    :initform nil))
  (:default-initargs
   :frame nil)
  (:documentation
   "For those `frame' are not likely to change `obj',
use `static-framed-mixin' to cache frame values. "))

(defmethod frame :around ((framed static-framed-mixin))
  (with-slots (static-frame) framed
    (values-list
     (or static-frame
         (setf static-frame (multiple-value-list
                             (call-next-method)))))))

(defmethod set-frame :after ((framed static-framed-mixin) x y w h)
  (with-slots (static-frame) framed
    (setf static-frame nil)))


;;;; minmax-framed-mixin

(defclass minmax-framed-mixin ()
  ((min-width
    :initarg  :min-width
    :initform 0
    :type     framed-size
    :accessor min-width)
   (max-width
    :initarg  :max-width
    :initform +flt-max+
    :type     framed-size
    :accessor max-width)
   (min-height
    :initarg  :min-height
    :initform 0
    :type     framed-size
    :accessor min-height)
   (max-height
    :initarg  :max-height
    :initform +flt-max+
    :type     framed-size
    :accessor max-height))
  (:documentation
   "Mixin class add sizing check before modification of size.

Note: this is only lisp-side sizing check, the ObjC's min/max size
updating may not be synced. "))

(defmethod initialize-instance :after ((framed minmax-framed-mixin) &key)
  (with-slots (min-width max-width min-height max-height) framed
    (set-min-size framed min-width min-height)
    (set-max-size framed max-width max-height)))

(defgeneric set-min-size (framed min-width min-height)
  (:documentation
   "Set the minimal size of FRAMED of MIN-WIDTH and MIN-HEIGHT.
Return the FRAMED itself. ")
  (:method :around (framed w h)
    (call-next-method)
    framed)
  (:method ((framed minmax-framed-mixin) (w real) (h real))
    (declare (type framed-size w h))
    (setf (min-width  framed) w
          (min-height framed) h)))

(defgeneric set-max-size (framed max-width max-height)
  (:documentation
   "Set the minimal size of FRAMED of MAX-WIDTH and MAX-HEIGHT.
Return the FRAMED itself. ")
  (:method :around (framed w h)
    (call-next-method)
    framed)
  (:method ((framed minmax-framed-mixin) (w real) (h real))
    (declare (type framed-size w h))
    (setf (max-width  framed) w
          (max-height framed) h)))

(defmethod set-frame :before ((framed minmax-framed-mixin) x y (w real) (h real))
  (assert (<= (min-width  framed) w (max-width  framed)))
  (assert (<= (min-height framed) h (max-height framed))))

(defgeneric min-width (framed)
  (:documentation "Get/Set minimal width of FRAMED. "))

(defgeneric min-height (framed)
  (:documentation "Get/Set minimal height of FRAMED. "))

(defgeneric max-width (framed)
  (:documentation "Get/Set maximal width of FRAMED. "))

(defgeneric max-height (framed)
  (:documentation "Get/Set maximal height of FRAMED. "))

(defmethod (setf min-width) :before ((mw real) (framed minmax-framed-mixin))
  (assert (<= mw (max-width framed)))
  (when (< (width framed) mw)
    (setf (width framed) mw)))

(defmethod (setf max-width) :before ((mw real) (framed minmax-framed-mixin))
  (assert (<= (min-width framed) mw))
  (when (< mw (width framed))
    (setf (width framed) mw)))

(defmethod (setf min-height) :before ((mh real) (framed minmax-framed-mixin))
  (assert (<= mh (max-height framed)))
  (when (< (height framed) mh)
    (setf (height framed) mh)))

(defmethod (setf max-height) :before ((mh real) (framed minmax-framed-mixin))
  (assert (<= (min-height framed) mh))
  (when (< mh (height framed))
    (setf (height framed) mh)))

(defmethod (setf min-width) ((mw null) (framed minmax-framed-mixin))
  (setf (min-width framed) 0))

(defmethod (setf max-width) ((mw null) (framed minmax-framed-mixin))
  (setf (max-width framed) +flt-max+))

(defmethod (setf min-height) ((mh null) (framed minmax-framed-mixin))
  (setf (min-height framed) 0))

(defmethod (setf max-height) ((mh null) (framed minmax-framed-mixin))
  (setf (max-height framed) +flt-max+))

;;;; framed.lisp ends here
