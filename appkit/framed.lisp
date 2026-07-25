;;;; framed.lisp --- Mixin for classes support frame and setFrame: method

(in-package :coca.appkit)

(defconstant +flt-max+ 3.4028235d38
  "FLT_MAX in ObjC side. ")

(deftype framed-size ()
  "Range of `size' for `framed' should be limited within `+flt-max+'. "
  `(real 0 ,+flt-max+))

(defclass framed-mixin () ()
  (:documentation
   "Titled mixin for classes support frame and setFrame: method.

Use `frame' to get the frame (`ns-rect');
Use `set-frame' to set the frame size.

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

Dev Note:
+ subclass of `framed-mixin' should define their
  own (objc-ptr framed-mixin :frame) method,
  to make sure frame and setFrame: use the
  correct ObjC object pointer
"))

(defmethod objc-ptr ((framed framed-mixin) (name (eql :frame)))
  "By default `objc-ptr' of `framed-mixin' use default ObjC pointer. "
  (objc-ptr framed :ptr))

(defgeneric frame (framed)
  (:documentation
   "Get the frame of FRAMED.
Return values X, Y, W, H. ")
  (:method ((framed framed-mixin))
    (invoke (obj-ptr framed :frame) "frame" :ns-rect)))

(defgeneric visible-frame (framed)
  (:documentation
   "Get the visible frame of FRAMED.
Return values X, Y, W, H. ")
  (:method (framed) (frame framed)))

(defgeneric set-frame (framed x y w h)
  (:documentation
   "Set the frame of FRAMED.
Return `framed'. ")
  (:method ((framed framed-mixin) (x real) (y real) (w real) (h real))
    (declare (type framed-size w h))
    (with-ptr framed (ptr :frame)
      (dispatch-main ()
        (invoke ptr "setFrame:" :ns-rect (x y w h))))))

(defmethod width ((framed framed-mixin))
  (multiple-value-bind (x y w h) (frame framed)
    (declare (ignore x y h))
    w))

(defmethod (setf width) ((width real) (framed framed-mixin))
  (multiple-value-bind (x y w h) (frame framed)
    (declare (ignore w))
    (set-frame framed x y width h)))

(defmethod height ((framed framed-mixin))
  (multiple-value-bind (x y w h) (frame framed)
    (declare (ignore x y w))
    h))

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
Return values X, Y, W, H. ")
  (:method (framed)
    (frame (parent framed))))

(defgeneric parent-size (framed)
  (:documentation
   "Get size of FRAMED parent.
Return values W, H. ")
  (:method (framed)
    (multiple-value-bind (x y w h) (parent-frame framed)
      (declare (ignore x y))
      (values w h))))

(defgeneric parent-origin (framed)
  (:documentation
   "Get origin of FRAMED parent.
Return values X, Y. ")
  (:method (framed)
    (multiple-value-bind (x y w h) (parent-frame framed)
      (declare (ignore w h))
      (values x y))))

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


;;;; static-framed-mixin

(defclass static-framed-mixin ()
  ((static-frame
    :initform nil))
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
updating will not be synced. "))

(defmethod set-frame :before ((framed minmax-framed-mixin) x y (w real) (h real))
  (assert (<= (min-width  framed) w (max-width  framed)))
  (assert (<= (min-height framed) h (max-height framed))))

(defmethod (setf min-width) :before ((mw real) (framed minmax-framed-mixin))
  (assert (<= mw (max-width framed))))

(defmethod (setf max-width) :before ((mw real) (framed minmax-framed-mixin))
  (assert (<= (min-width framed) mw)))

(defmethod (setf min-height) :before ((mh real) (framed minmax-framed-mixin))
  (assert (<= mh (max-height framed))))

(defmethod (setf max-width) :before ((mh real) (framed minmax-framed-mixin))
  (assert (<= (min-height framed) mh)))

(defmethod (setf min-width) ((mw null) (framed minmax-framed-mixin))
  (setf (min-width framed) 0))

(defmethod (setf max-width) ((mw null) (framed minmax-framed-mixin))
  (setf (max-width framed) +flt-max+))

(defmethod (setf min-height) ((mh null) (framed minmax-framed-mixin))
  (setf (min-height framed) 0))

(defmethod (setf max-height) ((mh null) (framed minmax-framed-mixin))
  (setf (max-height framed) +flt-max+))

;;;; framed.lisp ends here
