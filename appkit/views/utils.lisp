;;;; utils.lisp --- Utils mixin for building NSViews-like widgets

(in-package :coca.appkit)

(define-objc-enum (:ns-control-state :alias :long)
  "Whether a control is on, off, or in a mixed state. "
  (:on    1)
  (:off   0)
  (:mixed -1))

(defclass state-mixin () ()
  (:documentation
   "Mixin class for obj support state method. "))

(defgeneric state (state-mixin)
  (:method ((obj state-mixin))
    (with-ptr obj ptr
      (invoke ptr "state" :ns-control-state))))

(defmethod (setf state) (state (obj state-mixin))
  (declare (type (member :on :off :mixed) state))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setState:" :ns-control-state state))))

(defclass bordered-mixin () ()
  (:documentation
   "Mixin class for obj support isBordered method. "))

(defgeneric borderedp (bordered-mixin)
  (:documentation
   "Get/Set if BORDERED-MIXIN is bordered or not. ")
  (:method ((obj bordered-mixin))
    (with-ptr obj ptr
      (invoke ptr "isBordered" :bool))))

(defmethod initialize-instance :after ((obj bordered-mixin) &key bordered)
  (setf (borderedp obj) bordered))

(defmethod (setf borderedp) (bordered (obj bordered-mixin)
                             &aux (borderedp (and bordered t)))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setBordered:" :bool borderedp))
    borderedp))

(defclass value-mixin () ()
  (:documentation
   "Base mixin class for those who support `value' method.

Dev Note:
+ don't use it but use it's subclasses.
"))

(defmethod initialize-instance :after ((obj value-mixin) &key (value nil value?))
  (when value? (setf (value obj) value)))

(defclass string-value-mixin (value-mixin) ()
  (:documentation
   "Mixin class for obj support stringValue method. "))

(defgeneric value (widget)
  (:documentation
   "Return value of WIDGET. ")
  (:method ((obj string-value-mixin))
    (with-ptr obj ptr
      (invoke ptr "stringValue" :ns-string))))

(defmethod (setf value) ((value string) (obj string-value-mixin))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setStringValue:" :ns-string value))))

(defclass font-mixin ()
  ((font
    :initarg  :font
    :initform (make-font)
    :type     font
    :reader   font))
  (:documentation
   "Mixin class for those who support font method. "))

(defmethod initialize-instance :after ((obj font-mixin) &key)
  (with-slots (font) obj
    (setf (font obj) font)))

(defgeneric font (widget)
  (:documentation
   "Return the `font' of WIDGET. "))

(defmethod (setf font) ((font font) (obj font-mixin))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setFont:" :ns-font font))
    (setf (slot-value obj 'font) font)))

;;;; utils.lisp ends here
