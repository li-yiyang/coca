;;;; text-field.lisp --- Wrapper for NSTextField-like widgets

(in-package :coca.appkit)

(define-objc-enum :ns-text-alignment
  "NSTextAlignment"
  (:left      0)
  (:center    1)
  (:right     2)
  (:justified 3)
  (:natural   4))

(define-objc-enum :ns-text-field-bezel
  (:square  0)
  (:rounded 1))

(defclass base-text-field (base-view
                           owned-mixin
                           font-mixin
                           string-value-mixin)
  ((text-alignment
    :initarg  :text-alignment
    :initform :left
    :type     (member :left :center :right :justified :natural)
    :reader   text-alignment)
   (editable
    :initarg  :editable
    :initform t
    :type     boolean
    :reader   editable)
   (selectable
    :initarg  :selectable
    :initform t
    :type     boolean
    :reader   selectable)
   (draws-background-p
    :initarg  :draws-background
    :initform t
    :reader   draws-background-p)
   (borderedp
    :initarg  :bordered
    :initform t
    :reader   borderedp))
  (:documentation
   "Base class for NSTextField. ")
  (:default-initargs
   :objc-class "NSTextField"))

(defmethod initialize-instance :after ((text base-text-field) &key)
  (with-slots (text-alignment
               editable
               selectable
               draws-background-p
               borderedp)
      text
    (dispatch-main ()
      (setf (text-alignment     text) text-alignment
            (editable           text) editable
            (selectable         text) selectable
            (draws-background-p text) draws-background-p
            (borderedp          text) borderedp))))

(defgeneric text-alignment (widget)
  (:documentation
   "Text alignment of WIDGET.

Possible Values:
+ `:left'
+ `:center'
+ `:right'
+ `:justified'
+ `:natural'"))

(defgeneric editable (widget)
  (:documentation
   "Get/Set if WIDGET is editable. "))

(defgeneric selectable (widget)
  (:documentation
   "Get/Set if WIDGET is selectable. "))

(defgeneric draws-background-p (widget)
  (:documentation
   "Get/Set if WIDGET draws background. "))

(defmethod (setf text-alignment) (alignment (text base-text-field))
  (with-ptr text ptr
    (let ((alignment* (as-ns-text-alignment alignment)))
      (dispatch-main ()
        (invoke ptr "setAlignment:" :unsigned-long alignment*)))))

(defmethod (setf editable) (value* (text base-text-field)
                            &aux (value (and value* t)))
  (with-ptr text ptr
    (dispatch-main ()
      (invoke ptr "setEditable:" :bool value))
    (setf (slot-value text 'editable) value)))

(defmethod (setf selectable) (value* (text base-text-field)
                              &aux (value (and value* t)))
  (with-ptr text ptr
    (dispatch-main ()
      (invoke ptr "setSelectable:" :bool value))
    (setf (slot-value text 'selectable) value)))

(defmethod (setf draws-background-p) (value* (text base-text-field)
                                      &aux (value (and value* t)))
  (with-ptr text ptr
    (dispatch-main ()
      (invoke ptr "setDrawsBackground:" :bool value))
    (setf (slot-value text 'draws-background-p) value)))

(defmethod (setf borderedp) (value* (text base-text-field)
                             &aux (value (and value* t)))
  (with-ptr text ptr
    (dispatch-main ()
      (invoke ptr "setBezeled:" :bool value))
    (setf (slot-value text 'borderedp) value)))

(defclass label (base-text-field)
  ()
  (:documentation
   "Static text label. ")
  (:default-initargs
   :value (alx:required-argument :value)
   :selectable       nil
   :editable         nil
   :bordered         nil
   :draws-background nil))

(defclass placeholder-mixin ()
  ((placeholder
    :initarg  :placeholder
    :initform ""
    :type     string
    :reader   placeholder)))

(defmethod initialize-instance :after ((obj placeholder-mixin) &key)
  (with-slots (placeholder) obj
    (setf (placeholder obj) placeholder)))

(defgeneric placeholder (widget)
  (:documentation
   "Get/Set the placeholder string of WIDGET. "))

(defmethod (setf placeholder) ((string string) (obj placeholder-mixin))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setPlaceholderString:" :ns-string string))
    (setf (slot-value obj 'placeholder) string)))

(defmethod (setf placeholder) ((none null) (obj placeholder-mixin))
  (setf (placeholder obj) ""))

(defmethod (setf placeholder) (value (obj placeholder-mixin))
  (setf (placeholder obj) (princ-to-string value)))

(defclass text-input (base-text-field
                      placeholder-mixin
                      target-mixin)
  ()
  (:documentation
   "One line text input. ")
  (:default-initargs
   :selectable       t
   :editable         t
   :bordered         t
   :draws-background t))

;;;; text-field.lisp ends here
