;;;; combobox.lisp --- Wrapper of NSComboBox

(in-package :coca.appkit)

(defclass combobox (base-view
                    target-mixin
                    font-mixin)
  ((item-list*
    :initform ()
    :type     list
    :documentation
    "An alist of (VALUE . DISPLAY-STRING). ")
   (intercell-spacing*
    :initarg  :intercell-spacing
    :initform (make-array 2 :element-type 'double-float
                            :initial-contents '(3d0 2d0)))
   (has-vertical-scroller-p
    :initarg  :has-vertical-scoller
    :initform t
    :reader   has-vertical-scroller-p
    :type     boolean)
   (button-bordered-p
    :initarg  :button-bordered
    :initform t
    :reader   button-bordered-p
    :type     boolean)
   (item-height
    :initarg  :item-height
    :initform 16.0d0
    :reader   item-height
    :type     (double-float 0d0)))
  (:documentation
   "A combobox.

A NSComboBox is a view that displays a list of values in
a pop-up menu where the user selects a value or types in
a custom value.

")
  (:default-initargs
   :objc-class "NSComboBox"))

(defmethod (setf has-vertical-scroller-p) (value (combobox combobox)
                                           &aux (value* (and value t)))
  (with-ptr combobox ptr
    (dispatch-main ()
      (invoke ptr "setHasVerticalScroller:" :bool value*)))
  (setf (slot-value combobox 'has-vertical-scroller-p) value*))

(defgeneric set-intercell-spacing (widget width height)
  (:documentation
   "Set `intercell-spacing' of WIDGET with WIDTH and HEIGHT.
Return WIDGET itself. ")
  (:method ((combobox combobox) (width number) (height number))
    (declare (type (real 0) width))
    (with-slots (intercell-spacing*) combobox
      (let ((width  (coerce width  'double-float))
            (height (coerce height 'double-float)))
        (with-ptr combobox ptr
          (dispatch-main ()
            (invoke ptr "setIntercellSpacing:" :ns-size (width height))))
        (setf (aref intercell-spacing* 0) width
              (aref intercell-spacing* 1) height))
      combobox)))

(defmethod (setf button-bordered-p) (value (combobox combobox)
                                     &aux (value* (and value t)))
  (with-ptr combobox ptr
    (dispatch-main ()
      (invoke ptr "setButtonBordered:" :bool value*)))
  (setf (slot-value combobox 'button-bordered-p) value*))

(defmethod (setf item-height) ((height number) (combobox combobox))
  (declare (type (real 0) height))
  (with-ptr combobox ptr
    (let ((height (coerce height 'double-float)))
      (dispatch-main ()
        (invoke ptr "setItemHeight:" :double height))
      (setf (slot-value combobox 'item-height) height))))

(defmethod initialize-instance :after ((combobox combobox) &key items)
  (with-slots (has-vertical-scroller-p
               item-height
               button-bordered-p
               intercell-spacing*)
      combobox
    (dispatch-main ()
      (setf (has-vertical-scroller-p combobox) has-vertical-scroller-p
            (item-height             combobox) item-height
            (button-bordered-p       combobox) button-bordered-p)
      (set-intercell-spacing combobox
                             (aref intercell-spacing* 0)
                             (aref intercell-spacing* 1))
      (dolist (item items)
        (add-item combobox item)))))

(defmethod item-list ((combobox combobox))
  (with-slots (item-list*) combobox
    (mapcar #'car item-list*)))

(defmethod add-item ((combobox combobox) value)
  (with-slots (item-list*) combobox
    (let ((display (princ-to-string value)))
      (with-ptr combobox ptr
        (dispatch-main ()
          (invoke ptr "addItemWithObjectValue:" :ns-string display)))
      (setf item-list* (append item-list* (list (cons value display)))))))

(defmethod add-nth-item ((combobox combobox) (nth integer) value)
  (with-slots (item-list*) combobox
    (let ((display (princ-to-string value)))
      (with-ptr combobox ptr
        (dispatch-main ()
          (invoke ptr
                  "insertItemWithObjectValue:atIndex:"
                  :ns-string display
                  :ns-int    nth)))
      (setf item-list* (append (subseq item-list* 0 nth)
                               (list (cons value display))
                               (subseq item-list* nth))))))

(defmethod item-position ((combobox combobox) item)
  (with-slots (item-list*) combobox
    (position item item-list* :key #'car :test #'equal)))

(defmethod remove-nth-item ((combobox combobox) (nth integer))
  (with-slots (item-list*) combobox
    (with-ptr combobox ptr
      (dispatch-main ()
        (invoke ptr "removeItemAtIndex:" :ns-int nth)))
    (setf item-list* (append (subseq item-list* 0 nth)
                             (subseq item-list* (1+ nth))))))

(defmethod value ((combobox combobox))
  (with-slots (item-list*) combobox
    (with-ptr combobox ptr
      (let ((nth (invoke ptr "indexOfSelectedItem" :ns-int)))
        (if (= nth -1)
            (invoke ptr "stringValue" :ns-string)
            (car (nth nth item-list*)))))))

;;;; combobox.lisp ends here
