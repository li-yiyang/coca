;;;; combobox.lisp --- Wrapper of NSComboBox

(in-package :coca.appkit)

(define-coca-base-view (combobox "NSComboBox"
                                 target-mixin
                                 font-mixin)
  (:direct-slots
   (%item-list
    :initform ()
    :type     list
    :documentation
    "An alist of (VALUE . DISPLAY-STRING). "))
  (:objc-property
   (intercell-spacing
    :ns-point
    (:intercell-spacing #(3d0 2d0) "setIntercellSpacing:"))
   (has-vertical-scroller-p
    :bool
    (:has-vertical-scroller t "setHasVerticalScroller:"))
   (button-bordered-p
    :bool
    (:button-bordered t "setButtonBordered:"))
   (item-height
    :double
    (:item-height 16.0d0 "setItemHeight:")))
  (:after-initialize (items)
   (dolist (item items)
     (add-item combobox item)))
  (:documentation
   "A combobox.

A NSComboBox is a view that displays a list of values in
a pop-up menu where the user selects a value or types in
a custom value.

Initialize Parameters:
+ ITEMS: a list of initial item values

+ INTERCELL-SPACING
  + (WIDTH HEIGHT)
  + #(WIDTH HEIGHT)

+ HAS-VERTICAL-SCROLLER:
  if or not has vertical scroller for item drop menu

+ BUTTON-BORDERED:
  if or not the drop down button is bordered

+ ITEM-HEIGHT:
  height of items
"))


;;;; items

(defmethod item-list ((combobox combobox))
  (with-slots (%item-list) combobox
    (mapcar #'car %item-list)))

(defmethod add-item ((combobox combobox) value)
  (with-slots (%item-list) combobox
    (let ((display (princ-to-string value)))
      (with-ptr combobox ptr
        (dispatch-main ()
          (invoke ptr "addItemWithObjectValue:" :ns-string display)))
      (setf %item-list (append %item-list (list (cons value display)))))))

(defmethod add-nth-item ((combobox combobox) (nth integer) value)
  (with-slots (%item-list) combobox
    (let ((display (princ-to-string value)))
      (with-ptr combobox ptr
        (dispatch-main ()
          (invoke ptr
                  "insertItemWithObjectValue:atIndex:"
                  :ns-string display
                  :ns-int    nth)))
      (setf %item-list (append (subseq %item-list 0 nth)
                               (list (cons value display))
                               (subseq %item-list nth))))))

(defmethod item-position ((combobox combobox) item)
  (with-slots (%item-list) combobox
    (position item %item-list :key #'car :test #'equal)))

(defmethod remove-nth-item ((combobox combobox) (nth integer))
  (with-slots (%item-list) combobox
    (with-ptr combobox ptr
      (dispatch-main ()
        (invoke ptr "removeItemAtIndex:" :ns-int nth)))
    (setf %item-list (append (subseq %item-list 0 nth)
                             (subseq %item-list (1+ nth))))))


;;;; value

;; TODO: set the value of combobox

(defmethod value ((combobox combobox))
  (with-slots (%item-list) combobox
    (with-ptr combobox ptr
      (let ((nth (invoke ptr "indexOfSelectedItem" :ns-int)))
        (if (= nth -1)
            (invoke ptr "stringValue" :ns-string)
            (car (nth nth %item-list)))))))

;;;; combobox.lisp ends here
