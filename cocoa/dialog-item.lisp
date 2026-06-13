;;;; dialog-item.lisp

(in-package :coca.cocoa)


;;;; Dialog Item Protocol

(defgeneric dialog-items (view &optional item-class must-be-enabled)
  (:documentation
   "Return a list of the dialog items in VIEW.

Parameters:
+ VIEW: a `view'
+ ITEM-CLASS:
  If the value of ITEM-CLASS is specified and non-nil, then
  only dialog items matching ITEM-CLASS are returned.
  The default value is `nil'.
+ MUST-BE-ENABLED:
  Ff `t', then only dialog items that are enabled are returned.
  The default value is `nil'. "))

(defun make-dialog-item (class position size text &optional action &rest attributes)
  "Creates a dialog item using `make-instance'.

Parameters:
+ CLASS: the class of dialog item
+ POSITION: the position of the dialog item with respect to its container
+ SIZE: the size of the dialog item
+ TEXT: the text included within the dialog item
+ ACTION: the action associated within the dialog item
+ ATTRIBUTES: one or more attributes belonging to the dialog item.
  The number and nature of these depends on the type of dialog item.
"
  (assert (subtypep class 'dialog-item))
  (apply #'make-instance class
         :view-position      position
         :view-size          size
         :dialog-item-text   text
         :dialog-item-action action
         attributes))

(defgeneric dialog-item-action (item)
  (:documentation
   "Called whenever the user clicks a dialog item.
The method for `dialog-item' calls item's `dialog-item-action-function',
if it is not `nil'. Otherwise, it does nothing.

The `dialog-item-action' function is normally called when the mouse
button is released, not when it is pressed.

If an item is disabled, its action is not run.

Since `dialog-item-action' is usually called by `view-click-event-handler'
as a result of event processing, event processing is ordinarily
disabled while the `dialog-item-action' function is running.
This means that other dialog items cannot be selected during the
action. To avoid locking out other event processing, you can
use `eval-enqueue' to insert forms into the `read-evel-print' loop. "))

(defgeneric dialog-item-action-function (item)
  (:documentation
   "Returns the value set by the `:dialog-item-action'
initialization argument or the `set-dialog-item-action-function'
function.

Unless it is `nil', this function is called with a single argument, item,
by the `dialog-item-action' method for `dialog-item'.

This generic function is called by the `view-click-event-handler'
method for `dialog-item' when the user clicks a dialog item."))

(declaim (inline set-dialog-item-action-function))
(defun set-dialog-item-action-function (item new-function)
  "Sets the value of `dialog-item-action-function' for ITEM as NEW-FUNCTION.

Dev Note:
+ implement (setf dialog-item-action-function)"
  (setf (dialog-item-action-function item) new-function))

(defgeneric view-click-event-handler (item where)
  (:documentation
   "Called by the event system when the user clicks the ITEM.
The method for `dialog-item' calls ITEM's `dialog-item-action-function'
with ITEM as the single argument. If ITEM's `dialog-item-action-function'
is `nil', nothing is done.

Parameters:
+ ITEM: a dialog item
+ WHERE: a cursor position, it is ignored. "))

(defgeneric dialog-item-text (item)
  (:documentation
   "Returns the string of text associated with the ITEM. "))

(declaim (inline set-dialog-item-text))
(defun set-dialog-item-text (item text)
  "Sets the text associated with the dialog item to TEXT and returns TEXT.

The text of a dialog item has a different meaning for each class of
dialog item. It is the text of static-text and editable-dialog text
items. It is the label displayed inside buttons and to the right of
radio buttons and checkboxes.

If you prefer to put text in a different location, set the text to
the empty string and use a separate static-text item to place the
text where you would like it.

Dev Note:
+ implement method for (setf dialog-item-text)"
  (declare (type dialog-item item))
  (setf (dialog-item-text item) text))

(defgeneric dialog-item-enable (item)
  (:documentation
   "Enables the dialog item.
The item is not dimmed, and its action is run when the user clicks it.
The function returns `nil'. "))

(defgeneric dialog-item-disable (item)
  (:documentation
   "Disable the dialog item.
The dialog item is dimmed; clicks in the item are ignored,
and the action of the item is never run.
The function returns `nil'. "))

(defgeneric dialog-item-enabled-p (item)
  (:documentation
   "Returns `t' if the dialog item is enabled;
or `nil' if it is disabled. "))


;;;; dialog-item

(define-objc-class "CocaDialogItemController" "NSViewController")

(define-objc-method ("CocaDialogItemController" "handleCocaDialogTarget:"
                     :encoding "v@:@")
                    :void ((item* :object))
  (alx:when-let* ((item (find-objc-obj item*))
                  (fn   (dialog-item-action-function item)))
    (funcall fn item)))

(defclass dialog-item (simple-view)
  ((dialog-item-text
    :initarg  :dialog-item-text
    :initform ""
    :reader   dialog-item-text)
   (dialog-item-handle
    :initarg  :dialog-item-text
    :initform nil
    :reader   dialog-item-handle)
   (dialog-item-enabled-p
    :initarg  :dialog-item-enabled-p
    :initform t
    :reader   dialog-item-enabled-p)
   (dialog-item-action-function
    :initarg  :dialog-item-action
    :initform nil
    :accessor dialog-item-action-function)
   (%target
    :initform (init (alloc "CocaDialogItemController"))))
  (:documentation
   "The class `dialog-item' provides the basic functionality
shared by all dialog items. It is built on `simple-view'. "))

(defmethod initialize-instance :after ((item dialog-item)
                                       &key
                                         dialog-item-text
                                         ;; dialog-item-handle
                                         dialog-item-enabled-p
                                         dialog-item-action-function)
  ;; (when dialog-item-text
  ;;   (setf (dialog-item-text item) dialog-item-text))
  ;; (when dialog-item-enabled-p
  ;;   (setf (dialog-item-enabled-p item) dialog-item-enabled-p))
  (with-ptr item
    (let ((target* (slot-value item '%target)))
      (dispatch-main ()
        (invoke ptr "setTarget:" :object target*)
        (invoke ptr "setAction:" :sel "handleCocaDialogTarget:")))))

(defmethod dealloc ((item dialog-item))
  (release (slot-value item '%target))
  (call-next-method))


;;;; button

(defgeneric press-button (button)
  (:documentation
   ""))

(defclass button (dialog-item)
  ((objc-class
    :initform (coerce-to-objc-class "NSButton")
    :documentation "NSButton")))

(defmethod (setf dialog-item-text) ((text string) (button button))
  (with-ptr button
    (dispatch-main () (invoke ptr "setTitle:" :ns-string text)))
  (setf (slot-value button 'dialog-item-text) text))

;;;; dialog-item.lisp ends here
