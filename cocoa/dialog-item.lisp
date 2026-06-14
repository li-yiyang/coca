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

(declaim (inline dialog-item-enable))
(defun dialog-item-enable (item)
  "Enables the dialog item.
The item is not dimmed, and its action is run when the user clicks it.
The function returns `nil'. "
  (setf (dialog-item-enabled-p item) t)
  nil)

(declaim (inline dialog-item-disable))
(defun dialog-item-disable (item)
  "Disable the dialog item.
The dialog item is dimmed; clicks in the item are ignored,
and the action of the item is never run.
The function returns `nil'. "
  (setf (dialog-item-enabled-p item) nil)
  nil)

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
  ((dialog-item-handle
    :initarg  :dialog-item-handle
    :initform (init (alloc "CocaDialogItemController"))
    :type     (or null foreign-pointer)
    :reader   dialog-item-handle)
   (dialog-item-enabled-p
    :initarg  :dialog-item-enabled-p
    :initform t
    :type     boolean
    :reader   dialog-item-enabled-p)
   (dialog-item-action-function
    :initarg  :dialog-item-action
    :initform nil
    :accessor dialog-item-action-function))
  (:documentation
   "The class `dialog-item' provides the basic functionality
shared by all dialog items. It is built on `simple-view'. "))

(defmethod (setf dialog-item-handle) (handle (dialog dialog-item))
  (declare (type foreign-pointer handle))
  (with-ptr dialog
    (when (slot-value dialog 'dialog-item-handle)
      (remhash (pointer-address (slot-value dialog 'dialog-item-handle)) *objc-objects*))
    (dispatch-main ()
      (invoke ptr "setTarget:" :object handle)
      (invoke ptr "setAction:" :sel "handleCocaDialogTarget:"))
    (regist-objc-obj dialog handle)
    (setf (slot-value dialog 'dialog-item-handle) handle)))

(defmethod initialize-instance :after ((item dialog-item)
                                       &key
                                         dialog-item-action-function
                                         dialog-item-text)
  (setf (dialog-item-text      item) (or dialog-item-text
                                         (dialog-item-text item)))
  (setf (dialog-item-enabled-p item) (dialog-item-enabled-p item))
  
  ;; patch of (setf dialog-item-handle) to skip `remhash'
  (let ((handle (dialog-item-handle item)))
    (setf (slot-value item 'dialog-item-handle) nil)
    (setf (dialog-item-handle item) handle)))

(defmethod dealloc ((item dialog-item))
  (let ((handle (dialog-item-handle item)))
    (remhash (pointer-address handle) *objc-objects*)
    (release handle))
  (call-next-method))

;; dialog-item-enabled-p

(defmethod (setf dialog-item-enabled-p) (value (dialog dialog-item))
  (with-ptr dialog
    (cond (value
           (dispatch-main () (invoke ptr "setEnabled:" :bool t))
           (setf (slot-value dialog 'dialog-item-enabled-p) t))
          (t
           (dispatch-main () (invoke ptr "setEnabled:" :bool nil))
           (setf (slot-value dialog 'dialog-item-enabled-p) nil)))))


;;;; dialog-item-titled-mixin

(defmethod dialog-item-text ((dialog dialog-item))
  "")

(defmethod (setf dialog-item-text) (text (dialog dialog-item))
  (declare (ignore text dialog)))

(defclass dialog-item-titled-mixin ()
  ((dialog-item-text
    :initarg  :dialog-item-text
    :initform ""
    :type     string
    :reader   dialog-item-text))
  (:documentation
   "For NSControl with `title' SEL method. "))

(defmethod (setf dialog-item-text)
    ((text string) (dialog dialog-item-titled-mixin))
  (with-ptr dialog
    (dispatch-main () (invoke ptr "setTitle:" :ns-string text))
    (setf (slot-value dialog 'dialog-item-text) text)))

(defclass dialog-item-string-value-mixin () ()
  (:documentation
   "For NSControl with `stringValue' SEL method. "))

(defmethod dialog-item-text ((dialog dialog-item-string-value-mixin))
  (with-ptr dialog
    (invoke ptr "stringValue" :ns-string)))

(defmethod (setf dialog-item-text)
    ((text string) (dialog dialog-item-string-value-mixin))
  (with-ptr dialog
    (dispatch-main () (invoke ptr "setStringValue:" :ns-string text))))

(defclass dialog-item-string-mixin () ()
  (:documentation
   "For NSView with `string' SEL method. "))

(defmethod dialog-item-text ((dialog dialog-item-string-mixin))
  (with-ptr dialog (invoke ptr "string" :ns-string)))

(defmethod (setf dialog-item-text)
    ((text string) (dialog dialog-item-string-mixin))
  (with-ptr dialog
    (dispatch-main () (invoke ptr "setString:" :ns-string text))))


;;;; button

(defgeneric press-button (button)
  (:documentation
   ""))

(defclass button-dialog-item (dialog-item
                              dialog-item-titled-mixin)
  ((objc-class
    :initform (coerce-to-objc-class "NSButton")
    :documentation "NSButton"))
  (:default-initargs
   :dialog-item-text "Button"))


;;;; static-text-dialog-item

(defclass static-text-dialog-item (dialog-item
                                   dialog-item-string-value-mixin)
  ((objc-class
    :initform (coerce-to-objc-class "NSTextField")))
  (:default-initargs
   :dialog-item-text "")
  (:documentation
   "NSTextField as static-text-dialog-item. "))

(defmethod initialize-instance :after ((dialog static-text-dialog-item) &key)
  (with-ptr dialog
    (dispatch-main ()
      (invoke ptr "setEditable:"        :bool nil)
      (invoke ptr "setSelectable:"      :bool nil)
      (invoke ptr "setBordered:"        :bool nil)
      (invoke ptr "setDrawsBackground:" :bool nil))))


;;;; editable-text-dialog-item

;; TODO: implement `cluffer-buffer-mixin'?
(defclass editable-text-dialog-item (ns-text-view-dialog-item
                                     dialog-item-string-mixin)
  ((objc-class
    :initform (coerce-to-objc-class "NSTextView")))
  (:default-initargs
   :dialog-item-text "")
  (:documentation
   "NSTextView as editable-text-dialog-item. "))

(defmethod initialize-instance :after ((dialog editable-text-dialog-item) &key)
  (with-ptr dialog
    (dispatch-main ()
      (invoke ptr "setEditable:" :bool t))))

;;;; dialog-item.lisp ends here
