;;;; dialog-item.lisp

(in-package :coca.cocoa)

(defclass dialog-item (simple-view)
  ((dialog-item-text
    :initarg  :dialog-item-text
    :initform ""
    :accessor dialog-item-text)
   (dialog-item-handle
    :initarg  :dialog-item-text
    :initform nil
    :accessor dialog-item-handle)
   (dialog-item-enabled-p
    :initarg  :dialog-item-enabled-p
    :initform t
    :accessor dialog-item-enabled-p)
   (dialog-item-action-function
    :initarg  :dialog-item-action
    :initform nil
    :accessor dialog-item-action-function)))


;;;; button

(defclass button (dialog-item)
  ((objc-class
    :initform (coerce-to-objc-class "NSButton")
    :documentation "NSButton")))

;;;; dialog-item.lisp ends here
