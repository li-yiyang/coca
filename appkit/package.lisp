;;;; package.lisp --- Package definition for Coca.AppKit

(uiop:define-package #:coca.appkit
  (:use :common-lisp :cffi :coca.objc :coca.app)
  (:local-nicknames (:alx  :alexandria)
                    (:m    :trivia)
                    (:objc :coca.objc))
  (:documentation
   "Coca.AppKit is a collection of mixin classes for AppKit. ")
  ;; obj.lisp
  (:export
   #:obj
   #:obj-ptr
   #:find-obj-mixin
   #:find-obj)
  ;; hierarchy.lisp
  (:export
   #:parent
   #:children
   #:add-child
   #:remove-child
   #:remove-from-parent)
  ;; subview.lisp
  (:export
   #:base-view
   #:subview-mixin)
  ;; named.lisp
  (:export
   #:name
   #:named-mixin)
  ;; visible.lisp
  (:export
   #:visible
   #:visible-mixin
   #:hidden-mixin
   #:show
   #:hide)
  ;; titled.lisp
  (:export
   #:titled-mixin
   #:title
   #:alternate-titled-mixin
   #:alternate-title)
  ;; framed.lisp
  (:export
   #:framed-mixin
   #:frame
   #:visible-frame
   #:set-frame
   #:width
   #:height
   #:size
   #:set-size
   #:origin
   #:set-origin
   #:parent-frame
   #:parent-size
   #:parent-width
   #:parent-height
   #:parent-origin
   #:location
   #:set-location
   #:default-frame
   #:static-framed-mixin
   #:minmax-framed-mixin
   #:min-width
   #:max-width
   #:min-height
   #:max-height)
  ;; menu.lisp
  (:export
   #:menu)
  ;; screen.lisp
  (:export
   #:screen
   #:screen-dpi
   #:screen-dpi-x
   #:screen-dpi-y
   #:screen-backing-scale
   #:main-screen
   #:screen-list)
  ;; window.lisp
  (:export
   #:window
   #:screen
   #:window-style)
  ;; modal.lisp
  (:export
   #:alert
   #:alert-style)
  ;; target.lisp
  (:export
   #:target-mixin
   #:target
   #:action
   #:enabledp)
  ;; views.lisp
  (:export
   #:state-mixin
   #:state
   #:bordered-mixin
   #:borderedp
   #:base-button
   #:button
   #:checkbox
   #:radio-button
   #:help-button
   #:bezel-style
   #:button-type))

(in-package :coca.appkit)

;;;; package.lisp ends here
