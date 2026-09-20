;;;; package.lisp --- Package definition for Coca.AppKit

(uiop:define-package #:coca.appkit
  (:use :common-lisp :cffi :coca.objc :coca.app)
  (:local-nicknames (:alx  :alexandria)
                    (:m    :trivia)
                    (:objc :coca.objc)
                    (:app  :coca.app))
  (:documentation
   "Coca.AppKit is a collection of mixin classes for AppKit. ")
  ;; obj.lisp
  (:export
   #:obj
   #:obj-ptr
   #:objc-ptr
   #:with-ptr
   #:with-ptrs
   #:find-obj-mixin
   #:find-obj
   #:appkit-condition
   #:already-destroyed
   #:owned-mixin
   #:destroy)
  ;; font.lisp
  (:export
   #:font
   #:font-name
   #:font-family
   #:font-display-name
   #:font-property
   #:font-family-list
   #:load-font-file
   #:make-font)
  ;; hierarchy.lisp
  (:export
   #:parent
   #:children
   #:add-child
   #:remove-child
   #:remove-from-parent
   #:item-list
   #:item-list-length
   #:add-item
   #:add-nth-item
   #:nth-item
   #:item-position
   #:remove-item
   #:remove-nth-item)
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
   #:menu-element
   #:menu
   #:menu-item
   #:menu-separator
   #:main-menu-p
   #:main-menu-mixin
   #:set-main-menu)
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
   #:window-list
   #:window
   #:screen
   #:window-style
   #:window-select
   #:window-select-event-handler
   #:window-close
   #:window-close-event-handler)
  ;; modal.lisp
  (:export
   #:alert)
  ;; target.lisp
  (:export
   #:target-mixin
   #:target
   #:action
   #:enabledp)
  ;; views/utils.lisp
  (:export
   #:state-mixin
   #:state
   #:bordered-mixin
   #:borderedp
   #:value
   #:string-value-mixin
   #:double-value-mixin)
  ;; views/button.lisp
  (:export
   #:base-button
   #:button
   #:checkbox
   #:radio-button
   #:help-button
   #:bezel-style
   #:button-type)
  ;; views/text-field.lisp
  (:export
   #:base-text-field
   #:text-alignment
   #:editable
   #:selectable
   #:draws-background-p
   #:label
   #:placeholder-mixin
   #:placeholder
   #:text-input)
  ;; views/combobox.lisp
  (:export
   #:combobox
   #:has-vertical-scroller-p
   #:intercell-spacing
   #:set-intercell-spacing
   #:button-bordered-p
   #:item-height))

(in-package :coca.appkit)

;;;; package.lisp ends here
