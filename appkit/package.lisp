;;;; package.lisp --- Package definition for Coca.AppKit

(uiop:define-package #:coca.appkit
  (:use :common-lisp :cffi :coca.objc :coca.app)
  (:local-nicknames (:alx :alexandria))
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
   #:children)
  ;; named.lisp
  (:export
   #:name
   #:named-mixin)
  ;; titled.lisp
  (:export
   #:titled-mixin
   #:title)
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
   #:parent-origin
   #:location
   #:set-location
   #:static-framed-mixin
   #:minmax-framed-mixin
   #:min-width
   #:max-width
   #:min-height
   #:max-height)
  ;; screen.lisp
  (:export
   #:screen
   #:screen-dpi
   #:screen-dpi-x
   #:screen-dpi-y
   #:screen-backing-scale
   #:main-screen
   #:screen-list))

;;;; package.lisp ends here
