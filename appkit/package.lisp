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
  ;; titled.lisp
  (:export
   #:titled-mixin
   #:title)
  ;; hierarchy.lisp
  (:export
   #:parent
   #:children)
  ;; framed.lisp
  (:export
   #:framed-mixin
   #:frame
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
   #:minmax-framed-mixin
   #:min-width
   #:max-width
   #:min-height
   #:max-height))

;;;; package.lisp ends here
