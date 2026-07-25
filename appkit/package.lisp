;;;; package.lisp --- Package definition for Coca.AppKit

(uiop:define-package #:coca.appkit
  (:use :common-lisp :cffi :coca.objc :coca.app)
  (:local-nicknames (:alx :alexandria))
  (:documentation
   "Coca.AppKit is a collection of mixin classes for AppKit. ")
  ;; obj.lisp
  (:export
   #:obj
   #:obj-ptr)
  ;; titled.lisp
  (:export
   #:titled-mixin
   #:title)
  ;; framed.lisp
  (:export
   #:framed-mixin
   #:frame
   #:set-frame))

;;;; package.lisp ends here
