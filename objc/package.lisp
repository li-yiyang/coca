;;;; package.lisp --- Package definition of McCLIM-Coca.ObjC

(uiop:define-package #:coca.objc
  (:use :common-lisp :cffi)
  (:local-nicknames (:alx :alexandria)
                    (:m   :trivia))
  (:documentation "Coca.ObjC is a minimum ObjC runtime binding")
  ;; resources.lisp
  (:export
   #:sel
   #:objc-class
   #:coerce-to-selector
   #:coerce-to-objc-class
   #:define-objc-class
   #:define-objc-method
   #:ensure-objc-initialized
   #:with-fp-traps-masked
   #:objc-symbol-value
   #:with-autorelease-pool)
  ;; typing.lisp
  (:export
   #:define-objc-typing
   #:ns-rect
   #:ns-rect-origin
   #:ns-rect-size
   #:ns-rect-x
   #:ns-rect-y
   #:ns-rect-w
   #:ns-rect-h
   #:ns-size
   #:ns-size-w
   #:ns-size-h
   #:ns-point
   #:ns-point-x
   #:ns-point-y)
  ;; invoke.lisp
  (:export
   #:invoke
   #:invoke-super
   #:self
   #:string-to-ns-string
   #:ns-string-to-string
   #:pathname-to-ns-url
   #:ns-array-to-list
   #:ns-mutable-dictionary
   #:ns-number
   #:alloc
   #:init
   #:description
   #:release
   #:retain
   #:autorelease))

(in-package :coca.objc)

;;;; package.lisp
