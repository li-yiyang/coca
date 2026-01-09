;;;; package.lisp --- Package definition of coca.objc

(uiop:define-package #:coca.objc
  (:use :cl :cffi)
  (:export
   ;; Utils
   #:objc-error
   #:as-boolean

   ;; CFFI
   #:objc-object-pointer
   #:standard-objc-object

   ;; ObjC class
   #:objc-class
   #:coerce-to-objc-class
   #:define-objc-class

   ;; ObjC sel
   #:sel
   #:coerce-to-selector

   ;; ObjC encoding
   #:objc-basic-encoding
   #:objc-encoding
   #:define-objc-typedef

   ;; ObjC struct
   #:define-objc-struct

   ;; ObjC enum
   #:define-objc-enum

   ;; ObjC const
   #:define-objc-const

   ;; ObjC method
   ;; #:define-objc-method
   ;; #:define-objc-protocol
   #:self
   #:super
   #:define-objc-method
   #:send
   #:invoke
   #:can-invoke-p
   #:coca-init
   #:define-coca-init))

(in-package :coca.objc)

;;;; package.lisp ends here
