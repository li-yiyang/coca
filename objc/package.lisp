;;;; package.lisp --- Package definition of coca.objc

(uiop:define-package #:coca.objc
  (:use :cl :cffi)
  (:export
   ;; Utils
   #:objc-error

   ;; CFFI
   #:objc-object-pointer
   #:standard-objc-object

   ;; ObjC class
   #:objc-class
   #:coerce-to-objc-class
   #:doc-objc-class

   ;; ObjC sel
   #:sel
   #:coerce-to-selector

   ;; ObjC encoding
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
   #:declare-objc-method
   #:send
   #:invoke
   #:can-invoke-p))

(in-package :coca.objc)

;;;; package.lisp ends here
