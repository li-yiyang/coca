;;;; package.lisp --- Package definition of coca.objc

(uiop:define-package #:coca.objc
  (:use :cl :cffi)
  (:export
   ;; CFFI
   #:objc-object-pointer
   #:standard-objc-object

   ;; ObjC class
   #:objc-class
   #:coerce-to-objc-class

   ;; ObjC sel
   #:sel
   #:coerce-to-selector

   ;; ObjC method
   #:define-objc-method
   #:define-objc-protocol
   #:define-objc-struct
   #:description
   #:ensure-objc-initialized
   #:invoke
   #:alloc-init-object

   ;; ObjC object

   ))

(in-package :coca.objc)

;;;; package.lisp ends here
