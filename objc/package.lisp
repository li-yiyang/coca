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
   #:doc-objc-class

   ;; ObjC sel
   #:sel
   #:coerce-to-selector

   ;; ObjC struct
   #:define-objc-struct

   ;; ObjC enum
   #:define-objc-enum

   ;; ObjC method
   ;; #:define-objc-method
   ;; #:define-objc-protocol
   #:invoke
   #:can-invoke-p))

(in-package :coca.objc)

;;;; package.lisp ends here
