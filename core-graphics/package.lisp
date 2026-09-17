;;;; package.lisp --- Package definition of Coca.Core-Graphics

(uiop:define-package #:coca.core-graphics
  (:use :common-lisp :coca.objc :cffi))

(in-package :coca.core-graphics)

(define-objc-library core-graphics
  (:darwin (:framework "CoreGraphics")))

;;;; package.lisp ends here
