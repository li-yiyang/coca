;;;; package.lisp --- Package definition of Coca.Core-Graphics

(uiop:define-package #:coca.core-graphics
  (:use :common-lisp :coca.objc :cffi))

(in-package :coca.core-graphics)

(define-foreign-library core-graphics
  (:darwin (:framework "CoreGraphics")))
(load-foreign-library 'core-graphics)
(pushnew 'core-graphics *objc-libraries*)

;;;; package.lisp ends here
