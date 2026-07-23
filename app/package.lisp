;;;; package.lisp --- Package definition for coca.app

(uiop:define-package #:coca.app
  (:use :common-lisp :cffi :coca.objc)
  (:local-nicknames (:alx :alexandria))
  ;; app.lisp
  (:export
   #:app
   #:coca-app-run
   #:coca-app-terminate)
  (:documentation
   "Coca.App as simple NSApp management layer. "))

(in-package :coca.app)

;;;; package.lisp ends here
