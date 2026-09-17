;;;; package.lisp --- Package definition for coca.app

(uiop:define-package #:coca.app
  (:use :common-lisp :cffi :coca.objc)
  (:local-nicknames (:alx :alexandria))
  (:documentation
   "Coca.App as simple NSApp management layer. ")
  ;; app.lisp
  (:export
   #:app
   #:define-on-coca-app-finish-run
   #:coca-app-run
   #:coca-app-terminate
   #:dispatch-main))

(in-package :coca.app)

(define-objc-library appkit
  (:darwin (:framework "AppKit")))

;;;; package.lisp ends here
