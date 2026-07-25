;;;; package.lisp --- Package definition for coca.app

(uiop:define-package #:coca.app
  (:use :common-lisp :cffi :coca.objc)
  (:local-nicknames (:alx :alexandria))
  (:documentation
   "Coca.App as simple NSApp management layer. ")
  ;; app.lisp
  (:export
   #:app
   #:coca-app-run
   #:coca-app-terminate
   #:dispatch-main))

(in-package :coca.app)

(define-foreign-library appkit
  (:darwin (:framework "AppKit")))
(load-foreign-library 'appkit)
(pushnew 'appkit *objc-libraries*)

;;;; package.lisp ends here
