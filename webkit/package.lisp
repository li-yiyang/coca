;;;; package.lisp

(uiop:define-package #:coca.webkit
  (:use :common-lisp :coca.objc :coca.app :coca.appkit)
  (:local-nicknames (:alx :alexandria))
  (:export
   #:webview
   ;; TODO: configuration abstraction in lisp
   #:configuration
   #:inspectable
   #:loadingp
   #:estimated-progress
   #:url
   #:user-agent
   #:has-only-secure-content-p
   #:page-zoom
   #:allow-magnification
   #:magnification
   #:navigate
   #:go-back
   #:go-forward
   #:reload
   #:reload-from-origin
   #:start-navigation-handler
   #:finish-navigation-handler))

(in-package :coca.webkit)

(define-objc-library webkit
  (:darwin (:framework "WebKit")))

;;;; package.lisp ends here
