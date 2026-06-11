;;;; package.lisp --- Package definition of McCLIM-Coca.Cocoa

(uiop:define-package #:coca.cocoa
  (:use :common-lisp :cffi :coca.objc)
  (:local-nicknames (:alx :alexandria)
                    (:tgs :trivial-gray-streams))
  ;; obj.lisp
  (:export
   #:objc-obj
   #:alloc-init
   #:dealloc
   #:find-objc-obj
   #:regist-objc-obj
   #:define-objc-mask
   #:define-objc-global-variable)
  ;; app.lisp
  (:export
   #:application
   #:*application*
   #:coca-app-run
   #:coca-app-terminate)
  ;; block.lisp
  (:export
   #:define-objc-block)
  ;; view.lisp
  (:export
   #:simple-view
   #:view-container
   #:set-view-container
   #:view-position
   #:set-view-position
   #:view-size
   #:set-view-size
   #:view-nickname
   #:view-default-size
   #:view-default-position
   #:view-get
   #:view-put
   #:view-remprop
   #:view-scroll-position
   #:view-subviews)
  ;; screen.lisp
  (:export)
  ;; window.lisp
  (:export)
  (:documentation
   "Coca.Cocoa as layer of Cocoa AppKit and Foundation. "))

;;;; package.lisp ends here
