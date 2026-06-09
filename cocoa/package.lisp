;;;; package.lisp --- Package definition of McCLIM-Coca.Cocoa

(uiop:define-package #:coca.cocoa
  (:use :common-lisp :cffi :coca.objc)
  (:local-nicknames (:alx :alexandria)
                    (:tgs :trivial-gray-streams))
  ;; app.lisp
  (:export
   #:app
   #:coca-app-run
   #:coca-app-terminate)
  ;; block.lisp
  (:export
   #:define-objc-block)
  (:documentation
   "Coca.Cocoa as layer of Cocoa AppKit and Foundation. "))

;;;; package.lisp ends here
