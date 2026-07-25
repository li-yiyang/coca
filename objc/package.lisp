;;;; package.lisp --- Package definition of McCLIM-Coca.ObjC

(uiop:define-package #:coca.objc
  (:use :common-lisp :cffi)
  (:local-nicknames (:alx :alexandria)
                    (:m   :trivia))
  (:documentation "Coca.ObjC is a minimum ObjC runtime binding")
  ;; resources.lisp
  (:export
   #:*objc-libraries*
   #:*on-objc-initialization*
   #:sel
   #:objc-class
   #:coerce-to-selector
   #:coerce-to-objc-class
   #:define-objc-class
   #:define-objc-global-variable
   #:define-objc-method
   #:ensure-objc-initialized
   #:with-fp-traps-masked
   #:objc-symbol-value
   #:with-autorelease-pool)
  ;; typing.lisp
  (:export
   #:define-objc-typing)
  ;; invoke.lisp
  (:export
   #:invoke
   #:invoke-super
   #:self
   #:string-to-ns-string
   #:ns-string-to-string
   #:pathname-to-ns-url
   #:ns-array-to-list
   #:ns-mutable-dictionary
   #:ns-number
   #:alloc
   #:init
   #:description
   #:release
   #:retain
   #:autorelease)
  ;; block.lisp
  (:export
   #:define-objc-block))

(in-package :coca.objc)

;;;; utils

(defun symbol-concat (&rest things)
  (intern (with-output-to-string (sym)
            (dolist (thing things)
              (typecase thing
                (symbol    (write-string (string thing) sym))
                (string    (write-string thing sym))
                (character (write-char   thing sym))
                (t (format sym "~A" thing)))))))

;;;; package.lisp
