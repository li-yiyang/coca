;;;; package.lisp --- Package definition of McCLIM-Coca.ObjC

(uiop:define-package #:coca.objc
  (:use :common-lisp :cffi)
  (:local-nicknames (:alx :alexandria)
                    (:m   :trivia))
  (:documentation "Coca.ObjC is a minimum ObjC runtime binding")
  ;; resources.lisp
  (:export
   #:define-objc-library
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
   #:define-objc-typing
   #:define-objc-mask
   #:define-objc-enum)
  ;; invoke.lisp
  (:export
   #:invoke
   #:invoke-super
   #:self
   #:string-to-ns-string
   #:ns-string-to-string
   #:pathname-to-ns-url
   #:ns-url-to-pathname
   #:ns-array-to-list
   #:ns-mutable-dictionary
   #:get-ns-dictionary
   #:ns-number
   #:ns-number-value
   #:alloc
   #:init
   #:description
   #:release
   #:retain
   #:autorelease)
  ;; url.lisp
  (:export
   #:url-to-ns-url
   #:ns-url-to-pathname-or-url)
  ;; osx-version.lisp
  (:export
   #:osx-version
   #:osx-version>=)
  ;; block.lisp
  (:export
   #:define-objc-block)
  ;; runtime-inspect.lisp
  (:export
   #:objc-class-list
   #:objc-class-superclass
   #:objc-class-subclasses
   #:objc-class-subclassp
   #:decode-objc-type-encoding
   #:encode-objc-type-encoding
   #:objc-class-instance-method-encoding
   #:objc-class-class-method-encoding))

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
