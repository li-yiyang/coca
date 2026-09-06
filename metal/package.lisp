;;;; package.lisp --- Package definition of Coca.Metal

(uiop:define-package #:coca.metal
  (:use :common-lisp :coca.objc :cffi)
  (:local-nicknames (:alx  :alexandria)
                    (:m    :trivia)
                    (:objc :coca.objc))
  ;; device
  (:export
   #:device
   #:make-device
   #:default-device
   #:device-name
   #:device-id
   #:device-architecture
   #:device-supports-family-p
   #:device-current-allocated-size)
  ;; resources
  (:export
   #:msl-dtype
   #:msl-dtype-size
   #:make-buffer)
  ;; pipeline
  (:export
   #:library
   #:library-name
   #:library-type
   #:library-device
   #:make-library-with-source
   #:make-library-with-file
   #:library-compute-pipeline)
  ;; command
  (:export
   #:command-queue
   #:make-command-queue
   #:default-command-queue
   #:command-buffer
   #:encoder))

(in-package :coca.metal)

(define-foreign-library metal
  (:darwin (:framework "Metal")))
(load-foreign-library 'metal)
(pushnew 'metal *objc-libraries*)

;;;; package.lisp ends here
