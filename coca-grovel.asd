;;;; coca-grovel.asd ---- ObjC File extension for CFFI-Grovel

(defsystem #:coca-grovel
  :author ("Dan Knapp <dankna@accela.net>" ; original author of cffi-grovel
           "凉凉")
  :license "LGPL"
  ;; ROADMAP:
  ;; + 0.0.1: :objc-file as replacement of :cffi-wrapper-file
  ;; + 0.0.2: lispy sugar to write the :objc-file
  :version "0.0.1"
  :depends-on (:cffi-grovel)
  :description "The CFFI Groveller extension for ObjC"
  :long-description
  "Coca/Grovel is an extension to CFFI-Grovel for generating ObjC grovel/wrapper file.

Use

    (:objc-file ...)

in ASDF component description will compiles and loads (like :cffi-wrapper-file)
the ObjC library into lisp environment.

Syntax:
+ (progn &rest forms)
+ (in-package package)
+ (objc &rest strings-of-objc-codes)
+ (cc-flags &rest flags)
+ (ld-flags &rest flags)
+ (pkg-config-cflags pkg)
+ (proclaim &rest proclamations)
+ (declaim  &rest declamations)
+ (define name &optional value)
+ (include &rest includes)
+ (import  &rest includes)

See more or define more syntax in cffi-grovel;syntax.lisp.

Dev Note:
this is basically a copy of cffi-grovel's `cffi::wrapper-file'."
  :serial t
  :pathname "grovel"
  :components
  ((:file "grovel")
   (:file "syntax")
   (:file "asdf")))

;;;; coca-grovel.asd ends here
