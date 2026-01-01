;;;; syntax.lisp --- Syntax of ObjC File

(in-package :cffi-grovel)

(define-objc-syntax progn (&rest forms)
  "PROGN names a list of additional ObjC Grovel Expressions.

Could be used as:

    #+darwin
    (progn (balabala)
           (balabala))

    #+gnustep
    (progn (balabala)
           (balabala))

to support features switches. "
  (dolist (form forms)
    (process-wrapper-form out form)))

(define-objc-syntax in-package (name)
  "Declare the ObjC File should be interned to lisp package of NAME.

Note: the package of NAME should be defined. Otherwise,
an error would be throw. "
  (assert (find-package name) (name)
          "Wrapper file specified (in-package ~s)~%~
           however that does not name a known package."
          name)
  (setq *package* (find-package name))
  (push `(in-package ,name) *lisp-forms*))

(define-objc-syntax objc (&rest strings)
  "Insert ObjC code directly.

Example:

    (objc \"
#import <Foundation/Foundation.h>

void objc_function_haha () {
 // ...
}
      \")

"
  (dolist (string strings)
    (write-line string out)))

(define-objc-syntax cc-flags (&rest flags)
  "Add compiler flags"
  (appendf *cc-flags* (parse-command-flags-list flags)))

(define-objc-syntax ld-flags (&rest flags)
  "Add linker flags"
  (appendf *ld-dll-flags* (parse-command-flags-list flags)))

(define-objc-syntax pkg-config-cflags (pkg &key optional)
  "Add compiler flags using pkg-config --cflags

Parameters:
+ PKG: package used in pkg-config PKG --cflags
+ OPTIONAL: if non-nil, the PKG is ignorable if pkg-config cannot find it"
  (let ((output-stream (make-string-output-stream))
        (program+args  (list "pkg-config" pkg "--cflags")))
    (format *debug-io* "~&;~{ ~a~}~%" program+args)
    (handler-case
        (progn
          (run-program program+args
                       :output (make-broadcast-stream output-stream *debug-io*)
                       :error-output *debug-io*)
          (appendf *cc-flags*
                   (parse-command-flags (get-output-stream-string output-stream))))
      (error (e)
        (let ((message (format nil "~a~&~%~a~&"
                               e (get-output-stream-string output-stream))))
          (cond (optional
                 (format *debug-io* "~&; ERROR: ~a" message)
                 (format *debug-io* "~&~%; Attempting to continue anyway.~%"))
                (t
                 (grovel-error "~a" message))))))))

(define-objc-syntax proclaim (&rest proclamations)
  (push `(proclaim ,@proclamations) *lisp-forms*))

(define-objc-syntax declaim (&rest declamations)
  (push `(declaim ,@declamations) *lisp-forms*))

(define-objc-syntax define (name &optional value)
  (format out "#define ~A~@[ ~A~]~%" name value))

(define-objc-syntax include (&rest includes)
  (format out "~{#include <~A>~%~}" includes))

(define-objc-syntax import (&rest includes)
  (format out "~{#import <~A>~%~}" includes))

;;;; TODO:
;; ObjC specific wrappers
;;
;; + (define-objc-class  ...)
;; + (define-objc-method ...)
;; + (define-objc-struct ...)
;;

;;;; syntax.lisp ends here
