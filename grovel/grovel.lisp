;;;; grovel.lisp --- ObjC-File extension for the CFFI-Grovel

(in-package :cffi-grovel)

(defun make-objc-file-name (output-defaults &optional suffix)
  "ObjC file should have extension name of `.m'. "
  (make-pathname :type "m"
                 :name (strcat (pathname-name output-defaults) suffix)
                 :defaults output-defaults))

(defgeneric %process-objc-form (type out arguments)
  (:documentation
   "Process ObjC File grovel expression.

DO NOT Manually `defmethod' of `cffi-grovel::%process-objc-form',
use `cffi-grovel::define-objc-syntax' instead.

See coca/grovel;syntax.lisp for implementations and examples. ")
  (:method (name out arguments)
    (declare (ignore out arguments))
    (grovel-error "Unknown ObjC File Grovel syntax: ~S. " name)))

(defun process-objc-form (out form)
  (%process-objc-form (form-kind form) out (cdr form)))

(defun generate-objc-lib-file (input-file output-defaults)
  (let ((*lisp-forms* nil)
        (objc-file  (make-objc-file-name output-defaults "__wrapper")))
    (with-open-file (out objc-file :direction :output :if-exists :supersede)
      (with-open-file (in input-file :direction :input)
        (write-string *header* out)
        (loop :for form := (read in nil nil) :while form
              :do (process-objc-form out form))))
    (values objc-file (nreverse *lisp-forms*))))

(defun process-objc-file (input-file
                          &key
                            (output-defaults (make-pathname :defaults input-file
                                                            :type     "processed"))
                            lib-soname)
  (with-standard-io-syntax
    (multiple-value-bind (c-file lisp-forms)
        (generate-objc-lib-file input-file output-defaults)
      (let ((lib-file (make-so-file-name (make-soname lib-soname output-defaults)))
            (o-file   (make-o-file-name output-defaults "__wrapper")))
        (cc-compile o-file (list (cc-include-grovel-argument) c-file))
        (link-shared-library lib-file (list o-file))
        ;; FIXME: hardcoded library path.
        (values (generate-bindings-file lib-file lib-soname lisp-forms output-defaults)
                lib-file)))))

(defmacro define-objc-syntax (name lambda-list &body body)
  "Define ObjC File Grovel syntax.

Syntax:

    (define-objc-syntax NAME LAMBDA-LIST
      [DOCSTRING]
      &BODY)

Within BODY:
+ OUT is a lexically binded variable as the output ObjC file stream,
  write to it as ObjC code direct output
+ *CC-FLAGS*: compile flags
+ *LD-DLL-FLAGS*: ld flags
+ *LISP-FORMS*: use as lisp codes to be evaluated"
  (with-unique-names (name-var args)
    (let ((docstring (pop body)))
      (unless (stringp docstring)
        (push docstring body)
        (setf docstring nil))
      `(defmethod %process-objc-form ((,name-var (eql ',name)) out ,args)
         ,@(when docstring (list docstring))
         (declare (ignorable out))
         (destructuring-bind ,lambda-list ,args
           ,@body)))))

;;;; grovel.lisp ends here
