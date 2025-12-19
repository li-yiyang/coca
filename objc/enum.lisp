;;;; enum.lisp --- Wrapper of ObjC enum

(in-package :coca.objc)

(defmacro define-objc-enum (name &body bindings)
  "Define ObjC enum of NAME with BINDINGS.

Syntax:

    (define-objc-enum NAME
      [docstring]
      (KEYWORD VAL [docstring]))

"
  (let ((docstring (pop bindings)))
    (unless (stringp docstring)
      (push docstring bindings)
      (setf docstring (format nil "ObjC enum of ~A. " name)))
    `(progn
       (defun ,name (&rest flags)
         ,(with-output-to-string (doc)
            (write-line docstring doc)
            (loop :for (keyword val . docs) :in bindings :do
              (format doc "+ ~S (~D)~%" keyword (eval val))
              (format doc "~{  ~A~%~}" docs))
            (format doc "
Dev Note:
the `~S' also provides a compiler macro function to literally
compile enum expression as literal values. "
                    name))
         (the unsigned-byte
           (reduce (lambda (res flag)
                     (logior (the fixnum res)
                             (the fixnum
                               (ecase flag
                                 ,@(loop :for (keyword val) :in bindings
                                         :collect `(,(the keyword keyword)
                                                    ,(eval val)))))))
                   flags
                   :initial-value (the fixnum 0))))
       (define-compiler-macro ,name (&whole expr &rest flags)
         (if (every #'keywordp flags)
             (eval expr)
             expr)))))

;;;; enum.lisp ends here
