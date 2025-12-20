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
       (deftype ,name ()
         ,(with-output-to-string (doc)
            (write-line docstring doc)
            (format doc "~&~%")
            (loop :for binding :in bindings :do
              (if (stringp binding)
                  (format doc "~%~A~%" binding)
                  (destructuring-bind (keyword val . docs) binding
                    (format doc "+ ~S (~D)~%" keyword (eval val))
                    (format doc "~{  ~A~%~}" docs))))
            (format doc
                    "~&Use functions `~S' to convert ~S flags into enum numbers. "
                    name name))
         '(member ,@(loop :for binding :in bindings
                          :if (listp binding)
                            :collect (car binding))))
       (defun ,name (&rest flags)
         ,(format nil
                  "Convert FLAGS into ObjC enum numbers.
Return `unsigned-byte' as enum numbers.

Parameters:
+ FLAGS: every flag should be of type `~S'. "
                  name)
         (the unsigned-byte
           (reduce (lambda (res flag)
                     (logior (the unsigned-byte res)
                             (the unsigned-byte
                               (ecase flag
                                 ,@(loop :for (keyword val) :in (remove-if #'stringp bindings)
                                         :collect `(,(the keyword keyword)
                                                    ,(eval val)))))))
                   flags
                   :initial-value (the unsigned-byte 0))))
       (define-compiler-macro ,name (&whole expr &rest flags)
         (if (every #'keywordp flags)
             (eval expr)
             expr)))))

(defmacro with-objc-enum-flags (val &body body)
  (let ((value (gensym "VAL"))
        (flags (gensym "FLAGS")))
    `(let ((,value ,val)
           (,flags ()))
       ,@(loop :for (flag v) :in body
               :for mask := (eval v)
               :collect `(when (= ,mask (logand ,value ,mask))
                           (push ,flag ,flags)))
       ,flags)))

;;;; enum.lisp ends here
