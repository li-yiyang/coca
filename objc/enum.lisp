;;;; enum.lisp --- Wrapper of ObjC enum

(in-package :coca.objc)

(defmacro define-objc-enum (name &body bindings)
  "Define ObjC enum of NAME with BINDINGS.

Syntax:

    (define-objc-enum NAME
      [docstring]
      (KEYWORD VAL [docstring]))

Dev Note:
this will define:
+ lisp type of NAME
+ a lisp function <NAME>-p to test if is valid flag
+ an encoder function of NAME to convert enum keywords to unsigned-byte
+ a decoder function of NAME DECODE-<NAME> to convert unsigned-byte to keywords
"
  (let ((docstring (pop bindings)))
    (unless (stringp docstring)
      (push docstring bindings)
      (setf docstring (format nil "ObjC enum of ~A. " name)))
    (let ((documentation (with-output-to-string (doc)
                           ;; [DOCSTRING]
                           (write-line docstring doc)
                           (format doc "~&~%")
                           ;; + KEYWORD (LITERAL-VALUE)
                           ;;   documentation string
                           ;; ...
                           (loop :for binding :in bindings :do
                             (if (stringp binding)
                                 (format doc "~%~A~%" binding)
                                 (destructuring-bind (keyword val . docs) binding
                                   (format doc "+ ~S (~D)~%" keyword (eval val))
                                   (format doc "~{  ~A~%~}" docs))))
                           ;; Use functions ...
                           (format doc
                                   "~&Use functions `~S' to convert ~S flags into enum numbers. "
                                   name name)))
          ;; BINDINGS:
          ;; ((KEYWORD LITERAL-VALUE) ...)
          (bindings      (loop :for binding :in bindings
                               :if (listp binding)
                                 :collect (destructuring-bind (keyword val . ignore) binding
                                            (declare (ignore ignore))
                                            (list (the keyword       keyword)
                                                  (the unsigned-byte (eval val)))))))
      `(progn
         (deftype ,name ()
           ,documentation
           '(member ,@(mapcar #'car bindings)))
         (defun ,(intern (str:concat (symbol-name name) "-P")) (flag)
           ,(format nil "Test if FLAG is of ~S flag. " name)
           (flet ((flagp (flag) (and (keywordp flag) (typep flag ',name))))
             (or (flagp flag)
                 (and (listp flag) (every #'flagp flag)))))
         (defun ,name (flag &rest flags)
           ,(format nil
                    "Convert FLAGS into ObjC enum numbers.
Return `unsigned-byte' as enum numbers.

Parameters:
+ FLAGS: every flag should be of type `~S'.

See type documentation `~S'. "
                    name name)
           (labels ((decode (flag)
                      (the unsigned-byte
                        (ecase flag
                          ,@(loop :for (keyword val) :in bindings
                                  :collect `(,keyword ,val)))))
                    (decode* (list)
                      (the unsigned-byte
                        (reduce (lambda (res flag)
                                  (logior (the unsigned-byte res)
                                          (the unsigned-byte (decode flag))))
                                list
                                :initial-value (the unsigned-byte 0)))))
             (the unsigned-byte
               (if (endp flags)
                   (etypecase flag
                     (unsigned-byte flag)
                     (keyword       (decode  flag))
                     (list          (decode* flag)))
                   (decode* (cons flag flags))))))
         (defun ,(intern (str:concat "DECODE-" (symbol-name name))) (flag)
           ,(format nil "Decode FLAG as `~S', a list of `~S' or unsigned-integer if fails. "
                    name name)
           (declare (type unsigned-byte flag))
           (with-objc-enum-flags flag
             ,@bindings))))))

;; TODO: bettter and efficient decoding methods
(defmacro with-objc-enum-flags (val &body body)
  "Decode ObjC VAL as ObjC enum flags.
Return decoded flags as single keyword, list of keywords as flags,
or VAL if failed to decode VAL.

Syntax:

    (with-objc-enum-flags VAL
      (FLAG    VAL)
      ...)
"
  (let ((value (gensym "VAL"))
        (flags (gensym "FLAGS"))
        (cnts  (gensym "CNTS")))
    `(let ((,value ,val)
           (,flags ())
           (,cnts  0))
       ,@(loop :for (flag v) :in body
               :for mask := (eval v)
               :collect `(when (= ,mask (logand ,value ,mask))
                           (push ,flag ,flags)
                           (incf ,cnts)))
       (cond ((zerop ,cnts) ,value)
             ((= ,cnts 1)   (car ,flags))
             (t             ,flags)))))

;;;; enum.lisp ends here
