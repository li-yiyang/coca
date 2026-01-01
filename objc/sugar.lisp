;;;; sugar.lisp --- Syntax sugar for Coca.ObjC

(in-package :coca.objc)


;;; objc-encoding

(defmacro define-objc-typedef (name objc-encoding &body body)
  "Define ObjC OBJC-ENCODING alias as NAME.

Syntax:

    (define-objc-typedef NAME OBJC-ENCODING
      [docstring]
      &body BODY)
"
  (let ((docstring (pop body))
        (type      (as-objc-encoding objc-encoding)))
    (unless (stringp docstring)
      (push docstring body)
      (setf docstring (format nil "ObjC encoding alias of ~S. " type)))
    `(progn
       (deftype ,name ()
         ,docstring
         ,(if (endp body)
              `',(objc-encoding-lisp-type type)
              `(progn ,@body)))
       (set-objc-encoding-alias ',name ',objc-encoding)
       ',name)))

(defmacro define-objc-struct ((lisp-name objc-name) &body slot-definitions)
  "Define ObjC struct.

Syntax:

    (define-objc-struct LISP-NAME OBJC-NAME
      { (SLOT OBJC-ENCODING &key TYPE) }*)

Parameters:
+ LISP-NAME: symbol of lisp struct
+ OBJC-NAME: string of ObjC struct
+ SLOT: symbol of slot
+ OBJC-ENCODING: ObjC encoding of struct slot
+ TYPE: lisp type of slot,
  if setting, the result should be coerced when copy to foreign
"
  (declare (type symbol lisp-name)
           (type string objc-name))
  (multiple-value-bind (slots encodings inits types coerce-p)
      (loop :for (slot objc-encoding &key type) :in slot-definitions
            :for encoding := (as-objc-encoding objc-encoding)
            :for coerce   := (and type t)
            :for stype    := (or  type (objc-encoding-lisp-type encoding))
            :collect slot                                :into slots
            :collect encoding                            :into encodings
            :collect (objc-encoding-init-value encoding) :into inits
            :collect stype                               :into types
            :collect coerce                              :into coerce-p
            :finally (return (values slots encodings inits types coerce-p)))
    `(progn
       (defstruct ,lisp-name
         ,@(loop :for slot :in slots
                 :for type :in types
                 :for init :in inits
                 :collect (list slot init :type type)))
       (defcstruct ,lisp-name
         ,@(loop :for slot :in slots
                 :for enc  :in encodings
                 :collect (list slot (objc-encoding-cffi-type enc))))
       ,@(loop :with prefix  := (str:concat (symbol-name lisp-name) "-")
               :with package := (symbol-package lisp-name)
               :for slot :in slots
               :for i :from 0
               :for reader := (intern (str:concat prefix (symbol-name slot)) package)
               :collect `(defmethod struct-aref ((struct ,lisp-name) (i (eql ,i)))
                           (,reader struct)))
       (regist-objc-struct ',lisp-name ',objc-name ',slots ',encodings ',coerce-p))))

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

(defmacro define-objc-const (name (objc-name objc-encoding &optional (library :default))
                             &optional documentation)
  "Define a ObjC constant of OBJC-NAME in lisp as NAME.

Syntax:

    (define-objc-const name
        (objc-name objc-encoding &optional library)
      [documentation])

Parameters:
+ NAME: lisp constant name
+ OBJC-NAME: objc global varible name
+ OBJC-ENCODING: should be ObjC-encoding literally
+ LIBRARY: library to search the ObjC global const
+ DOCUMENTATION: optional documentation"
  (declare (type symbol name)
           (type string objc-name)
           (type objc-encoding objc-encoding)
           (type (or null string) documentation))
  (case (atomize objc-encoding)
    ((:union :array)  (error "Don't support `:union' yet. "))
    ((:struct :pointer)
     `(defparameter ,name
        (cffi:mem-ref (cffi::fs-pointer-or-lose ,objc-name ',library)
                      ,(objc-encoding-cffi-type objc-encoding))
        ,documentation))
    ((:object)
     `(defparameter ,name
        (coerce-to-objc-object
         (cffi:mem-ref (cffi::fs-pointer-or-lose ,objc-name ',library)
                       :pointer))
        ,documentation))
    ((:class)
     `(defparameter ,name
        (coerce-to-objc-class
         (cffi:mem-ref (cffi::fs-pointer-or-lose ,objc-name ',library)
                       :pointer))
        ,documentation))
    ((:sel)
     `(defparameter ,name
        (coerce-to-selector
         (cffi:mem-ref (cffi::fs-pointer-or-lose ,objc-name ',library)
                       :pointer))
        ,documentation))
    (otherwise
     `(defconstant ,name
        (cffi:mem-ref (cffi::fs-pointer-or-lose ,objc-name ',library)
                      ,(objc-encoding-cffi-type objc-encoding))
        ,documentation))))

;; (trivial-indent:define-indentation define-objc-const (2 4 &body))

;;;; sugar.lisp ends here
