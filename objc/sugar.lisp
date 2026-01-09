;;;; sugar.lisp --- Syntax sugar for Coca.ObjC

(in-package :coca.objc)


;;; objc-encoding

(trivial-indent:define-indentation define-objc-typedef (4 4 &body))
(defmacro define-objc-typedef (name objc-encoding &body body)
  "Define ObjC type alias of NAME for OBJC-ENCODING.

Example:

    ;; define ObjC type alias
    (define-objc-typedef ns-uinteger :unsigned-long
      \"NSUInteger in lisp. \")

    ;; define ObjC type, but overwrite the lisp type
    (define-objc-typedef ns-null :object
      \"NSNull\"
      'null)

Syntax:

   (define-objc-typedef NAME OBJC-ENCODING &body [DOC] BODY)

Parameters:
+ NAME: symbol as lisp type alias
+ OBJC-ENCODING: `objc-encoding'
+ DOC: document string of type
+ TYPE: if given, would generate additional TYPE declaration for NAME
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

(trivial-indent:define-indentation define-objc-struct (4 &body))
(defmacro define-objc-struct ((lisp-name objc-name) &body slot-definitions)
  "Define ObjC struct.

Example:

    ;; the
    (define-objc-struct (ns-point \"CGPoint\")
      (x :double)
      (y :double))

    ;; overwrite default lisp slot type definition
    (define-objc-struct (ns-rect \"CGRect\")
      (x :double  :type real)
      (y :double  :type real)
      (w :double  :type real)
      (h :double  :type real))

Syntax:

    (define-objc-struct (LISP-NAME OBJC-NAME)
      { (SLOT OBJC-ENCODING &key TYPE) }*)

Parameters:
+ LISP-NAME: symbol of lisp struct
+ OBJC-NAME: string of ObjC struct
+ SLOT: symbol of slot
+ OBJC-ENCODING: ObjC encoding of struct slot
+ TYPE: lisp type of slot,
  if setting, the result should be coerced when copy to foreign

Dev Note:
This will define:
+ lisp struct of LISP-NAME
+ cffi struct of LISP-NAME
+ `coca.objc::struct-aref' method implementation
  the struct in Coca.ObjC could be replaced by `simple-vector',
  which means calling with (coca:make-ns-rect ...) is equal to
  calling with #(300d0 300d0 400d0 400d0).
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

Example:

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

(trivial-indent:define-indentation define-objc-const (4 &lambda &body))
(defmacro define-objc-const (name (objc-name objc-encoding &optional (library :default))
                             &optional documentation
                             &aux (encoding (as-objc-encoding objc-encoding)))
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
           (type (or null string) documentation))
  `(progn
     (declaim (type ,(objc-encoding-lisp-type objc-encoding) ,name))
     ,(case (atomize encoding)
        ((:union :array)  (error "Don't support `:union' yet. "))
        ((:struct :pointer :object :class :sel)
         (let ((fn (intern (format nil "%DEFINE-OBJC-CONST-STRUCT-~A" objc-name))))
           `(progn
              (defvar ,name)
              (setf (documentation ',name 'variable) ,documentation)
              (defun ,fn ()
                (setf ,name ,(objc-method-foreign-aref-form
                              `(foreign-symbol-pointer ,objc-name :library ',library)
                              encoding)))
              (pushnew ',fn *coca-post-init-hooks*))))
        (otherwise
         `(defconstant ,name
            (cffi:mem-ref (foreign-symbol-pointer ,objc-name :library ',library)
                          ,(objc-encoding-cffi-type encoding))
            ,documentation)))))

(defparameter *coca-pre-init-hooks* ()
  "A list of hook functions before Coca init. ")

(defparameter *coca-post-init-hooks* ()
  "A list of hook functions after Coca init. ")

#+sbcl
(pushnew 'coca-init sb-ext:*init-hooks*)
(defun coca-init ()
  "Initialize Coca foreign pointers.

Dev Note:
if you save Coca as standalone executable file, you should call
this method before all the method calling of ObjC methods or objects.

Use `define-coca-init' to clean up and setup how to reload
ObjC environment."
  (map nil #'funcall *coca-pre-init-hooks*)
  ;; Clear ffi_type, re-register it
  (setf +ffi_type_sint8+   (foreign-symbol-pointer "ffi_type_sint8")
        +ffi_type_uint8+   (foreign-symbol-pointer "ffi_type_uint8")
        +ffi_type_sint16+  (foreign-symbol-pointer "ffi_type_sint16")
        +ffi_type_uint16+  (foreign-symbol-pointer "ffi_type_uint16")
        +ffi_type_sint32+  (foreign-symbol-pointer "ffi_type_sint32")
        +ffi_type_uint32+  (foreign-symbol-pointer "ffi_type_uint32")
        +ffi_type_sint64+  (foreign-symbol-pointer "ffi_type_sint64")
        +ffi_type_uint64+  (foreign-symbol-pointer "ffi_type_uint64")
        +ffi_type_float+   (foreign-symbol-pointer "ffi_type_float")
        +ffi_type_double+  (foreign-symbol-pointer "ffi_type_double")
        +ffi_type_pointer+ (foreign-symbol-pointer "ffi_type_pointer")
        +ffi_type_void+    (foreign-symbol-pointer "ffi_type_void"))
  (maphash (lambda (- struct)
             (declare (type objc-struct struct))
             (tg:cancel-finalization struct)
             (multiple-value-bind (ffi-type elements)
                 (%alloc-struct-ffi-type (objc-struct-encodings struct))
               (setf (objc-struct-ffi-type struct) ffi-type)
               (tg:finalize struct (lambda ()
                                     (foreign-free ffi-type)
                                     (foreign-free elements)))
               struct))
           *objc-structs*)
  ;; Clear ffi_cif, re-register it
  (maphash (lambda (- method-encoding)
             (declare (type objc-method-encoding method-encoding))
             (tg:cancel-finalization method-encoding)
             (multiple-value-bind (cif atypes)
                 (%alloc-ffi-cif (objc-method-encoding-ret    method-encoding)
                                 (objc-method-encoding-atypes method-encoding))
               (setf (objc-method-encoding-cif method-encoding) cif)
               (tg:finalize method-encoding (lambda ()
                                              (foreign-free cif)
                                              (foreign-free atypes)))
               method-encoding))
           *objc-method-encodings*)
  ;; rebind ObjC Class pointer
  (maphash (lambda (- class) (reinitialize-instance class)) *classes*)
  ;; rebind ObjC SEL pointer
  (maphash (lambda (- sel)   (reinitialize-instance sel))   *sels*)
  ;; clear ObjC objects, should be rebind in *coca-post-init-hooks*
  (clrhash *objc-objects*)
  (map nil #'funcall *coca-post-init-hooks*))

(defmacro define-coca-init (hook &body body)
  "Define HOOK to be triggered when init Coca.

Syntax:

    (define-coca-init { :pre | :post } &body)

Dev Note:
+ :pre will execute the BODY before `coca-init'
  this is adviced to remove the interned ObjC object
+ :post will execute the BODY after `coca-init'
  this is adviced to rebind the ObjC object

For example, if you defines an ObjC object as global parameter:

   (defparameter *foo* (get-objc-object-foo))
   (define-coca-init :post (setf *foo* (get-objc-object-foo)))

This will ensure that the foreign ObjC pointer is correctly
reinitialized after you close the ObjC environment. "
  (ecase hook
    (:pre  `(pushnew (lambda () ,@body) *coca-pre-init-hooks*  :test #'equal))
    (:post `(pushnew (lambda () ,@body) *coca-post-init-hooks* :test #'equal))))

;;;; sugar.lisp ends here
