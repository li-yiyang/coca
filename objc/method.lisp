;;;; method.lisp --- Implement ObjC method wrapper

(in-package :coca.objc)


;;;; objc-method-encoding

(defvar *objc-method-encodings* (make-hash-table :test 'equal)
  "ObjC method encodings.

KEY: method type encoding string
VAL: `coca.objc::objc-method-encoding'")

(defstruct objc-method-encoding
  (encoding ""    :type string)
  (atypes   ()    :type list)
  (ret      :void :type objc-basic-encoding)
  (calling  nil   :type function)
  (cif      nil   :type foreign-pointer))

(defun %alloc-ffi-cif (ret arg-types)
  "Allocate ffi_cif for method of encoding.
Return values are
+ ffi_cif foreign pointer,
+ ffi_type pointer array
+ and calling function.

Parameters:
+ RET: basic ObjC encoding
+ ARG-TYPES: list of ObjC encoding
"
  (declare (type list                arg-types)
           (type objc-basic-encoding ret))
  (let* ((len    (length arg-types))
         (atypes (foreign-alloc :pointer :count len)))
    (loop :for i :from 0
          :for enc :in arg-types
          :do (setf (mem-aref atypes :pointer i)
                    (objc-encoding-ffi-type enc)))
    (let ((cif (coca_alloc_ffi_cif len (objc-encoding-ffi-type ret) atypes)))
      (values cif
              atypes
              (compile nil (objc-method-foreign-call-form
                            cif ret arg-types))))))

(defun coerce-to-objc-method-encoding (encoding)
  "Make `coca.objc::objc-method-encoding' with ENCODING.

Parameter:
+ ENCODING: ObjC method type encoding string
"
  (declare (type string encoding))
  (with-cached (encoding *objc-method-encodings*)
    (multiple-value-bind (arg-types ret)
        (decode-objc-type-encoding encoding)
      (multiple-value-bind (cif atypes calling)
          (%alloc-ffi-cif ret arg-types)
        (let* ((info (make-objc-method-encoding
                      :encoding encoding
                      :atypes   arg-types
                      :ret      ret
                      :cif      cif
                      :calling  calling)))
          (tg:finalize info (lambda ()
                              (foreign-free cif)
                              (foreign-free atypes)))
          info)))))


;;; Generate ObjC method calling lambda form
;;
;; See man ffi_call
;;
;; 1. generate ffi_cif:
;;    + `objc-method-encoding'
;;
;;    ffi_type* atypes = malloc(sizeof(ffi_type*) * (length ARGS));
;;    atypes[i] = (objc-encoding-ffi-type ARGS[i]);
;;    ffi_cif* cif = coca_alloc_ffi_cif(
;;      (length ARGS),
;;      (objc-encoding-ffi-type RET-TYPE),
;;      atypes
;;    );
;; 2. compile calling lambda function
;;    + `objc-method-foreign-alloc-form'
;;    + `objc-method-foreign-setf-form'
;;    + `objc-method-foreign-aref-form'
;;    + `objc-method-foreign-free-form'
;;    + `objc-method-foreign-call-form'

(defun objc-method-foreign-alloc-form (encoding)
  "Alloc foreign memory as place-holder for lisp value of ObjC ENCODING.
Return S-expression to be used in `coca.objc::objc-encoding-foreign-aref-form'.

Parameter:
+ ENCODING: ObjC encoding to alloc

Dev Note:
this is equal to generate a C code calling:

    malloc(sizeof(ENCODING))

"
  (declare (type objc-basic-encoding encoding))
  (case encoding
    ((:unknown :class :sel :object :pointer :string)
     `(foreign-alloc :pointer))
    (otherwise
     `(foreign-alloc ',encoding))))

(defun objc-method-foreign-setf-form (foreign lisp encoding)
  "Copy LISP value to FOREIGN with ENCODING.
Return S-expression to be used in `coca.objc::objc-encoding-foreign-aref-form'.

Parameters:
+ FOREIGN: symbol of variable to the foreign arg
  FOREIGN is allocated by `coca.objc::objc-encoding-alloc-form'
+ LISP: symbol of lisp variable to be copied to FOREIGN
+ ENCODING: ObjC encoding"
  (case (atomize encoding)
    (:string                            ; char **FOREIGN;
     `(setf (mem-aref ,foreign :pointer) (foreign-string-alloc ,lisp)))
    (:struct                            ; struct ENCODING *FOREIGN;
     (let* ((struct    (objc-struct           (second encoding)))
            (slots     (objc-struct-slots     struct))
            (coerce-p  (objc-struct-coerce-p  struct))
            (encodings (objc-struct-encodings struct)))
       `(with-foreign-slots (,slots ,foreign ,encoding)
          ,@(loop :for i :from 0
                  :for slot     :in slots
                  :for coerce   :in coerce-p
                  :for encoding :in encodings
                  :collect (if coerce-p
                               `(setf ,slot (coerce (struct-aref ,lisp ,i)
                                                    ',(objc-encoding-lisp-type encoding)))
                               `(setf ,slot (struct-aref ,lisp ,i)))))))
    ((:object :sel :class)              ; id/SEL/Class FOREIGN;
     `(setf (mem-aref ,foreign :pointer) (objc-object-pointer ,lisp)))
    (otherwise
     `(setf (mem-aref ,foreign ,(objc-encoding-cffi-type encoding)) ,lisp))))

(defun objc-method-foreign-aref-form (foreign encoding)
  "Copy foreign value from FOREIGN to LISP with ENCODING.
Return S-expression to be used in `coca.objc::objc-encoding-foreign-aref-form'.

Parameter:
+ FOREIGN: symbol of variable to the foreign result pointer
+ ENCODING: ObjC encoding"
  (declare (type objc-basic-encoding encoding))
  (case (atomize encoding)
    (:string                            ; char **FOREIGN;
     `(the string (foreign-string-to-lisp (mem-aref ,foreign :pointer))))
    (:struct                            ; struct STRUCT *FOREIGN;
     (let* ((struct-info (objc-struct (second encoding)))
            (name        (objc-struct-lisp-name struct-info))
            (slots       (objc-struct-slots     struct-info)))
       `(with-foreign-slots (,slots ,foreign ,encoding)
          ;; (make-STRUCT {:SLOT SLOT}* ...)
          (,(intern (str:concat "MAKE-" (symbol-name name)) (symbol-package name))
           ,@(loop :for slot :in slots
                   :collect (intern (symbol-name slot) :keyword)
                   :collect slot)))))
    (:object                            ; id *FOREIGN;
     `(coerce-to-objc-object          (the foreign-pointer (mem-aref ,foreign :pointer))))
    (:class                             ; Class *FOREIGN;
     `(pointer-to-objc-class-nullable (the foreign-pointer (mem-aref ,foreign :pointer))))
    (:sel                               ; SEL *FOREIGN;
     `(coerce-to-selector             (the foreign-pointer (mem-aref ,foreign :pointer))))
    (otherwise                          ; ENCODING* FOREIGN;
     `(the ,(objc-encoding-lisp-type encoding)
        (mem-aref ,foreign ,(objc-encoding-cffi-type encoding))))))

(defun objc-method-foreign-free-form (foreign encoding)
  "Free FOREIGN with ENCODING.
Return S-expression to be used in `coca.objc::objc-encoding-foreign-aref-form'.

Parameters:
+ FOREIGN: symbol to foreign pointer
+ ENCODING: ObjC encoding
"
  (declare (type symbol foreign)
           (type objc-basic-encoding encoding))
  (case (atomize encoding)
    (:string `(progn (foreign-free (mem-aref ,foreign :pointer))
                     (foreign-free ,foreign)))
    (otherwise `(foreign-free ,foreign))))

(defun objc-method-foreign-call-form (cif ret arg-types)
  "Generate ObjC method calling form.

Dev Note:
This is equal to generating below C code:

    void* arg_values = malloc(sizeof(void*) * LEN);

    // Init args: (objc-encoding-copy-to-foreign SYM VAL ENCODING)
    // see `coca.objc::objc-method-foreign-alloc-form'
    void* arg[i] = malloc(sizeof(TYPE));

    // Fill ffi_call arg_values buffer
    // see `coca.objc::objc-method-foreign-setf-form'
    arg_values[i] = arg[i];

    // Call `coca.objc::coca_objc_msgSend'
    // see coca/objc;wrapper.lisp
    exception = coca_objc_msgSend(&cif, imp, &result, arg_values);

    // Clean up:
    // see `coca.objc::objc-method-foreign-free-form'
    free(arg_values);
    free(arg[i]);
"
  (multiple-value-bind (args syms declaration)
      (loop :for type :in arg-types
            :for name := (symbol-name (atomize type))
            :for arg  := (gensym name)
            :for sym  := (gensym (str:concat "FOREIGN-" name))
            :for hint := (case (atomize type)
                           (:object   `(or standard-objc-object
                                           objc-class
                                           foreign-pointer
                                           null))
                           (:struct   `(or ,(objc-encoding-lisp-type type)
                                           simple-vector))
                           (otherwise (objc-encoding-lisp-type  type)))
            :collect arg :into args
            :collect sym :into syms
            :collect `(type ,hint ,arg) :into declaration
            :finally (return (values args syms declaration)))
    (let ((len     (length (the list arg-types)))
          (voidp   (eq ret :void))
          (avalues (gensym "ARG-VALUES"))
          (result  (gensym "RESULT"))
          (imp     (gensym "IMP"))
          (status  (gensym "STATUS")))
      `(lambda (,imp ,@args)
         (declare (type foreign-pointer ,imp)
                  ,@declaration)
         (let ((,avalues (foreign-alloc :pointer :count ,len))
               ,@(loop :for sym  :in syms
                       :for type :in arg-types
                       :collect `(,sym ,(objc-method-foreign-alloc-form type)))
               (,result ,(if voidp
                             '(null-pointer)
                             (objc-method-foreign-alloc-form ret))))
           (declare (type foreign-pointer ,avalues ,@syms ,result))
           (unwind-protect
                (progn
                  ,@(loop :for arg  :in args
                          :for sym  :in syms
                          :for type :in arg-types
                          :collect (objc-method-foreign-setf-form sym arg type))
                  ,@(loop :for i :from 0
                          :for sym :in syms
                          :collect `(setf (mem-aref ,avalues :pointer ,i) ,sym))
                  (let ((,status (coca_objc_msgSend ,cif ,imp ,result ,avalues)))
                    (if (null-pointer-p ,status)
                        ,(if voidp
                             '(values)
                             (objc-method-foreign-aref-form result ret))
                        (error "NSException: ~S. " (coerce-to-objc-object ,status)))))
             (foreign-free ,avalues)
             ,@(loop :for sym  :in syms
                     :for type :in arg-types
                     :collect (objc-method-foreign-free-form sym type))
             (foreign-free ,result)))))))

(defun compile-objc-method-calling (imp encoding)
  "Compile an lambda function calling IMP using ENCODING. "
  (let* ((enc (coerce-to-objc-method-encoding encoding))
         (fun (objc-method-encoding-calling   enc)))
    (lambda (&rest args) (apply fun (cons imp args)))))



(defun objc-class-class-method (class sel)
  "Get function to call CLASS and SEL. "
  (declare (type objc-class class)
           (type sel        sel))
  (the function
    (with-slots (objc-class-methods) class
      (with-cached (sel objc-class-methods)
        (let ((method (class_getClassMethod (objc-object-pointer class)
                                            (objc-object-pointer sel))))
          (when (null-pointer-p method)
            (error "Unknown ObjC class method ~A for ~A. "
                   (sel-name        sel)
                   (objc-class-name class)))
          (let ((imp (method_getImplementation method))
                (enc (method_getTypeEncoding   method)))
            (compile-objc-method-calling imp enc)))))))

(defun objc-class-instance-method (class sel)
  "Get function to call instance of CLASS and SEL. "
  (declare (type objc-class class)
           (type sel        sel))
  (the function
    (with-slots (objc-instance-methods) class
      (with-cached (sel objc-instance-methods)
        (let ((method (class_getInstanceMethod (objc-object-pointer class)
                                               (objc-object-pointer sel))))
          (when (null-pointer-p method)
            (error "Unknown ObjC instance method ~A for ~A. "
                   (sel-name        sel)
                   (objc-class-name class)))
          (let ((imp (method_getImplementation method))
                (enc (method_getTypeEncoding   method)))
            (compile-objc-method-calling imp enc)))))))

(defun objc-method-encoding (object method)
  "Find `coca.objc::objc-method-encoding' by OBJECT and METHOD. "
  (declare (type (or symbol string objc-class foreign-pointer objc-pointer) object)
           (type (or string sel) method))
  (etypecase object
    ((or symbol string objc-class)
     (let* ((class  (coerce-to-objc-class object))
            (class* (objc-object-pointer  class))
            (sel    (coerce-to-selector   method))
            (sel*   (objc-object-pointer  sel))
            (method (class_getClassMethod class* sel*))
            (method (if (null-pointer-p method)
                        (class_getInstanceMethod class* sel*)
                        method)))
       (when (null-pointer-p method)
         (error "Unknown ObjC method ~A for ~A. " sel class))
       (coerce-to-objc-method-encoding (method_getTypeEncoding method))))
    (standard-objc-object
     (let* ((class  (class-of            object))
            (sel    (coerce-to-selector  method))
            (class* (objc-object-pointer class))
            (sel*   (objc-object-pointer sel))
            (method (class_getInstanceMethod class* sel*)))
       (when (null-pointer-p method)
         (error "Unknown ObjC method ~A for ~A. " sel class))
       (coerce-to-objc-method-encoding (method_getTypeEncoding method))))
    (foreign-pointer
     (let ((class (if (object_isClass object)
                      (coerce-to-objc-class object)
                      (coerce-to-objc-class (object_getClassName object)))))
       (objc-method-encoding class method)))))

(defun objc-class-method-signature (object method)
  "Return signature of OBJECT and METHOD. "
  (let ((enc (objc-method-encoding object method)))
    (values (objc-method-encoding-atypes   enc)
            (objc-method-encoding-ret      enc)
            (objc-method-encoding-encoding enc))))


;;; invoke

;; self:  self
;; class: super class of self
(defstruct objc-super
  (self  nil :type standard-objc-object)
  (class nil :type objc-class))

(defun can-invoke-p (object method)
  "Test if OBJECT can invoke METHOD or not.

Parameter:
+ OBJECT:
  + string or symbol of ObjC class name: test both class and instance method
  + `objc-class': test class method
  + `standard-objc-object': test only instance method
  + foreign-pointer: depending on the object's type
+ METHOD: `sel' or things can be `coerce-to-selector'"
  (etypecase object
    ((or symbol string)
     (let ((class (coerce-to-objc-class object))
           (sel   (coerce-to-selector   method)))
       (or (ignore-errors (and (objc-class-class-method    class sel) t))
           (ignore-errors (and (objc-class-instance-method class sel) t)))))
    (standard-objc-object
     (and (ignore-errors (objc-class-instance-method (class-of           object)
                                                     (coerce-to-selector method)))
          t))
    (objc-class
     (and (ignore-errors (objc-class-class-method object (coerce-to-selector method)))
          t))
    (foreign-pointer
     (if (object_isClass object)
         (can-invoke-p (coerce-to-objc-class  object) method)
         (can-invoke-p (coerce-to-objc-object object) method)))))

(defun invoke (object method &rest args)
  "Call METHOD on OBJECT by ARGS.
Return value is wrapped as lisp value.

Parameters:
+ OBJECT: object, class to call
+ METHOD: sel, string as function

Return Value:
+ if Class:  wrap as `objc-class'
+ if Object: wrap as `standard-objc-object'
+ if SEL:    wrap as `sel'
+ if struct: as lisp struct (`define-objc-struct')
+ if CFFI compatible value: as CFFI behavior

Example:

    ;; [NSWindow alloc];
    (invoke \"NSWindow\" \"alloc\")

    ;; [NSString initWithUTF8String:\"Hello World\"];
    (invoke \"NSString\" \"initWithUTF8String:\" \"Hello World\")

    ;; [[NSWindow alloc] initWithContentRect:rect
    ;;                             styleMask:style
    ;;                               backing:backing
    ;;                                 defer:defer]
    (invoke (invoke \"NSWindow\" \"alloc\")
            \"initWithContentRect:styleMask:backing:defer:\"
            rect style backing defer)
"
  (declare (type (or symbol string objc-class foreign-pointer objc-pointer objc-super) object)
           (type (or string sel) method))
  (etypecase object
    (standard-objc-object
     (let ((class (class-of object))
           (sel   (coerce-to-selector method)))
       (apply (the function (objc-class-instance-method class sel))
              (cons object (cons sel args)))))
    (objc-super
     (let ((class (objc-super-class object))
           (sel   (coerce-to-selector method)))
       (apply (the function (objc-class-instance-method class sel))
              (cons (objc-super-self object) (cons sel args)))))
    ((or symbol string objc-class)
     (let ((class  (coerce-to-objc-class object))
           (sel    (coerce-to-selector   method)))
       (apply (the function (objc-class-class-method class sel))
              (cons class (cons sel args)))))
    (foreign-pointer
     (if (object_isClass object)
         (let ((class (coerce-to-objc-class object))
               (sel   (coerce-to-selector   method)))
           (apply (the function (objc-class-class-method class sel))
                  (cons class (cons sel args))))
         (let ((class (coerce-to-objc-class (objc_getClass object)))
               (sel   (coerce-to-selector   method)))
           (apply (the function (objc-class-instance-method class sel))
                  (cons object (cons sel args))))))))


;;; send

(defmacro send (object &rest sending-form)
  "CCL's objc:send style macro for `invoke'.

Example:

    (send object :message-like-this) ;; => (invoke object \"messageLikeThis\")
    (send object :foo-abc a :bar b)  ;; => (invoke object \"fooAbc:Bar:\" a b)

Dev Note:
Just a compability layer.
Use with caution. "
  (let ((len (length (the list sending-form))))
    (when (zerop len)
      (error "Malformed sending form, expecting (send ~A [METHOD-ARG...]). " object))
    (cond ((= len 1)
           `(invoke ,object ,(str:camel-case (first sending-form))))
          ((evenp len)
           (loop :with sel  := (str:concat (str:camel-case (pop sending-form)) ":")
                 :with args := (list (pop sending-form))
                 :for (key . rest) :on sending-form :by #'cddr
                 :for arg := (if (endp rest)
                                 (error "Missing argument after ~S. " key)
                                 (first rest))
                 :do (setf sel (str:concat sel (str:pascal-case key) ":"))
                     (push arg args)
                 :finally (return `(invoke ,object ,sel ,@(reverse args)))))
          (t
           (error "Malfromed SENDING-FORM ~S. " sending-form)))))


;;; define-objc-method

;; `objc-generic-function' is defined in coca/objc;sel.lisp

(defun %update-objc-generic-function-objc-class (gf old-class &optional force)
  "Update GF's `objc-class' if new class is superclass of OLD-CLASS.

Parameters:
+ GF: updated or initialized `coca.objc::objc-generic-function'
+ OLD-CLASS: previous objc-class of the GF or nil
+ FORCE: non-nil to force update

Dev Note:
use only in `reinitialize-instance', `initialize-instance' and
`add-method' for `coca.objc::objc-generic-function'. "
  (declare (type objc-generic-function gf)
           (type (or null objc-class)  old-class))
  ;; Make sure that GF is properly initialized
  (when (slot-boundp gf 'objc-class)
    (with-slots (objc-class sel objc-type objc-object-pointer) gf
      (if (and old-class (c2mop:subclassp objc-class old-class) (not force))
          ;; if new OBJC-CLASS is just subclass of OLD-CLASS,
          ;; reset the `objc-clas' of GF by OLD-CLASS
          (setf objc-class old-class)
          ;; if OLD-CLASS is unbound (nil), or subclass of new OBJC-CLASS,
          ;; set the default implementation of GF
          (let (;; the IMP should be get by `class_replaceMethod',
                ;; the ObjC callback should be
                (imp (class_replaceMethod (objc-object-pointer objc-class)
                                          (objc-object-pointer sel)
                                          (get-callback (c2mop:generic-function-name gf))
                                          objc-type)))
            ;; Clear previously cached ObjC instance-methods (if any)
            (setf (gethash sel (slot-value objc-class 'objc-instance-methods)) nil)
            ;; if original IMP is not NULL, which means existing previously
            ;; defined ObjC method, define the default callback
            (unless (null-pointer-p imp)
              (let* ((lambda-list  (c2mop:generic-function-lambda-list gf))
                     ;; the default method should have specializer like
                     ;; (OBJC-CLASS t t ...), to make it, just replace
                     ;; the first with OBJC-CLASS and leave the rest as t class
                     (specializers (cons objc-class
                                         (loop :with tclass := (find-class t)
                                               :repeat (1- (length lambda-list))
                                               :collect tclass))))
                ;; if the definition of generic method already exists,
                ;; do not overwrite it (this should be overwrite when `defmethod')
                (unless (find-method gf () specializers nil)
                  (let* ((function  (compile-objc-method-calling imp objc-type))
                         (gf-method (make-instance
                                     'standard-method
                                     :lambda-list   lambda-list
                                     :qualifier     ()
                                     :specializers  specializers
                                     :function      function
                                     :documentation (format nil
                                                            "Invokes ObjC method ~A. "
                                                            (sel-name sel)))))
                    (add-method gf gf-method))))))))))

(defmethod reinitialize-instance :around ((gf objc-generic-function) &key)
  (let ((old-class (when (slot-boundp gf 'objc-class)
                     (slot-value gf 'objc-class))))
    (call-next-method)
    (%update-objc-generic-function-objc-class gf old-class)
    gf))

(defmethod initialize-instance :around ((gf objc-generic-function) &key)
  (let ((old-class (when (slot-boundp gf 'objc-class)
                     (slot-value gf 'objc-class))))
    (call-next-method)
    (%update-objc-generic-function-objc-class gf old-class)
    (setf (gethash (c2mop:generic-function-name gf) *objc-methods*) gf)
    gf))

;; TODO: maybe i should modify the `%update-objc-generic-function-objc-class'
;; function to make sure that it's more efficient.
(defmethod add-method :around ((gf objc-generic-function) (method method))
  (let ((new-class (car (c2mop:method-specializers method))))
    (if (typep new-class 'objc-class)
        (let ((old-class (slot-value gf 'objc-class)))
          (setf (slot-value gf 'objc-class) new-class)
          (call-next-method)
          (%update-objc-generic-function-objc-class gf old-class)
          method)
        (call-next-method))))

(trivial-indent:define-indentation define-objc-method (4 4 &lambda &body))
(defmacro define-objc-method ((class method &optional name) ret lambda-list &body options)
  "Define ObjC instance METHOD for CLASS.

Syntax:

    (define-objc-method (CLASS METHOD &optional NAME) RET
        ( { (VAR OBJC-ENCODING) }* )
      (:documentation \"docstring\")
      (:method ((self class) (arg ...))
        body)
      (:wrapper wrapper-function))

+ CLASS: symbol or string of ObjC class
  this should be the root of ObjC class where the lisp generic function
  would be used to replace / define ObjC method IMP as callback
+ METHOD: string of method SEL name
+ NAME: lisp generic method name
  if not given, would generated by METHOD (see `coca.objc::objc-intern')
+ RET: ObjC type encoding
  it is recommanded to use `objc-basic-encoding'
+ LAMBDA-LIST:
  VAR: symbol of argument
  OBJC-ENCODING: ObjC type encoding for argument

Example:

    (define-objc-method (ns-window \"foo\") :bool ()
      (:documentation \"...\")
      (:wrapper as-boolean)
      (:method ((self ns-window))
        (call-next-method) ;; like [super ...] in ObjC
        (foo self)))
"
  (declare (type (or symbol string) class)
           (type string method)
           (type symbol name)
           (type objc-encoding ret))
  (let* ((class-root  (coerce-to-objc-class class))
         (class-name  (class-name           class-root))
         (name        (or name (objc-intern method)))
         (ret         (as-objc-encoding ret))
         (lambda-list (loop :for (var enc) :in lambda-list
                            :collect (list var (as-objc-encoding enc))))
         (rest-args   (mapcar #'first lambda-list))
         (wrapper     (or (second (assoc :wrapper options))
                          (when (eql ret :bool) 'as-boolean)))
         (encodings   (encode-objc-type-encoding
                       `(,ret :object :sel ,@(mapcar #'second lambda-list))))
         (sel         (gensym "SEL")))
    `(progn
       (defcallback ,name ,(objc-encoding-cffi-type ret)
           ((self :pointer)
            (,sel :pointer)
            ,@(loop :for (var enc) :in lambda-list
                    :collect (list var (objc-encoding-cffi-type enc))))
         (declare (ignore ,sel))
         (let ((self (coerce-to-objc-object self))
               ,@(loop :for (var enc) :in lambda-list
                       :for type := (atomize enc)
                       :if (eql type :object)
                         :collect `(,var (coerce-to-objc-object          ,var))
                       :if (eql type :class)
                         :collect `(,var (pointer-to-objc-class-nullable ,var))
                       :if (eql type :sel)
                         :collect `(,var (coerce-to-selector             ,var))))
           (,name self ,@rest-args)))
       ;; ensure `objc-class' and `sel' is correctly setted
       (c2mop:ensure-generic-function
        ',name
        :generic-function-class (find-class 'objc-generic-function)
        :objc-type              ,encodings
        :objc-class             (coerce-to-objc-class ,class)
        :sel                    (coerce-to-selector   ,method)
        :documentation          ,(or (second (assoc :documentation options))
                                     (format nil
                                             "Implements ObjC method ~S"
                                             method)))
       ,@(when wrapper
           `((defmethod ,name :around ((self ,class-name) ,@rest-args)
               (,wrapper (call-next-method)))))
       ,@(loop :for (car . cdr) :in options
               :if (eql car :method)
                 :collect `(defmethod ,name ,@cdr))
       ',name)))


;;;; method.lisp ends here
