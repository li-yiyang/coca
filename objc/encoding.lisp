;;;; encoding.lisp --- Lowlevel Implementation of ObjC encoding logic

(in-package :coca.objc)


;;; objc-struct

(defparameter *objc-struct-names* (make-hash-table :test 'equal)
  "Named ObjC structs.

KEY: string of ObjC struct name
VAL: symbol of ObjC struct name.

Example:
KEY: \"CGRect\"
VAL: `ns-rect'")

(defparameter *objc-structs* (make-hash-table)
  "Infomations of ObjC structs.

KEY: symbol of ObjC struct name
VAL: `coca.objc::objc-struct'

Example:
KEY: `ns-rect'
VAL: (objc-struct :objc-name \"CGRect\"
                  :lisp-name 'nc-rect
                  :slots     '(x y w h)
                  :encodings '(:double :double :double :double)
                  :coerce-p  '(t t t t)
                  :ffi-type  <foreign-pointer to ffi_type>)
")

(defstruct objc-struct
  (objc-name ""  :type string)
  (lisp-name nil :type symbol)
  (slots     ()  :type list)
  (encodings ()  :type list)
  (coerce-p  ()  :type list)
  (ffi-type  nil :type foreign-pointer))

(defun objc-struct (name)
  "Get registered ObjC struct of NAME.
Return `coca.objc::objc-struct'.

Parameter:
+ NAME:
  + string: `coca.objc::*objc-struct-names*' -> `coca.objc::*objc-structs*'
  + symbol: `coca.objc::*objc-structs*'
  + (:struct symbol): `coca.objc::*objc-structs*'
"
  (etypecase name
    (string (objc-struct (or (gethash name *objc-struct-names*)
                             (error "Unknown ObjC struct ~A. " name))))
    (symbol (or (gethash name *objc-structs*)
                (error "Unknown ObjC struct ~S. " name)))
    ((cons (eql :struct) (cons symbol null))
     (objc-struct (second name)))))

(defun regist-objc-struct (lisp-name objc-name slots encodings coerce-p)
  "Register ObjC struct in `coca.objc::*objc-structs*'.
Return `coca.objc::objc-struct'.

Parameters:
+ LISP-NAME: lisp symbol of ObjC struct
+ OBJC-NAME: string of ObjC struct name
+ SLOTS:     list of struct name symbol
+ ENCODINGS: list of `objc-basic-encoding'
+ COERCE-P:  list of boolean, whether or not converting struct values
"
  (let* ((len      (length encodings))
         (elements (foreign-alloc :pointer :count (1+ len))))
    (loop :for i :from 0
          :for encoding :in encodings
          :do (setf (mem-aref elements :pointer i)
                    (objc-encoding-ffi-type encoding)))
    (setf (mem-aref elements :pointer len) (null-pointer))
    (let* ((ffi-type (coca_alloc_struct_ffi_type elements))
           (struct   (make-objc-struct :objc-name objc-name
                                       :lisp-name lisp-name
                                       :slots     slots
                                       :encodings encodings
                                       :coerce-p  coerce-p
                                       :ffi-type  ffi-type)))
      (tg:finalize struct (lambda ()
                            (foreign-free ffi-type)
                            (foreign-free elements)))
      (setf (gethash objc-name *objc-struct-names*) lisp-name)
      (setf (gethash lisp-name *objc-structs*)      struct)
      struct)))

(defgeneric struct-aref (struct offset)
  (:documentation "Get data at OFFSET for STRUCT. ")
  (:method ((struct simple-vector) (offset integer))
    (svref struct offset)))


;;; objc-anonymous-struct

(defun objc-anonymous-struct (types)
  "Generate anonymous struct like {?=...}. "
  (let* ((encoding (format nil "{?=~A}" (encode-objc-type-encoding types)))
         (name     (with-cached (encoding *objc-struct-names*)
                     (let ((name     (intern encoding))
                           (slots    (loop :repeat (length types)
                                           :for i :from 0
                                           :collect (intern (format nil "S~D" i))))
                           (coerce-p (loop :repeat (length types)
                                           :collect nil)))
                       (eval `(defstruct  ,name ,@slots))
                       (eval `(defcstruct ,name
                                ,@(loop :for slot :in slots
                                        :for type :in types
                                        :collect (list slot
                                                       (objc-encoding-cffi-type
                                                        type)))))
                       (regist-objc-struct name encoding slots types coerce-p)
                       name))))
    (objc-struct name)))


;;; objc-encoding

(defparameter *objc-encoding-aliases* (make-hash-table)
  "Aliases of ObjC encoding.

KEY: symbol of ObjC encoding
VAL: `objc-basic-encoding'. ")

(deftype objc-basic-encoding ()
  "Basic ObjC encoding.

    ObjC Type Encoding       objc-basic-encoding
    c                        :char
    C                        :unsigned-char
    i                        :int
    I                        :unsigned-int
    l                        :long
    L                        :unsigned-long
    q                        :long-long
    Q                        :unsigned-long-long
    f                        :float
    d                        :double
    B                        :bool
    v                        :void
    *                        :string
    @                        :object
    #                        :class
    :                        :sel
    [len type]               (:array len encoding)
    {NAME=type...}           (:struct NAME)
    (NAME=type...)           (:union  NAME)
    bNUM                     (:bits   NUM)
    ^type                    :pointer
    ?                        :unknown
"
  `(or (member :char      :unsigned-char
               :int       :unsigned-int
               :short     :unsigned-short
               :long      :unsigned-long
               :long-long :unsigned-long-long
               :float     :double
               :bool      :void
               :string    :object
               :class     :sel
               :unknown   :pointer)
       (cons (member :struct :union) (cons symbol  null))
       (cons (member :bits)          (cons integer null))
       (cons (member :array)         (cons integer (cons symbol null)))))

(defun as-objc-encoding (encoding)
  "Convert `objc-encoding' ENCODING as `objc-basic-encoding'. "
  (the objc-basic-encoding
    (etypecase encoding
      (objc-basic-encoding encoding)
      (symbol  (cond ((gethash encoding *objc-encoding-aliases*)
                      (gethash encoding *objc-encoding-aliases*))
                     ((subtypep encoding 'sel)                  :sel)
                     ((subtypep encoding 'standard-objc-object) :object)
                     ((subtypep encoding 'objc-class)           :class)
                     ((subtypep encoding 'objc-pointer)         :pointer)
                     (t (error "Unknown ObjC encoding ~S. " encoding)))))))

(defun set-objc-encoding-alias (alias objc-encoding)
  "Set ALIAS for OBJC-ENCODING. "
  (setf (gethash alias *objc-encoding-aliases*)
        (as-objc-encoding objc-encoding)))

(deftype objc-encoding ()
  "Type of ObjC encoding representation in lisp.

See also `objc-basic-encoding'. "
  `(or objc-basic-encoding
       (satisfies objc-encoding-p)))

(defun objc-encoding-p (encoding)
  "Test if ENCODING is valid `objc-encoding'.
Return `t' if ENCODING is `objc-basic-encoding'
or alias of `objc-basic-encoding', otherwise, return `nil'. "
  (the boolean
    (typecase encoding
      (objc-basic-encoding t)
      (symbol (and (or (gethash encoding *objc-encoding-aliases*)
                       (subtypep encoding 'objc-pointer)
                       nil)
                   t)))))

;; objc-encoding-* series functions should use `objc-basic-encoding'

(defun objc-encoding-init-value (encoding)
  "Initial value of ObjC type encoding in TYPE.
Return initial type (unevaled). "
  (declare (type objc-basic-encoding encoding))
  (ecase (atomize encoding)
    (:float   0.0f0)
    (:double  0.0d0)
    (:bool    nil)
    (:string  "")
    ((:pointer :unknown)    '(null-pointer))
    ((:char  :unsigned-char) #\L)
    ((:class :sel :object)   nil)
    ((:int       :unsigned-int
      :short     :unsigned-short
      :long      :unsigned-long
      :long-long :unsigned-long-long)
     0)
    (:bits   0)
    (:array
     `(make-array
       ,(second encoding)
       :element-type  ',(objc-encoding-lisp-type (third encoding))
       :initial-value ,(objc-encoding-init-value (third encoding))))
    (:struct
     `(,(intern (str:concat "MAKE-" (symbol-name (second encoding)))
                (symbol-package (second encoding)))))))

(defun objc-encoding-cffi-type (encoding)
  "CFFI type for ObjC type encoding TYPE.
Return CFFI type. "
  (declare (type objc-basic-encoding encoding))
  (etypecase encoding
    (keyword (case encoding
               ((:unknown :class :sel :object :pointer) :pointer)
               (otherwise encoding)))
    (list    (ecase (car encoding)
               (:struct  encoding)
               (:array   (error "Failed for :array"))))))

(defun objc-encoding-lisp-type (type)
  "Lisp type for ObjC type encoding TYPE.
Return lisp type declaration. "
  (declare (type (or objc-basic-encoding symbol) type))
  (etypecase type
    (keyword (case type
               ((:char :unsigned-char)                  'character)
               ((:int :short)                           '(signed-byte   32))
               ((:unsigned-int :unsigned-short)         '(unsigned-byte 32))
               ((:long :long-long)                      '(signed-byte   64))
               ((:unsigned-long :unsigned-long-long)    '(unsigned-byte 64))
               (:float                                  'single-float)
               (:double                                 'double-float)
               (:bool                                   'boolean)
               ((:unknown :pointer)                     'foreign-pointer)
               (:class                                  'objc-class)
               (:object                                 'standard-objc-object)
               (:string                                 'string)
               (:sel                                    'sel)
               (otherwise (objc-encoding-lisp-type (as-objc-encoding type)))))
    (symbol (cond ((subtypep type 'standard-objc-object)  type)
                  ((subtypep type 'objc-class)           'objc-class)
                  ((subtypep type 'sel)                  'sel)
                  (t (objc-encoding-lisp-type (as-objc-encoding type)))))
    (list   (ecase (first type)
              (:struct  (and (objc-struct (second type))
                             (second type)))
              (:array   `(simple-array ,(objc-encoding-lisp-type (third type))
                                       ,(second type)))
              (:pointer 'foreign-pointer)))))


;;; objc-encoding-ffi-type -> ffi_type

(defparameter +ffi_type_sint8+   (foreign-symbol-pointer "ffi_type_sint8"))
(defparameter +ffi_type_uint8+   (foreign-symbol-pointer "ffi_type_uint8"))
(defparameter +ffi_type_sint16+  (foreign-symbol-pointer "ffi_type_sint16"))
(defparameter +ffi_type_uint16+  (foreign-symbol-pointer "ffi_type_uint16"))
(defparameter +ffi_type_sint32+  (foreign-symbol-pointer "ffi_type_sint32"))
(defparameter +ffi_type_uint32+  (foreign-symbol-pointer "ffi_type_uint32"))
(defparameter +ffi_type_sint64+  (foreign-symbol-pointer "ffi_type_sint64"))
(defparameter +ffi_type_uint64+  (foreign-symbol-pointer "ffi_type_uint64"))
(defparameter +ffi_type_float+   (foreign-symbol-pointer "ffi_type_float"))
(defparameter +ffi_type_double+  (foreign-symbol-pointer "ffi_type_double"))
(defparameter +ffi_type_pointer+ (foreign-symbol-pointer "ffi_type_pointer"))
(defparameter +ffi_type_void+    (foreign-symbol-pointer "ffi_type_void"))

(defun objc-encoding-ffi-type (encoding)
  "Return ffi_type of ObjC encoding ENCODING.

Parameter:
+ ENCODING: `coca.objc::objc-basic-encoding'
"
  (declare (type objc-basic-encoding encoding))
  (ecase (atomize encoding)
    ((:char)                                           +ffi_type_sint8+)
    ((:unsigned-char :bool)                            +ffi_type_uint8+)
    ((:int)                                            +ffi_type_sint32+)
    ((:unsigned-int)                                   +ffi_type_uint32+)
    ((:short)                                          +ffi_type_sint16+)
    ((:unsigned-short)                                 +ffi_type_uint16+)
    ((:long :long-long)                                +ffi_type_sint64+)
    ((:unsigned-long :unsigned-long-long)              +ffi_type_uint64+)
    ((:float)                                          +ffi_type_float+)
    ((:double)                                         +ffi_type_double+)
    ((:pointer :object :sel :class :unknown :string)   +ffi_type_pointer+)
    ((:void)                                           +ffi_type_void+)
    ((:struct) (objc-struct-ffi-type (objc-struct (second encoding))))))


;;; Decode/Encode type encoding

(defun decode-objc-type-encoding (encoding)
  "Parse ObjC type encoding string.
Return values are arg-types, result-type, type-encoding.

Dev Note:
The encoding is cached in `*encodings*'.

  <type>    := :char | :int | :short | ... ;; see below
  <array>   := (:array len <type>)
  <struct>  := (:struct name (<type>...))
  <union>   := (:union name (<type>...))

ObjC type encoding: (
  Code                CFFI type             ObjC encoding       Meaning
  c                   :char                 :char               A char
  i                   :int                  :int                An int
  s                   :short                :short              A short
  l                   :long                 :long               A long
  q                   :long-long            :long-long          A long long
  C                   :unsigned-char        :unsigned-char      An unsigned char
  I                   :unsigned-int         :unsigned-int       An unsigned int
  S                   :unsigned-short       :unsigned-short     An unsigned short
  L                   :unsigned-long        :unsigned-long      An unsigned long
  Q                   :unsigned-long-long   :unsigned-long-long An unsigned long long
  f                   :float                :float              A float
  d                   :double               :double             A double
  B                   :bool                 :bool               A C++ bool or a C99 _Bool
  v                   :void                 :void               A void
  *                   :string               :string             A character string (char *)
  @                   :pointer              :object             An object (whether statically typed or typed id)
  #                   :pointer              :class              A class object (Class)
  :                   :pointer              :sel                A method selector (SEL)
  [array type]        :pointer              (:array len enc)    An array
  {name=type . ..}    (:struct NAME)        (:struct NAME)      A structure
  (name=type . ..)    *                     (:union  NAME)      A union
  bnum                *                     (:bits NUM)         A bit field of num bits
  ^type               :pointer              :pointer            A pointer to type
  ?                   :pointer              :unknown            An unknown type

ObjC method encoding: (would be ignored)
  r                         const
  n                         in
  N                         inout
  o                         out
  O                         bycopy
  R                         byref
  V                         oneway

see also:
https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ObjCRuntimeGuide/Articles/ocrtTypeEncodings.html
"
  (declare (type string encoding))
  (let ((len (length encoding)))
    (labels ((parse-arr     (pos) ; [<len><type>]  => (:array len type)
               ;; [12^f]
               ;; ^
               ;; pos
               ;; (assert (char= (svref pos pos) #\[))
               (multiple-value-bind (len pos)
                   (parse-integer encoding :start (1+ pos) :junk-allowed t)
                 ;; [12^f]
                 ;;    ^
                 ;;    pos
                 (multiple-value-bind (type pos)
                     (parse-type pos)
                   ;; [12^f]
                   ;;      ^
                   ;;      pos
                   (assert (char= (aref encoding pos) #\]))
                   (values (list :array len type)
                           (1+ pos)))))
             (parse-name    (pos) ; (rx (not (or #\= #\} #\))))
               (loop :for i :from pos :below len
                     :for chr := (aref encoding i)
                     :until (member chr '(#\= #\} #\)))
                     :finally (return (values (subseq encoding pos i) (1+ i)))))
             (parse-struct  (pos) ; {name=type...) => (:struct name)
               ;; {name=type...}
               ;; ^
               ;; pos
               ;; (assert (char= (svref pos pos) #\{))
               (multiple-value-bind (name pos)
                   (parse-name (1+ pos))
                 (if (string= name "?") ;; anonymous struct
                     (loop :with (type pos*) := (list nil pos)
                           :while (char/= (aref encoding pos*) #\})
                           :do (multiple-value-setq (type pos*)
                                 (parse-type pos*))
                           :collect type :into types
                           :finally (return
                                      (values (list :struct
                                                    (objc-struct-lisp-name
                                                     (objc-anonymous-struct types)))
                                              (1+ pos*))))
                     (let ((name (objc-struct-lisp-name (objc-struct name))))
                       (loop :with (type pos*) := (list nil pos)
                             :while (char/= (aref encoding pos*) #\})
                             :do (multiple-value-setq (type pos*)
                                   (parse-type pos*))
                             :finally (return (values (list :struct name)
                                                      (1+ pos*))))))))
             (parse-union   (pos) ; (name=type...) => (:union  name (type...))
               ;; (name=type...)
               ;; ^
               ;; pos
               ;; (assert (char= (svref pos pos) #\())
               (multiple-value-bind (name pos)
                   (parse-name (1+ pos))
                 (loop :with (type pos*) := (list nil pos)
                       :while (char/= (aref encoding pos*) #\))
                       :do (multiple-value-setq (type pos*)
                             (parse-type pos*))
                       :collect type :into types
                       ;; (name=type...)
                       ;;              ^
                       ;;              pos
                       :finally (return (values (list :struct name types)
                                                (1+ pos*))))))
             (parse-pointer (pos) ; ^<type>        => (:pointer type)
               ;; ^f
               ;; ^
               ;; pos
               ;; (assert (char= (svref pos pos) #\^))
               (multiple-value-bind (type pos)
                   (parse-type (1+ pos))
                 (declare (ignore type))
                 (values :pointer pos)))
             (parse-bits    (pos) ; b<num>         => (:bits num)
               ;; b8
               ;; ^
               ;; pos
               (multiple-value-bind (num pos)
                   (parse-integer encoding :start (1+ pos) :junk-allowed t)
                 (values (list :bits num) (1+ pos))))
             (parse-type    (pos)
               (ecase (aref encoding pos)
                 (#\c (values :char                 (1+ pos)))
                 (#\C (values :unsigned-char        (1+ pos)))
                 (#\i (values :int                  (1+ pos)))
                 (#\I (values :unsigned-int         (1+ pos)))
                 (#\s (values :short                (1+ pos)))
                 (#\S (values :unsigned-short       (1+ pos)))
                 (#\l (values :long                 (1+ pos)))
                 (#\L (values :unsigned-long        (1+ pos)))
                 (#\q (values :long-long            (1+ pos)))
                 (#\Q (values :unsigned-long-long   (1+ pos)))
                 (#\f (values :float                (1+ pos)))
                 (#\d (values :double               (1+ pos)))
                 (#\B (values :bool                 (1+ pos)))
                 (#\v (values :void                 (1+ pos)))
                 (#\* (values :string               (1+ pos)))
                 (#\@ (values :object               (1+ pos)))
                 (#\# (values :class                (1+ pos)))
                 (#\: (values :sel                  (1+ pos)))
                 (#\? (values :unknown              (1+ pos)))
                 (#\[ (parse-arr     pos))
                 (#\{ (parse-struct  pos))
                 (#\( (parse-union   pos))
                 (#\b (parse-bits    pos))
                 (#\^ (parse-pointer pos))
                 ((#\r #\n #\N #\o #\O #\R #\V)
                  ;; ignore qualifiers
                  (parse-type (1+ pos))))))
      (loop :with (type pos) := '(nil 0)
            :do (multiple-value-setq (type pos) (parse-type pos))
            :collect type :into encodings
            ;; ignore arg size
            :do (multiple-value-setq (type pos)
                  (parse-integer encoding :start pos :junk-allowed t))
            :while (< pos len)
            :finally (return (values (rest encodings) (first encodings) encoding))))))

(defun encode-objc-type-encoding (type-list)
  "Encode TYPE-LIST as OBJC type encoding string.
Return ObjC type encoding string.

Example:

    (encode-objc-type-encoding '(:void :object :sel)) ;; => \"v@:\"
"
  (with-output-to-string (enc)
    (labels ((fmt (type)
               (ecase (atomize type)
                 (:char               (write-char #\c enc))
                 (:unsigned-char      (write-char #\C enc))
                 (:int                (write-char #\i enc))
                 (:unsigned-int       (write-char #\I enc))
                 (:short              (write-char #\s enc))
                 (:unsigned-short     (write-char #\s enc))
                 (:long               (write-char #\l enc))
                 (:unsigned-long      (write-char #\L enc))
                 (:long-long          (write-char #\q enc))
                 (:unsigned-long-long (write-char #\Q enc))
                 (:float              (write-char #\f enc))
                 (:double             (write-char #\d enc))
                 (:bool               (write-char #\B enc))
                 (:void               (write-char #\v enc))
                 (:string             (write-char #\* enc))
                 (:object             (write-char #\@ enc))
                 (:class              (write-char #\# enc))
                 (:sel                (write-char #\: enc))
                 (:unknown            (write-char #\? enc))
                 (:union              (format enc "(~A)" (second type)))
                 (:bits               (format enc "b~D"  (second type)))
                 (:pointer            (write-char #\^ enc)
                  (let ((type (second (listfy type))))
                    (if type (fmt type) (write-char #\? enc))))
                 (:struct
                  (format enc "{~A}" (objc-struct-objc-name
                                      (objc-struct (second type))))))))
      (dolist (type type-list)
        (fmt (as-objc-encoding type))))))

;;;; encoding.lisp ends here
