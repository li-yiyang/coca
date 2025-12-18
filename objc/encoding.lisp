;;;; encoding.lisp --- Parse and manage ObjC type encoding

(in-package :coca.objc)

(deftype objc-encoding ()
  "Type of ObjC encoding representation in lisp.

Dev Note:
Method related with `objc-encoding' should prefix with `OBJC-ENCODING-'.
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
               :unknown   :pointer
               :pointer   :bits)
       (cons (member :struct :union) (cons string t))
       (cons (eql :array) t)
       (cons (eql :bits)  (cons number null))))

(defun objc-encoding-init-value (type)
  "Initial value of ObjC type encoding in TYPE.
Return initial type (unevaled). "
  (declare (type objc-encoding type))
  (etypecase type
    (keyword (ecase type
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
                0)))
    (list    (ecase (car type)
               (:bits   0)
               (:struct `(,(intern (str:concat "MAKE-" (symbol-name (second type))))))))))

(defun objc-encoding-cffi-type (type)
  "CFFI type for ObjC type encoding TYPE.
Return CFFI type. "
  (declare (type objc-encoding type))
  (etypecase type
    (keyword (case type
               ((:unknown :class :sel :object :pointer) :pointer)
               (otherwise type)))
    (list    (ecase (car type)
               (:struct type)))))

(defun objc-encoding-lisp-type (type)
  "Lisp type for ObjC type encoding TYPE.
Return lisp type. "
  (declare (type objc-encoding type))
  (etypecase type
    (keyword (ecase type
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
               (:sel                                    'sel)))))

(defparameter *encodings* (make-hash-table :test 'equal)
  "Cache of ObjC parsed type encoding.

KEY: type encoding string
VAL: parsed type encoding string")

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
  Code                CFFI type             Meaning
  c                   :char                 A char
  i                   :int                  An int
  s                   :short                A short
  l                   :long                 A long
  q                   :long-long            A long long
  C                   :unsigned-char        An unsigned char
  I                   :unsigned-int         An unsigned int
  S                   :unsigned-short       An unsigned short
  L                   :unsigned-long        An unsigned long
  Q                   :unsigned-long-long   An unsigned long long
  f                   :float                A float
  d                   :double               A double
  B                   :bool                 A C++ bool or a C99 _Bool
  v                   :void                 A void
  *                   :string               A character string (char *)
  @                   :pointer              An object (whether statically typed or typed id)
  #                   :pointer              A class object (Class)
  :                   :pointer              A method selector (SEL)
  [array type]        :pointer              An array
  {name=type . ..}    (:struct NAME)        A structure
  (name=type . ..)    *                     A union
  bnum                *                     A bit field of num bits
  ^type               :pointer              A pointer to type
  ?                   :pointer              An unknown type

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
  (let ((encodings
          (with-cached (encoding *encodings*)
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
                               (parse-type (1+ pos))
                             ;; [12^f]
                             ;;      ^
                             ;;      pos
                             (assert (char= (aref encoding (1+ pos)) #\]))
                             (values (list :array type len)
                                     (+ 2 pos)))))
                       (parse-name    (pos) ; (rx (not #\=))
                         (loop :for i :from pos :below len
                               :for chr := (aref encoding i)
                               :until (member chr '(#\= #\} #\)))
                               :finally (return (values (subseq encoding pos i) (1+ i)))))
                       (parse-struct  (pos) ; {name=type...) => (:struct name (type...))
                         ;; {name=type...}
                         ;; ^
                         ;; pos
                         ;; (assert (char= (svref pos pos) #\{))
                         (multiple-value-bind (name pos)
                             (parse-name (1+ pos))
                           (let ((name (objc-struct-name name)))
                             (loop :with (type pos*) := (list nil pos)
                                   :while (char/= (aref encoding pos*) #\})
                                   :do (multiple-value-setq (type pos*)
                                         (parse-type pos*))
                                   :finally (return (values (list :struct name)
                                                            (1+ pos*)))))))
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
                           (values (list :pointer type) pos)))
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
                            ;; parse qualifiers
                            ;; (let ((qualifiers (ecase (aref encoding pos)
                            ;;                     (#\r :const)
                            ;;                     (#\n :in)
                            ;;                     (#\N :inout)
                            ;;                     (#\o :out)
                            ;;                     (#\O :bycopy)
                            ;;                     (#\R :byref)
                            ;;                     (#\V :oneway))))
                            ;;   (multiple-value-bind (type pos)
                            ;;       (parse-type (1+ pos))
                            ;;     (values (cons qualifiers type) pos)))
                            ;; ignore qualifiers
                            (parse-type (1+ pos))))))
                (loop :with (type pos) := '(nil 0)
                      :do (multiple-value-setq (type pos) (parse-type pos))
                      :collect (the objc-encoding type)
                      ;; ignore arg size
                      :do (multiple-value-setq (type pos)
                            (parse-integer encoding :start pos :junk-allowed t))
                      :while (< pos len)))))))
    (values (rest encodings) (first encodings) encoding)))

;; TODO:
;; (defun encode-objc-type-encoding (type-list)
;;   "Encode TYPE-LIST as OBJC type encoding string. "
;;   (with-output-to-string (enc)
;;     (labels ((fmt (type)
;;                (ecase (atomize type)
;;                  (:char               (write-char #\c enc))
;;                  (:unsigned-char      (write-char #\C enc))
;;                  (:int                (write-char #\i enc))
;;                  (:unsigned-int       (write-char #\I enc))
;;                  (:short              (write-char #\s enc))
;;                  (:unsigned-short     (write-char #\s enc))
;;                  (:long               (write-char #\l enc))
;;                  (:unsigned-long      (write-char #\L enc))
;;                  (:long-long          (write-char #\q enc))
;;                  (:unsigned-long-long (write-char #\Q enc))
;;                  (:float              (write-char #\f enc))
;;                  (:double             (write-char #\d enc))
;;                  (:bool               (write-char #\B enc))
;;                  (:void               (write-char #\v enc))
;;                  (:string             (write-char #\* enc))
;;                  (:object             (write-char #\@ enc))
;;                  (:class              (write-char #\# enc))
;;                  (:sel                (write-char #\: enc))
;;                  (:unknown            (write-char #\? enc))
;;                  (:union              (format enc "(~A)" (second type)))
;;                  (:bits               (format enc "b~D"  (second type)))
;;                  (:pointer            (write-char #\^ enc)
;;                   (let ((type (second (listfy type))))
;;                     (if type (fmt type) (write-char #\? enc))))
;;                  (:struct
;;                   (format enc "{~A}"
;;                           (or (car (gethash (second type) *lisp-structs*))
;;                               (error "Unknown Lisp struct `~S' in ObjC runtime. "
;;                                      (second type))))))))
;;       (dolist (type type-list)
;;         (fmt type)))))

;;;; encoding.lisp ends here
