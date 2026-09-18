;;;; runtime-inspect.lisp --- Inspect ObjC runtime

(in-package :coca.objc)


;;;; objc-class hierarchy

(defun objc-class-list ()
  "Return a list of all the ObjC classes. "
  (flet ((get-class-list (arr num)
           (foreign-funcall "objc_getClassList"
                            :pointer arr
                            :int     num
                            :int)))
    (let ((n (get-class-list (null-pointer) 0)))
      (with-foreign-object (arr :pointer n)
        (get-class-list arr n)
        (loop :for i :below n
              :collect (coerce-to-objc-class (mem-aref arr :pointer i)))))))

(defun objc-class-superclass (objc-class
                              &aux (class (coerce-to-objc-class objc-class)))
  "Return the superclass of OBJC-CLASS.

If the OBJC-CLASS is the root class, its superclass is `t',
otherwise, return `objc-class'. "
  (let ((ptr (foreign-funcall "class_getSuperclass"
                              :pointer (objc-class-ptr class)
                              :pointer)))
    (if (null-pointer-p ptr)
        t
        (coerce-to-objc-class ptr))))

(defun objc-class-subclassp (class superclass)
  "Test if CLASS is subclass of SUPERCLASS. "
  (eq (objc-class-superclass class)
      (coerce-to-objc-class superclass)))

;; Dev Note: this is slow...
;; but anyway, for dev-only usage, it's acceptable
(defun objc-class-subclasses (objc-class
                              &aux (class (coerce-to-objc-class objc-class)))
  "Return a list of subclasses of OBJC-CLASS. "
  (loop :for subclass :in (objc-class-list)
        :if (objc-class-subclassp subclass class)
          :collect subclass))


;;;; encoding

;; Ref: https://github.com/li-yiyang/coca-libffi/

(defun decode-objc-type-encoding (encoding)
  "Parse ObjC type encoding string ENCODING.
Return values are list ObjC encoding, and ENCODING string.

Dev Note:

  <type>    := :char | :int | :short | ... ;; see below
  <array>   := (:array len <type>)
  <struct>  := (:struct name (<type>...))
  <union>   := (:union name (<type>...))

ObjC type encoding: (
  Code             ObjC encoding       Meaning
  c                :char               A char
  i                :int                An int
  s                :short              A short
  l                :long               A long
  q                :long-long          A long long
  C                :unsigned-char      An unsigned char
  I                :unsigned-int       An unsigned int
  S                :unsigned-short     An unsigned short
  L                :unsigned-long      An unsigned long
  Q                :unsigned-long-long An unsigned long long
  f                :float              A float
  d                :double             A double
  B                :bool               A C++ bool or a C99 _Bool
  v                :void               A void
  *                :string             A character string (char *)
  @                :object             An object (whether statically typed or typed id)
  #                :class              A class object (Class)
  :                :sel                A method selector (SEL)
  [array type]     (:array len enc)    An array
  {name=type . ..} (:struct NAME)      A structure
  (name=type . ..) (:union  NAME)      A union
  bnum             (:bits NUM)         A bit field of num bits
  ^type            :pointer            A pointer to type
  ?                :unknown            An unknown type

ObjC method encoding:
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
                 (loop :with (type pos*) := (list nil pos)
                       :while (char/= (aref encoding pos*) #\})
                       :do (multiple-value-setq (type pos*)
                             (parse-type pos*))
                       :collect type :into types
                       :finally (return
                                  (values `(:struct ,name ,@types)
                                          (1+ pos*))))))
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
                  (multiple-value-bind (type next)
                      (parse-type (1+ pos))
                    (values (cons (ecase (aref encoding pos)
                                    (#\r :const)
                                    (#\n :in)
                                    (#\N :inout)
                                    (#\o :out)
                                    (#\O :bycopy)
                                    (#\R :byref)
                                    (#\V :oneway))
                                  (case (alx:ensure-car type)
                                    ((:struct :array :pointer :union :bits)
                                     (list type))
                                    (otherwise
                                     (alx:ensure-list type))))
                            next))))))
      (loop :with (type pos) := '(nil 0)
            :do (multiple-value-setq (type pos) (parse-type pos))
            :collect type :into encodings
            ;; ignore arg size
            :do (multiple-value-setq (type pos)
                  (parse-integer encoding :start pos :junk-allowed t))
            :while (< pos len)
            :finally (return (values (the list   encodings)
                                     (the string encoding)))))))

(defun encode-objc-type-encoding (objc-encoding-list)
  "Encode OBJC-ENCODING-LIST as ObjC type encoding string.
Return ObjC type encoding string.

Example:

    (encode-objc-type-encoding '(:void :object :sel)) ;; => \"v@:\"
"
  (with-output-to-string (enc)
    (labels ((fmt (type)
               (ecase (alx:ensure-car type)
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
                  (let ((type (second (alx:ensure-list type))))
                    (if type (fmt type) (write-char #\? enc))))
                 (:struct
                  (destructuring-bind (name &rest types)
                      (cdr type)
                    (format enc "{~A" name)
                    (dolist (type types)
                      (fmt type))
                    (format enc "}")))
                 ((:const :in :inout :out :bycopy :byref :oneway)
                  (loop :for mod :in type
                        :do (case mod
                              (:const  (write-char #\r enc))
                              (:in     (write-char #\n enc))
                              (:inout  (write-char #\N enc))
                              (:out    (write-char #\o enc))
                              (:bycopy (write-char #\O enc))
                              (:byref  (write-char #\R enc))
                              (:oneway (write-char #\V enc))
                              (otherwise
                               (fmt mod)
                               (return nil))))))))
      (dolist (encoding objc-encoding-list)
        (fmt encoding)))))


;;;; methods

(defun objc-method-encoding (method)
  (declare (type foreign-pointer method))
  (multiple-value-bind (list encoding)
      (decode-objc-type-encoding
       (foreign-funcall "method_getTypeEncoding" :pointer method :string))
    (values (rest list) (first list) encoding)))

(defun objc-class-instance-method (objc-class sel)
  (declare (type objc-class objc-class)
           (type sel sel))
  (foreign-funcall "class_getInstanceMethod"
                   :pointer (objc-class-ptr objc-class)
                   :pointer (sel-ptr sel)
                   :pointer))

(defun objc-class-class-method (objc-class sel)
  (declare (type objc-class objc-class)
           (type sel sel))
  (foreign-funcall "class_getClassMethod"
                   :pointer (objc-class-ptr objc-class)
                   :pointer (sel-ptr sel)
                   :pointer))

(defun objc-class-instance-method-encoding
    (objc-class-or-object sel
     &optional (errorp t) (objc-class-p nil objc-class-p?))
  "Get class instance method encoding of OBJC-CLASS and SEL.
Return values are argument encoding list, return type and encoding string.
If not found and not ERRORP, return `nil'.

Parameters:
+ OBJC-CLASS-OR-OBJECT:
  + foreign-pointer to object
  + otherwise, would be `coerce-to-objc-class'
+ SEL: would be `coerce-to-selector' first
+ ERRORP: if or not error when class method is not found
+ OBJC-CLASS-P:
  if non-nil, assume OBJC-CLASS-OR-OBJECT is pointer to ObjC class
"
  (let* ((class  (if (typep objc-class-or-object 'foreign-pointer)
                     (if (if objc-class-p?
                             objc-class-p
                             (foreign-funcall "object_isClass"
                                              :pointer objc-class-or-object
                                              :bool))
                         (coerce-to-objc-class objc-class-or-object)
                         (coerce-to-objc-class
                          (foreign-funcall "object_getClass"
                                           :pointer objc-class-or-object
                                           :pointer)))
                     (coerce-to-objc-class objc-class-or-object)))
         (sel    (coerce-to-selector sel))
         (method (objc-class-instance-method class sel)))
    (if (null-pointer-p method)
        (when errorp
          (error "No instance method ~A for ~A. " sel class))
        (objc-method-encoding method))))

(defun objc-class-class-method-encoding (objc-class sel &optional (errorp t))
  "Get class method encoding of OBJC-CLASS and SEL.
Return values are argument encoding list, return type and encoding string.
If not found and not ERRORP, return `nil'.

Parameters:
+ OBJC-CLASS: would be `coerce-to-objc-class' first
+ SEL: would be `coerce-to-selector' first
+ ERRORP: if or not error when class method is not found
"
  (let* ((class  (coerce-to-objc-class objc-class))
         (sel    (coerce-to-selector sel))
         (method (objc-class-class-method class sel)))
    (if (null-pointer-p method)
        (when errorp
          (error "No class method ~A for ~A. " sel class))
        (objc-method-encoding method))))

;;;; runtime-inspect.lisp ends here
