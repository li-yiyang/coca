;;;; invoke.lisp --- Wrapper or objc_msgSend (invoke) and objc_msgSendSuper (invoke-super)

(in-package :coca.objc)

;;; Invoke

;; `invoke' and `invoke-super' is a thing wrapper layer
;; on top of `cffi:foreign-funcall'.
;;
;; Use `invoke' and `invoke-super' directly if the method
;; calling is only used once or twice.
;;
;; Wrap the `invoke' and `invoke-super' into a function
;; calling is helpful to save some space.

(defmacro invoke (class-or-object method &rest args)
  "Invokes an ObjC method that returns RESULT CFFI type.

Syntax:

    (invoke CLASS-OR-OBJECT METHOD
            { OBJC-TYPE VAR }*
            [ (RESULT :void) ])

Parameters:
+ CLASS-OR-OBJECT: things can be converted via `objc-pointer'
+ METHOD: a string naming the method to invoke
+ OBJC-TYPE: extended CFFI type

  Built-in ObjC typing:
  + :object        expecting to be `objc-object'
  + :class         expecting to be `objc-class'
  + :sel           expecting to be `sel'
  + :ns-string     convert between lisp string and NSString
  + :ns-url        convert between lisp string/pathname and NSURL
  + :ns-uint       alias as :unsigned-long
  + :ns-int        alias as :long
  + :ns-number     convert lisp values to NSNumber
                   adviced to use `ns-number-value' to get NSNumber
                   value back as lisp type
  + :ns-array      convert NSArray into a list of foreign-pointer
                   to NSObject
  + :ns-dictionary input syntax

                       ((KEY VAL)
                        ;; key with nested NSDictionary
                        (KEY ((KEY VAL)
                              (KEY VAL))))

                   creates NSMutableDictionary behind the scene

  Built-in ObjC typing in `coca/objc/block' subsystem:
  + :block    expecting (BLOCK-TYPE (lambda-list ...) ,@body)
    see `define-objc-block' for details

  Built-in ObjC typing in `coca/appkit' subsystem:
  + :ns-rect  expecting (X Y W H) inline value passing
  + :ns-point expecting (X Y) inline value passing
  + :ns-size  expecting (W H) inline value passing

  Use `define-objc-typing' to define new ObjC typing.
+ RESULT:

  Built-in ObjC typing:
  + :ns-array return `list' of `foreign-pointer'

  Use `define-objc-typing' to define new ObjC typing.

  If not provided, by default as `:void'

See also `invoke-super'. "
  (declare (type list args))
  (let* ((resultp (oddp (length args)))
         (result  (if resultp (car (last args)) :void))
         (args    (if resultp (butlast args) args)))
    (multiple-value-bind (args rewrites)
        (expand-invoke-args args)
      (let ((cffi-call
              `(foreign-funcall
                "objc_msgSend"
                :pointer ,(m:match class-or-object
                            ;; literal ObjC CLASS
                            ((and (type string) class)
                             `(objc-class-ptr ,(coerce-to-objc-class class)))
                            ;; (coerce-to-objc-class <literal ObjC CLASS>)
                            ((list 'coerce-to-objc-class (and (type string) class))
                             `(objc-class-ptr ,(coerce-to-objc-class class)))
                            ;; (coerce-to-objc-class CLASS)
                            ((list 'coerce-to-objc-class class)
                             `(objc-class-ptr (coerce-to-objc-class ,class)))
                            (_ `(objc-pointer ,class-or-object)))
                :pointer (sel-ptr ,(if (stringp method)
                                       (coerce-to-selector method)
                                       `(coerce-to-selector ,method)))
                ,@args
                ,(invoke-result-cffi-type result))))
        (rewrite-invoke rewrites (wrap-invoke-result result cffi-call))))))

(defcstruct objc-super
  (self        :pointer)
  (super-class :pointer))

(defun %fill-objc-super (super object)
  "Fill foreign SUPER (`objc-super') C struct with OBJECT. "
  (declare (type foreign-pointer super object))
  (macrolet ((slot (slot)
               `(foreign-slot-value super '(:struct objc-super) ',slot)))
    (setf (slot self)        object
          (slot super-class) (foreign-funcall
                              "class_getSuperclass"
                              :pointer (foreign-funcall "object_getClass"
                                                        :pointer object
                                                        :pointer)
                              :pointer))))

(defmacro invoke-super (object method &rest args)
  "Invokes an ObjC method that returns RESULT CFFI type.

Syntax:

    (invoke-super OBJECT METHOD
                  { OBJC-TYPE VAR }*
                  [ (RESULT :void) ])

Parameters:
+ OBJECT: `objc-object'
+ METHOD: a string naming the method to invoke
+ OBJC-TYPE: extended CFFI type

  Built-in ObjC typing:
  + :object        expecting to be `objc-object'
  + :class         expecting to be `objc-class'
  + :sel           expecting to be `sel'
  + :ns-string     convert between lisp string and NSString
  + :ns-url        convert between lisp string/pathname and NSURL
  + :ns-uint       alias as :unsigned-long
  + :ns-int        alias as :long
  + :ns-number     convert lisp values to NSNumber
                   adviced to use `ns-number-value' to get NSNumber
                   value back as lisp type
  + :ns-array      convert NSArray into a list of foreign-pointer
                   to NSObject
  + :ns-dictionary input syntax

                       ((KEY VAL)
                        ;; key with nested NSDictionary
                        (KEY ((KEY VAL)
                              (KEY VAL))))

                   creates NSMutableDictionary behind the scene

  Built-in ObjC typing in `coca/objc/block' subsystem:
  + :block    expecting (BLOCK-TYPE (lambda-list ...) ,@body)
    see `define-objc-block' for details

  Built-in ObjC typing in `coca/appkit' subsystem:
  + :ns-rect  expecting (X Y W H) inline value passing
  + :ns-point expecting (X Y) inline value passing
  + :ns-size  expecting (W H) inline value passing

  Use `define-objc-typing' to define new ObjC typing.
+ RESULT:

  Built-in ObjC typing:
  + :ns-array return `list' of `foreign-pointer'

  Use `define-objc-typing' to define new ObjC typing.

  If not provided, by default as `:void'

See also `invoke'. "
  (declare (type list args))
  (let* ((resultp (oddp (length args)))
         (result  (if resultp (car (last args)) :void))
         (args    (if resultp (butlast args) args))
         (self    (gensym "SELF"))
         (super   (gensym "SUPER")))
    (multiple-value-bind (args rewrites)
        (expand-invoke-args args)
      (let ((cffi-call
              `(let ((,self (the foreign-pointer ,object)))
                 (with-foreign-object (,super '(:struct objc-super))
                   (%fill-objc-super ,super ,self)
                   (foreign-funcall
                    "objc_msgSendSuper"
                    :pointer ,super
                    :pointer (sel-ptr ,(if (stringp method)
                                           (coerce-to-selector method)
                                           `(coerce-to-selector ,method)))
                    ,@args
                    ,(invoke-result-cffi-type result))))))
        (rewrite-invoke rewrites (wrap-invoke-result result cffi-call))))))


;;; Utils

;; copied from commonqt
;; https://github.com/commonqt/commonqt/blob/dffff3ee3dbd0686c85c323f579b8bbf4881e60e/ffi.lisp#L282
(defmacro with-fp-traps-masked (&body body)
  "Only for SBCL: ignore float traps (often trapped when doing CFFI on macOS).
For other Lisp implementations, this will take no effects. "
  `(#+sbcl sb-int:with-float-traps-masked
    #+sbcl (:invalid :divide-by-zero :underflow :overflow :inexact)
    #-sbcl progn
    ,@body))

(defcstruct (%ns-operating-system-version
             :class ns-operating-system-version)
  (major :long)
  (minor :long)
  (patch :long))

(defmethod translate-from-foreign (ptr (type ns-operating-system-version))
  (with-foreign-slots ((major minor patch)
                       ptr
                       (:struct %ns-operating-system-version))
    (list major minor patch)))

(defun ns-string-to-string (ns-string)
  "Convert NSString object NS-STRING to lisp string.
Return string of NS-STRING. "
  (declare (type foreign-pointer ns-string))
  (let ((cstring (invoke ns-string "UTF8String" :pointer)))
    (the string
      ;; the null pointer CSTRING should be
      ;; correctly mapped into "" (empty string)
      (if (null-pointer-p cstring)
          ""
          (foreign-string-to-lisp cstring)))))

(defun string-to-ns-string (string)
  "Convert lisp STRING into NSString object.
Return foreign-pointer of NSString for STRING. "
  (declare (type string string))
  (the foreign-pointer
    (invoke "NSString" "stringWithUTF8String:"
            :string string
            :object)))

(defun pathname-to-ns-url (pathname)
  "Convert PATHNAME into NSURL.
Return foreign-pointer of NSPath for PATHNAME. "
  (declare (type (or string pathname) pathname))
  (let ((url (string-to-ns-string (uiop:native-namestring pathname))))
    (the foreign-pointer
      (invoke "NSURL" "fileURLWithPath:" :object url :object))))

(defun ns-url-to-pathname (ns-url)
  "Convert NS-URL to pathname.
Return a `pathname' instance. "
  (declare (type foreign-pointer ns-url))
  (if (invoke ns-url "isFileURL" :bool)
      (the pathname
        (pathname
         (invoke ns-url "fileSystemRepresentation" :string)))
      (error "NSURL ~A is not a file URL.
Load `coca/objc/url' and use `coca.objc::ns-url-to-pathname-or-url'
to get NSURL support with `quri'. "
             (description ns-url))))

(defun ns-array-to-list (ns-array)
  "Convert NS-ARRAY into list.
Return a list of foreign-pointer to NSObject. "
  (declare (type foreign-pointer ns-array))
  (loop :for i :below (invoke ns-array "count" :unsigned-long)
        :collect (invoke ns-array "objectAtIndex:" :unsigned-long i :object)))

(defun ns-number (val)
  "Convert VAL into NSNumber.
Return foreign-pointer to NSNumber.

Parameter:
+ VAL
  might be:
  + boolean
  + character
  + integer
  + float
"
  (the foreign-pointer
    (etypecase val
      (boolean
       (invoke "NSNumber" "numberWithBool:" :bool val :object))
      (character
       (invoke "NSNumber" "numberWithChar:" :char (char-code val) :object))
      (integer
       (invoke "NSNumber" "numberWithInt:" :int val :object))
      (single-float
       (invoke "NSNumber" "numberWithFloat:" :float val :object))
      (double-float
       (invoke "NSNumber" "numberWithDouble:" :double val :object)))))

(defun ns-number-value (ns-number type)
  "Convert pointer to NS-NUMBER into TYPE lisp value.
Return value of TYPE.

Parameters:
+ NS-NUMBER:
  foreign-pointer to NSNumber instance
+ TYPE:
  + `:bool'
  + `:char'
  + `:double'
  + `:float'
  + `:int'
  + `:string'
"
  (declare (type foreign-pointer ns-number)
           (type keyword type))
  (ecase type
    (:bool      (invoke ns-number "boolValue" :bool))
    (:char      (code-char (invoke ns-number "charValue" :char)))
    (:float     (invoke ns-number "floatValue"  :float))
    (:double    (invoke ns-number "doubleValue" :double))
    (:int       (invoke ns-number "intValue" :int))
    (:string    (invoke ns-number "stringValue" :ns-string))))

(defun make-ns-mutable-dictionary ()
  "Return a foreign-pointer to NSMutableDictionary. "
  (invoke "NSMutableDictionary" "dictionary" :object))

(defun as-ns-dictionary-key (key)
  (declare (type (or foreign-pointer string list) key))
  (etypecase key
    (foreign-pointer key)
    (string (objc-symbol-value key :object))
    (list   (destructuring-bind (type value) key
              (ecase type
                (:ns-string (string-to-ns-string value)))))))

(defun as-ns-dictionary-val (val)
  (declare (type (or foreign-pointer null string number) val))
  (etypecase val
    (foreign-pointer val)
    (null   (invoke "NSNull" "null" :object))
    (string (string-to-ns-string val))
    (number (ns-number val))))

(defun get-ns-dictionary (dictionary key &optional (result :object))
  "Get/Set object in DICTIONARY of KEY.
Return result specificed by RESULT.

Parameters:
+ DICTIONARY: foreign-pointer to NSDictionary
+ KEY:
  + foreign-pointer: foreign-pointer to NSObject
  + string: use (objc-symbol-value KEY :pointer) as key
  + list of (TYPE LISP-VALUE):
    + (:ns-string STRING)
      use NSString as key
+ RESULT:
  + `:object', `:pointer': return the foreign-pointer
  + `:ns-string': convert NSString as string
  + `:bool', `:char', `:float', `:double', `:int', `:string'
    treat value as NSNumber and decoded with `ns-number-value'

Dev Note:
+ when (setf (get-ns-dictionary dictionary key) value)
  the DICTIONARY should be foreign-pointer to NSMutableDictionary,
  sadly, this won't be checked when setting the value
+ when setf, the VALUE might be:
  + foreign-pointer
  + nil: treat as [NSNull null]
  + string: treat as NSString
  + number: converted using `ns-number'
"
  (declare (type foreign-pointer dictionary)
           (type (or foreign-pointer string list) key)
           (type keyword result))
  (let ((val (invoke dictionary "objectForKey:"
                     :pointer (as-ns-dictionary-key key)
                     :object)))
    (ecase result
      ((:object :pointer)
       val)
      ((:ns-string)
       (ns-string-to-string val))
      ((:bool :char :float :double :int :string)
       (ns-number-value val result)))))

(defun (setf get-ns-dictionary) (value dictionary key &optional result)
  (declare (type foreign-pointer dictionary)
           (ignore result))
  (invoke dictionary
          "setObject:forKey:"
          :object (as-ns-dictionary-val value)
          :object (as-ns-dictionary-key key)))

(defmacro ns-mutable-dictionary (&rest key-val-pairs)
  "Make NSMutableDictionary from KEY-VALS.
Return foreign-pointer of NSMutableDictionary.

Syntax:

    (ns-mutable-dictionary
      {KEY VAL}*
      ...)

Parameters:
KEY-VALS should be like {KEY VAL}... and would be evaluated
+ KEY
  + foreign-pointer to NSObject
  + `nil':  converted into foreign-pointer to NSNull
  + number: converted into foreign-pointer to NSNumber
+ VAL
  + foreign-pointer to NSObject
  + string: (objc-symbol-value VAL :object)
  + list: (TYPE VALUE)

    TYPE:
    + :ns-string => (string-to-ns-string VALUE)
"
  (alx:with-gensyms (dictionary)
    `(let ((,dictionary (make-ns-mutable-dictionary)))
       ,@(loop :for (key val) :in key-val-pairs
               :collect `(setf (get-ns-dictionary ,dictionary ,key) ,val))
       ,dictionary)))

(defun alloc (class)
  "Allocate instance of CLASS.
Return foreign-pointer to instance of CLASS (not initialized). "
  (the foreign-pointer
    (invoke (coerce-to-objc-class class) "alloc" :object)))

(defun init (ns-object)
  "Init NS-OBJECT.
Return foreign-pointer (same to) NS-OBJECT. "
  (declare (type foreign-pointer ns-object))
  (the foreign-pointer (invoke ns-object "init" :object)))

(defun description (ns-object)
  "Return description string of NS-OBJECT. "
  (declare (type foreign-pointer ns-object))
  (with-autorelease-pool
    (invoke ns-object "description" :ns-string)))

(defun release (ns-object)
  "Release NS-OBJECT. "
  (declare (type foreign-pointer ns-object))
  (invoke ns-object "release"))

(defun retain (ns-object)
  "Retain NS-OBJECT count.
Return foreign-pointer (same to) NS-OBJECT. "
  (declare (type foreign-pointer ns-object))
  (invoke ns-object "retain" :object))

(defun autorelease (ns-object)
  "Autorelease NS-OBJECT.
Return foreign-pointer (same to) NS-OBJECT. "
  (declare (type foreign-pointer ns-object))
  (invoke ns-object "autorelease" :object))

;;;; invoke.lisp ends here
