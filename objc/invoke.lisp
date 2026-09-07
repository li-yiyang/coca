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
  + :object   expecting to be `objc-object'
  + :class    expecting to be `objc-class'
  + :sel      expecting to be `sel'
  + :ns-uint  alias as :unsigned-long
  + :ns-int   alias as :long
  + :cg-float alias as :double

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

"
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
  + :object   expecting to be `objc-object'
  + :class    expecting to be `objc-class'
  + :sel      expecting to be `sel'
  + :ns-uint  alias as :unsigned-long
  + :ns-int   alias as :long
  + :cg-float alias as :double

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
"
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
  (the pathname
    (pathname
     (ns-string-to-string
      (invoke ns-url "absoluteString" :object)))))

(defun ns-array-to-list (ns-array)
  "Convert NS-ARRAY into list.
Return a list of foreign-pointer to NSObject. "
  (declare (type foreign-pointer ns-array))
  (loop :for i :below (invoke ns-array "count" :unsigned-long)
        :collect (invoke ns-array "objectAtIndex:" :unsigned-long i :object)))

(defun %ns-mutable-dictionary (&rest key-vals)
  (loop :with dict := (invoke "NSMutableDictionary" "dictionary" :object)
        :for (key val) :on key-vals :by #'cddr
        :for val* := (etypecase val
                       (foreign-pointer val)
                       (null   (invoke "NSNull" "null" :object))
                       (string (string-to-ns-string val))
                       (number (ns-number val)))
        :for key* := (etypecase key
                       (foreign-pointer key)
                       (string
                        (objc-symbol-value key :object))
                       (list
                        (destructuring-bind (type value) key
                          (ecase type
                            (:ns-string (string-to-ns-string value))))))
        :do (invoke dict "setObject:forKey:" :object val* :object key*)
        :finally (return dict)))

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
  `(%ns-mutable-dictionary ,@(loop :for (key val) :in key-val-pairs
                                   :collect key :collect val)))

(defun ns-number (val)
  "Convert VAL into NSNumber.
Return foreign-pointer to NSNumber. "
  (declare (type real val))
  (the foreign-pointer
    (etypecase val
      (integer
       (invoke "NSNumber" "numberWithInt:" :int val :object))
      (double-float
       (invoke "NSNumber" "numberWithDouble:" :double val :object)))))

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
