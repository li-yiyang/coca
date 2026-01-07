
;;;; objc-class.lisp --- ObjC Class wrapper in Lisp

(in-package :coca.objc)

(defparameter *classes* (make-hash-table :test 'equal)
  "Cache of ObjC classes.

KEY: name of ObjC class
VAL: lisp class as ObjC class wrapper")

(defclass objc-class (standard-class objc-pointer)
  ((objc-class-name
    :type    string
    :initarg :objc-class-name
    :reader  objc-class-name)
   (objc-instance-methods
    :initform (make-hash-table :test 'equal)
    :documentation "Methods cache of instance methods.
KEY: `sel' of methods
VAL: a function for FFI calling, see `*methods*'.

See `coca.objc::objc-class-instance-method'. ")
   (objc-class-methods
    :initform (make-hash-table :test 'equal)
    :documentation "Methods cache of class methods.
Key: `sel' of methods
VAL: a function for FFI calling, see `*methods*'.

See `coca.objc::objc-class-class-method'. "))
  (:documentation
   "Metaclass of ObjC object.

Use `coerce-to-objc-class' to make an ObjC Class as lisp class.
All the ObjC class should be cached within `coca.objc::*classes*'.
"))

(defun objc-class-p (class)
  "Test if CLASS is `objc-class'. "
  (let ((class (if (symbolp class) (find-class class) class)))
    (typep class 'objc-class)))

(defmethod c2mop:validate-superclass ((class objc-class) (super c2mop:standard-class))
  t)

(defmethod print-object ((class objc-class) stream)
  (print-unreadable-object (class stream)
    (format stream
            "~S ~A ~X"
            (class-name class)
            (slot-value class 'objc-class-name)
            (pointer-address (objc-object-pointer class)))))



;; Implementation ObjC slots as lisp slots
;;
;; Reference:
;; 18.4 Implementation of virtual slots
;; https://www.lispworks.com/documentation/lw81/lw/lw-mop-ug-4.htm

(defclass objc-property-slot (c2mop:standard-slot-definition)
  ((objc-property
    :type     string
    :initarg  :objc-property
    :accessor objc-property-name)
   (objc-encoding
    :type     objc-basic-encoding
    :initarg  :objc-property-encoding
    :accessor objc-property-encoding)
   (objc-property-reader
    :type     (or null sel)
    :initarg  :objc-property-reader
    :initform nil
    :accessor objc-property-reader)
   (objc-property-after-read
    :type      (or symbol function)
    :initarg   :objc-property-after-read
    :initform  'identity
    :accessor  objc-property-after-read)
   (objc-property-before-write
    :type      (or symbol function)
    :initarg   :objc-property-before-write
    :initform  'identity
    :accessor  objc-property-before-write)
   (objc-property-writer
    :type   (or null sel)
    :initarg  :objc-property-writer
    :initform nil
    :accessor objc-property-writer))
  (:documentation
   "Mixin for ObjC property slots and methods.

Dev Note:
In CLOS MOP, there's two slot type:
+ direct slot: specified at class definition
+ effective slot: computed when finalize class, used to get the value

See `coca.objc::objc-property-direct-slot' and `coca.objc::objc-property-effective-slot''"))

(defclass objc-property-direct-slot (c2mop:standard-direct-slot-definition objc-property-slot)
  ((objc-property :initform (error "Missing ~S. " :objc-property)))
  (:documentation "Class of direct ObjC slots and methods to construct them when appropriate. "))

(defclass objc-property-effective-slot (c2cl:standard-effective-slot-definition objc-property-slot)
  ((objc-property :initform ""))
  (:documentation "Class of effective ObjC slots and methods to construct them when appropriate. "))

(defun objc-property-slot-p (slot-definition)
  (declare (type c2mop:slot-definition slot-definition))
  (typep slot-definition 'objc-property-slot))

(defmethod c2mop:slot-definition-allocation ((slot objc-property-slot))
  "The slot value is allocated in ObjC side and should be accessed only using `invoke'. "
  :objc)

(defmethod (setf c2mop:slot-definition-allocation) (allocation (slot objc-property-slot))
  (unless (eq allocation :objc)
    (error "Cannot change the allocation of ~S. " 'objc-property-slot))
  allocation)

;; This is like:
;;
;; (let* ((normalized-direct-slot (:name ... ))
;;        (class       ...)
;;        (slot-class (apply #'direct-slot-definition-class class normalized-direct-slot)))
;;   (apply #'make-instance slot-class normalized-direct-slot))
;;
;; when creating the direct slot of CLASS
;;
(defmethod c2mop:direct-slot-definition-class ((class objc-class) &rest initargs)
  "Called when the class is being made, to choose the metaclass of a given direct slot.
It should return the class of slot definition required. "
  (if (eq (getf initargs :allocation) :objc)
      (find-class 'objc-property-direct-slot)
      (call-next-method)))

;; Same as `c2mop:direct-slot-definition-class',
;; used when `c2mop:compute-effective-slot-definition'.
(defmethod c2mop:effective-slot-definition-class ((class objc-class) &rest initargs)
  "Called when the CLASS is being finalized,
to choose the metaclass of a given effective slot.
It should return the class of slot definition required. "
  (if (eq (getf initargs :allocation) :objc)
      (find-class 'objc-property-effective-slot)
      (call-next-method)))

;; This finalized the objc-property-slot as effective slot from direct slot
(defmethod c2mop:compute-effective-slot-definition
    ((class objc-class) name direct-slot-definitions)
  "Copy objc-property-direct-slot metadata to objc-property-effective-slot. "
  (let ((eslotd (call-next-method)))
    (dolist (slotd direct-slot-definitions)
      (when (objc-property-slot-p slotd)
        (setf (objc-property-name         eslotd) (objc-property-name         slotd)
              (objc-property-encoding     eslotd) (objc-property-encoding     slotd)
              (objc-property-before-write eslotd) (objc-property-before-write slotd)
              (objc-property-after-read   eslotd) (objc-property-after-read   slotd))
        (when (objc-property-reader slotd)
          (setf (objc-property-reader eslotd)
                (coerce-to-selector (objc-property-reader slotd))))
        (when (objc-property-writer slotd)
          (setf (objc-property-writer eslotd)
                (coerce-to-selector (objc-property-writer slotd))))
        (return)))
    eslotd))

;; For `objc-property-slot':
;; reading slot is invoke on `objc-property-reader'
(defmethod c2mop:slot-value-using-class ((class objc-class) object (slot objc-property-slot))
  (funcall (objc-property-after-read slot) (invoke object (objc-property-reader slot))))

;; For `objc-property-slot':
;; writing slot is invoke on `objc-property-writer'
;; if `objc-property-writer' is `nil', it's a read-only slot
(defmethod (setf c2mop:slot-value-using-class) (value (class objc-class) object (slot objc-property-slot))
  (let ((writer (objc-property-writer slot)))
    (if writer
        (invoke object writer (funcall (objc-property-before-write slot) value))
        (error "ObjC property ~S is read only. " (c2mop:slot-definition-name slot)))))

(defmethod c2mop:slot-boundp-using-class ((class objc-class) object (slot objc-property-slot))
  (declare (ignore class))
  (can-invoke-p object (objc-property-reader slot)))

(defmethod c2mop:slot-makunbound-using-class ((class objc-class) object (slot objc-property-slot))
  (declare (ignore class object slot))
  (error "Cannot unbound ObjC property ~S. " (c2mop:slot-definition-name slot)))


;;;; Automatically Generate ObjC Property

;; Use `class_copyPropertyList' would copy the property list of `objc-class'.
;; Use `property_getName' would get the name of property -> `objc-intern'
;; Use `property_getAttributes' would get the attribute string -> `decode-objc-property-attributes'

(defun gen-objc-property-set (name)
  "Given NAME like isRunning and return setIsRunning:. "
  (declare (type string name))
  (concatenate 'string "set" (the string (str:pascal-case name)) ":"))

(defparameter *objc-property-names* (make-hash-table :test 'equal)
  "Cache of ObjC property names in lisp.

KEY: ObjC property name string
VAL: symbol of ObjC property in lisp (as slot name)")

(defparameter *objc-property-blacklist*
  (let* ((slots    '(
                     "debugDescription"
                     "className"        ; not needed
                     "superclass"
                     ))
         (blacklist (make-hash-table :test 'equal
                                     :size (length slots))))
    (dolist (slot slots blacklist)
      (setf (gethash (the string slot) blacklist) t)))
  "Names of ObjC properties that should not be interned.

KEY: ObjC property name string
VAL: boolean

see `objc-class-additional-direct-slots'. ")

(defmacro ignore-objc-properties (&rest properties)
  "Ignore ObjC PROPERTIES. "
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf ,@(loop :for prop :in properties
                   :collect `(gethash ,prop *objc-property-blacklist*)
                   :collect t))))

(defun decode-objc-property-attributes (name attributes)
  "Decode the ObjC property attribute type string.
Return values are LISP-NAME, OBJC-NAME, OBJC-ENCODING, READER, WRITER.

+ attributes are joined with ,
+ declared property type encodings:

  R           :read-only
  C           :copy
  &           :retain
  N           :nonatomic
  G<NAME>     :objc-property-reader
  S<NAME>     :objc-property-writer
  D           :dynamic
  W           :weak
  P           :eligible
  T<ENCODING> :type (ignored)
  t<ENCODING> :type (ignored)

see https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ObjCRuntimeGuide/Articles/ocrtPropertyIntrospection.html#//apple_ref/doc/uid/TP40008048-CH101"
  (loop :with read-only := nil
        :with reader    := name
        :with writer    := (gen-objc-property-set name)
        :with encoding  := :unknown
        :for attribute :in (str:split-omit-nulls "," attributes)
        :do (case (aref attribute 0)
              (#\R (setf writer nil))
              (#\G (setf reader (subseq attribute 1)))
              (#\S (setf writer (subseq attribute 1)))
              (#\T (multiple-value-bind (ignore enc)
                       (decode-objc-type-encoding
                        (str:replace-all "@\".*\"" "@" (subseq attribute 1) :regex t))
                     (declare (ignore ignore))
                     (setf encoding enc))))
        :finally (return (values (with-cached (name *objc-property-names*)
                                   (objc-intern name))
                                 name
                                 encoding
                                 reader
                                 writer))))

(defun objc-class-property-slots (objc-class)
  "Slot defintions of OBJC-CLASS.
Return a list of `objc-clas-property'. "
  (when (typep objc-class 'objc-class)
    (remove-if-not #'objc-property-slot-p (c2mop:class-slots objc-class))))

(defun objc-class-additional-direct-slots (pointer super-slots)
  "Return a list of decoded property infomation for OBJC-CLASS.

The additional property should not exists in SUPER-SLOTS.
Return form should be able to use as :direct-slots. "
  (declare (type foreign-pointer pointer))
  (let ((super-slots (loop :with hash := (make-hash-table :test 'equal)
                           :for slotd :in super-slots
                           :do (setf (gethash (objc-property-name slotd) hash) t)
                           :finally (return hash))))
    (with-foreign-pointer (out-count (foreign-type-size :unsigned-int))
      (let ((properties (class_copyPropertyList pointer out-count)))
        (unwind-protect
             (loop
               :with def := ()
               :for i :below (mem-ref out-count :unsigned-int)
               :for property   := (mem-aref properties :pointer i)
               :for name       := (property_getName       property)
               :for attributes := (property_getAttributes property)
               :if (and
                    (not (gethash name super-slots))
                    (not (gethash name *objc-property-blacklist*))
                    ;; if failed to parse encoding, ignore them... 
                    (ignore-errors
                     (multiple-value-bind
                           (lisp-name objc-name encoding reader writer)
                         (decode-objc-property-attributes name attributes)
                       (setf def
                             `(:name                   ,lisp-name
                               :objc-property          ,objc-name
                               :objc-property-encoding ,encoding
                               :objc-property-reader   ,(coerce-to-selector reader)
                               :objc-property-writer   ,(and writer (coerce-to-selector writer))
                               :allocation             :objc)))))
                 :collect def)
          (foreign-free properties))))))

(defun coerce-to-objc-class (class)
  "Coerces its argument to an Objective-C class pointer.
Return the lisp class of CLASS.

Parameters:
+ CLASS:
  + `objc-class':
  + symbol: find class and assert it's `objc-class'
  + string: of ObjC class
    this would make an `objc-class' instance class with name CLASS-NAME,
    by default the CLASS-NAME is parsed by `str:param-case' from CLASS,
    or you could set it via optional parameter CLASS-NAME

    this would using `coca.objc::objc_getClass' to get class in the ObjC
    runtime, if not found, raise error.
  + foreign-pointer of ObjC class
    the foreign-pointer would be checked via `coca.objc::object_isClass'
    to test if it's a valid foreign-pointer to ObjC Class

    the name of class would be fetched by `coca.objc::class_getName',
    the CLASS-NAME would be used to set the lisp class name if needed
    (see above as string CLASS input)
"
  (declare (type (or string symbol objc-class foreign-pointer) class))
  (flet ((wrap-ptr-as-objc-class (ptr name class-name)
           (declare (type foreign-pointer ptr)
                    (type string          name)
                    (type symbol          class-name))
           (let* ((super (class_getSuperClass ptr))
                  (super (if (null-pointer-p super)
                             (find-class 'standard-objc-object)
                             (coerce-to-objc-class super)))
                  (slots (objc-class-additional-direct-slots
                          ptr (objc-class-property-slots super))))
             (c2mop:ensure-finalized
              (c2mop:ensure-class
               class-name
               :name                class-name
               :metaclass           (find-class 'objc-class)
               :objc-class-name     name
               :objc-object-pointer ptr
               :direct-superclasses (list super)
               :direct-slots        slots)))))
    (the objc-class
      (etypecase class
        (objc-class class)
        (symbol
         ;; For symbol (already interned ObjC class):
         ;; find the `objc-class' of the name of symbol
         (the objc-class (find-class class)))
        (string
         ;; For ObjC class name
         (with-cached (class *classes*)
           (let ((ptr (objc_getClass class)))
             (when (null-pointer-p ptr)
               (error "Unknown ObjC class `~A' within current ObjC Runtime. " class))
             (wrap-ptr-as-objc-class ptr class (objc-intern class)))))
        (foreign-pointer
         (when (null-pointer-p class)
           (error "NULL pointer is not a pointer of ObjC class"))
         (unless (object_isClass class)
           (error "~S is not a pointer of ObjC class. " class))
         (let ((name (class_getName class)))
           (with-cached (name *classes*)
             (let ((class-name (objc-intern name)))
               (wrap-ptr-as-objc-class class name class-name)))))))))

(defun pointer-to-objc-class-nullable (pointer)
  "Convert POINTER to ObjC class.
Return `nil' if POINTER is NULL, otherwise `objc-class'.

Parameter:
+ POINTER: foreign pointer to ObjC class.
  can be NULL pointer.
"
  (declare (type foreign-pointer pointer))
  (if (null-pointer-p pointer)
      nil
      (coerce-to-objc-class pointer)))

(defun objc-class-modify-objc-properties (class objc-properties)
  "Modify ObjC OBJC-PROPERTIES of OBJC-CLASS.

Parameters:
+ OBJC-CLASS: should be coerce to objc class
+ OBJC-PROPERTIES: a list like:

     (NAME &KEY BEFORE AFTER READER ACCESSOR DOCUMENTAION)

"
  (declare (type objc-class class))
  (let* ((name   (class-name class))
         (super  (c2mop:class-direct-superclasses class))
         (direct (loop
                   :for slotd :in (c2mop:class-direct-slots class)
                   :for objc-p := (eq (c2mop:slot-definition-allocation slotd) :objc)
                   :for update := (and objc-p
                                       (assoc (objc-property-name slotd)
                                              objc-properties
                                              :test #'string=))
                   :if update
                     :collect
                     (destructuring-bind (name &key
                                                 before after accessor
                                                 (reader accessor)
                                                 (writer accessor)
                                                 (encoding (objc-property-encoding slotd))
                                                 (getter   (objc-property-reader   slotd))
                                                 (setter   (objc-property-writer   slotd))
                                                 documentation
                                          &allow-other-keys)
                         update
                       (declare (ignore name))
                       `(:name                       ,(c2mop:slot-definition-name slotd)
                         :objc-property              ,(objc-property-name   slotd)
                         :objc-property-encoding     ,encoding
                         :objc-property-reader       ,(coerce-to-selector getter)
                         :objc-property-writer       ,(and setter (coerce-to-selector setter))
                         :objc-property-before-write ,(or before #'identity)
                         :objc-property-after-read   ,(or after  #'identity)
                         :initargs                   ()
                         :readers                    ,(when reader `(,reader))
                         :writers                    ,(when writer `((setf ,writer)))
                         :allocation                 :objc
                         :documentation              ,documentation))
                   :else
                     :if objc-p
                       :collect
                   `(:name                   ,(c2mop:slot-definition-name    slotd)
                     :objc-property          ,(objc-property-name            slotd)
                     :objc-property-encoding ,(objc-property-encoding        slotd)
                     :objc-property-reader   ,(objc-property-reader          slotd)
                     :objc-property-writer   ,(objc-property-writer          slotd)
                     :initargs               ()
                     :readers                ,(c2mop:slot-definition-readers slotd)
                     :writers                ,(c2mop:slot-definition-writers slotd)
                     :allocation             :objc)
                   :else
                     :collect
                   `(:name       ,(c2mop:slot-definition-name       slotd)
                     :initargs   ,(c2mop:slot-definition-initargs   slotd)
                     :readers    ,(c2mop:slot-definition-readers    slotd)
                     :writers    ,(c2mop:slot-definition-writers    slotd)
                     :allocation ,(c2mop:slot-definition-allocation slotd)))))
    (c2mop:ensure-finalized
     (c2mop:ensure-class name
                         :metaclass           (find-class 'objc-class)
                         :name                name
                         :direct-superclasses super
                         :direct-slots        direct))))

(defmacro doc-objc-class (name &body documentations)
  "Set documentation for ObjC class of NAME with DOCUMENTATIONS.

Syntax:

    (doc-objc-class NAME
      [(OBJC-PROPERTY &key before after reader accessor documentation)]
      documentations...)

Parameters:
+ NAME: string of ObjC class name
+ DOCUMENTATIONS: documentation strings (joined by new line)
"
  (let ((class      (gensym "OBJC-CLASS"))
        (properties (let ((p (pop documentations)))
                      (unless (listp p)
                        (push p documentations)
                        (setf p nil))
                      p))
        (docstring  (format nil "~{~A~^~%~%~}" documentations)))
    `(let ((,class (coerce-to-objc-class ,name)))
       (setf (documentation ,class t) ,docstring)
       ,@(when properties `((objc-class-modify-objc-properties ,class ',properties)))
       (class-name ,class))))


;;;; Define new ObjC class

(defmethod initialize-instance :after ((class objc-class) &key)
  "If `objc-object-pointer' of CLASS is not bound, regist it. "
  (unless (objc-object-pointer class)
    (let* ((objc-name  (objc-class-name class))
           ;; select only ObjC property slot
           (dslots     (mapcar #'objc-property-slot-p
                               (c2mop:class-direct-superclasses class)))
           (objc-super (remove-if-not #'objc-class-p
                                      (c2mop:class-direct-superclasses class))))
      (when (< 1 (length objc-super))
        (error "ObjC class ~S can only be one subclass of ~{~A~^, ~}"
               objc-name objc-super))
      (let ((pointer (objc_allocateClassPair
                      (objc-object-pointer (car objc-super))
                      objc-name
                      0)))
        (dolist (slot dslots)
          (let* ((encoding (objc-property-encoding slot))
                 (size     (foreign-type-size (objc-encoding-cffi-type encoding))))
            (class_addIvar pointer
                           (objc-property-name slot)
                           (* 8 size)
                           size
                           (encode-objc-type-encoding (list encoding)))))
        (objc_registerClassPair pointer)
        (setf (slot-value class 'objc-object-pointer) pointer)))))

;;;; objc-class.lisp ends here
