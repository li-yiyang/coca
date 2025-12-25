;;;; objc-property.lisp --- Implement ObjC property as lisp slot

(in-package :coca.objc)

;;; Reference:
;;; 18.4 Implementation of virtual slots
;;; https://www.lispworks.com/documentation/lw81/lw/lw-mop-ug-4.htm

(defclass objc-property-slot (c2mop:standard-slot-definition)
  ((objc-property
    :type     string
    :initarg  :objc-property
    :accessor objc-property-name)
   (objc-property-reader
    :type     (or null sel)
    :initarg  :objc-property-reader
    :initform nil
    :accessor objc-property-reader)
   (objc-property-after-read
    :type      function
    :initarg   :objec-property-after-read
    :initform  #'identity
    :accessor  objc-property-after-read)
   (objc-property-befor-write
    :type      function
    :initarg   :objec-property-before-write
    :initform  #'identity
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
(defmethod c2mop:compute-effective-slot-definition ((class objc-class) name direct-slot-definitions)
  "Copy objc-property-direct-slot metadata to objc-property-effective-slot. "
  (let ((eslotd (call-next-method)))
    (dolist (slotd direct-slot-definitions)
      (when (objc-property-slot-p slotd)
        (setf (objc-property-name       eslotd) (objc-property-name   slotd)
              (objc-property-reader     eslotd) (objc-property-reader slotd)
              (objc-property-writer     eslotd) (objc-property-writer slotd)
              (objc-property-after-read eslotd) (objc-property-after-read slotd))
        (return)))
    eslotd))

;; For `objc-property-slot':
;; reading slot is invoke on `objc-property-reader'
(defmethod c2mop:slot-value-using-class ((class objc-class) object (slot objc-property-slot))
  (funcall (objc-property-wrapper slot) (invoke object (objc-property-reader slot))))

;; For `objc-property-slot':
;; writing slot is invoke on `objc-property-writer'
;; if `objc-property-writer' is `nil', it's a read-only slot
(defmethod (setf c2mop:slot-value-using-class) (value (class objc-class) object (slot objc-property-slot))
  (let ((writer (objc-property-reader slot)))
    (if writer
        (invoke object writer value)
        (error "ObjC property ~S is read only. " (c2mop:slot-definition-name slot)))))

(defmethod c2mop:slot-boundp-using-class ((class objc-class) object (slot objc-property-slot))
  (declare (ignore class object slot))
  t)

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
  (let* ((slots    '("debugDescription"))
         (blacklist (make-hash-table :test 'equal
                                     :size (length slots))))
    (dolist (slot slots blacklist)
      (setf (gethash (the string slot) blacklist) t)))
  "Names of ObjC properties that should not be interned.

KEY: ObjC property name string
VAL: boolean

see `objc-class-additional-direct-slots'. ")

(defun decode-objc-property-attributes (name attributes)
  "Decode the ObjC property attribute type string.
Return values are LISP-NAME, OBJC-NAME, READER, WRITER.

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
        :for attribute :in (str:split-omit-nulls "," attributes)
        :do (case (aref attribute 0)
              (#\R (setf writer nil))
              (#\G (setf reader (subseq attribute 1)))
              (#\S (setf writer (subseq attribute 1))))
        :finally (return (values (with-cached (name *objc-property-names*)
                                   (objc-intern name))
                                 name
                                 reader
                                 writer))))

(defun objc-class-property-slots (objc-class)
  "Slot defintions of OBJC-CLASS.
Return a list of `objc-clas-property'. "
  (when (typep objc-class 'objc-class)
    (loop :for slotd :in (c2mop:class-slots objc-class)
          :if (typep slotd 'objc-property-slot)
            :collect slotd)))

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
             (loop :for i :below (mem-ref out-count :unsigned-int)
                   :for property   := (mem-aref properties :pointer i)
                   :for name       := (property_getName       property)
                   :for attributes := (property_getAttributes property)
                   :if (and (not (gethash name super-slots))
                            (not (gethash name *objc-property-blacklist*)))
                     :collect (multiple-value-bind (name objc-property objc-reader objc-writer)
                                  (decode-objc-property-attributes name attributes)
                                `(:name                 ,name
                                  :objc-property        ,objc-property
                                  :objc-property-reader ,(coerce-to-selector objc-reader)
                                  :objc-property-writer ,(and objc-writer
                                                              (coerce-to-selector objc-writer))
                                  :allocation           :objc)))
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

;; (defun objc-property-slot-initargs (def)
;;   "Convert DEF as ObjC property initialize args. "
;;   (etypecase def
;;     (list
;;      (destructuring-bind (name &key
;;                                  objc-property-type
;;                                  objc-property
;;                                  objc-property-reader
;;                                  objc-property-writer
;;                                  objc-property-read-only
;;                                  documentation
;;                           &allow-other-keys)
;;          def
;;        (let* ((pname  (the string
;;                         (or objc-property
;;                             (error "Missing ~S for ObjC property. "
;;                                    :objc-property))))
;;               (reader (coerce-to-selector
;;                        (or objc-property-reader
;;                            pname)))
;;               (writer (if objc-property-read-only
;;                           nil
;;                           (coerce-to-selector
;;                            (or objc-property-writer
;;                                (gen-objc-property-set pname))))))
;;          `(:name                 ,name
;;            :objc-property        ,pname
;;            :objc-property-reader ,reader
;;            :objc-property-writer ,writer
;;            :type                 ,(if objc-property-type
;;                                       (objc-encoding-lisp-type objc-property-type)
;;                                       t)
;;            :documentation        ,(or documentation
;;                                       (format nil "ObjC property ~A. " pname))
;;            :initargs             ()
;;            :readers             (,name)
;;            :writers             ,(when writer `((setf ,name)))
;;            :allocation           :objc))))
;;     (objc-property-slot
;;      `(:name                 ,(c2mop:slot-definition-name         def)
;;        :objc-property        ,(objc-property-name                 def)
;;        :objc-property-reader ,(objc-property-reader               def)
;;        :objc-property-writer ,(objc-property-writer               def)
;;        :initargs             ,(c2mop:slot-definition-initargs     def)
;;        :initform             ,(c2mop:slot-definition-initform     def)
;;        :initfunction         ,(c2mop:slot-definition-initfunction def)
;;        :readers              ,(c2mop:slot-definition-readers      def)
;;        :writers              ,(c2mop:slot-definition-writers      def)
;;        :type                 ,(c2mop:slot-definition-type         def)
;;        :allocation           :objc
;;        :documentation        ,(documentation def t)))
;;     (c2mop:direct-slot-definition
;;      `(:name                 ,(c2mop:slot-definition-name         def)
;;        :initargs             ,(c2mop:slot-definition-initargs     def)
;;        :allocation           ,(c2mop:slot-definition-allocation   def)
;;        :initform             ,(c2mop:slot-definition-initform     def)
;;        :initfunction         ,(c2mop:slot-definition-initfunction def)
;;        :readers              ,(c2mop:slot-definition-readers      def)
;;        :writers              ,(c2mop:slot-definition-writers      def)
;;        :type                 ,(c2mop:slot-definition-type         def)
;;        :documentation        ,(documentation def t)))))

;; (defun objc-class-add-objc-properties (objc-class objc-properties)
;;   "Add ObjC OBJC-PROPERTIES to OBJC-CLASS.

;; Parameters:
;; + OBJC-CLASS: should be coerce to objc class
;; + OBJC-PROPERTIES: a list like:

;;      (NAME . OBJC-PROPERTY-SLOT-OPTIONS)

;;   OBJC-PROPERTY-SLOT-OPTIONS:

;;      objc-property-type
;;      objc-property
;;      objc-property-reader
;;      objc-property-writer
;;      objc-property-read-only
;;      documentation
;; "
;;   (let* ((class  (coerce-to-objc-class objc-class))
;;          (name   (class-name class))
;;          (super  (c2mop:class-direct-superclasses class))
;;          (direct (union (mapcar #'objc-property-slot-initargs (c2mop:class-direct-slots class))
;;                         (mapcar #'objc-property-slot-initargs objc-properties)
;;                         :key (lambda (plist) (getf plist :name)))))
;;     (c2mop:ensure-finalized
;;      (c2mop:ensure-class name
;;                          :metaclass           (find-class 'objc-class)
;;                          :name                name
;;                          :direct-superclasses super
;;                          :direct-slots        direct))))

(defmacro doc-objc-class (name &body documentations)
  "Set documentation for ObjC class of NAME with DOCUMENTATIONS.

Syntax:

    (doc-objc-class NAME
      documentations...)

Parameters:
+ NAME: string of ObjC class name
+ DOCUMENTATIONS: documentation strings (joined by new line)
"
  (let ((class     (gensym "OBJC-CLASS"))
        (docstring (format nil "~{~A~^~%~%~}" documentations)))
    `(let ((,class (coerce-to-objc-class ,name)))
       (setf (documentation ,class t) ,docstring)
       (class-name ,class))))

;;;; objc-property.lisp ends here
