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
        (setf (objc-property-name   eslotd) (objc-property-name   slotd)
              (objc-property-reader eslotd) (objc-property-reader slotd)
              (objc-property-writer eslotd) (objc-property-writer slotd))
        (return)))
    eslotd))

;; For `objc-property-slot':
;; reading slot is invoke on `objc-property-reader'
(defmethod c2mop:slot-value-using-class ((class objc-class) object (slot objc-property-slot))
  (invoke object (objc-property-reader slot)))

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

(defun objc-property-slot-initargs (def)
  "Convert DEF as ObjC property initialize args. "
  (etypecase def
    (list
     (destructuring-bind (name &key
                                 objc-property-type
                                 objc-property
                                 objc-property-reader
                                 objc-property-writer
                                 objc-property-read-only
                                 documentation
                          &allow-other-keys)
         def
       (let* ((pname  (the string
                        (or objc-property
                            (error "Missing ~S for ObjC property. "
                                   :objc-property))))
              (reader (coerce-to-selector
                       (or objc-property-reader
                           pname)))
              (writer (if objc-property-read-only
                          nil
                          (coerce-to-selector
                           (or objc-property-writer
                               (str:concat "set" (str:pascal-case pname) ":"))))))
         `(:name                 ,name
           :objc-property        ,pname
           :objc-property-reader ,reader
           :objc-property-writer ,writer
           :type                 ,(if objc-property-type
                                      (objc-encoding-lisp-type objc-property-type)
                                      t)
           :documentation        ,(or documentation
                                      (format nil "ObjC property ~A. " pname))
           :initargs             ()
           :readers             (,name)
           :writers             ,(when writer `((setf ,name)))
           :allocation           :objc))))
    (objc-property-slot
     `(:name                 ,(c2mop:slot-definition-name         def)
       :objc-property        ,(objc-property-name                 def)
       :objc-property-reader ,(objc-property-reader               def)
       :objc-property-writer ,(objc-property-writer               def)
       :initargs             ,(c2mop:slot-definition-initargs     def)
       :initform             ,(c2mop:slot-definition-initform     def)
       :initfunction         ,(c2mop:slot-definition-initfunction def)
       :readers              ,(c2mop:slot-definition-readers      def)
       :writers              ,(c2mop:slot-definition-writers      def)
       :type                 ,(c2mop:slot-definition-type         def)
       :allocation           :objc
       :documentation        ,(documentation def t)))
    (c2mop:direct-slot-definition
     `(:name                 ,(c2mop:slot-definition-name         def)
       :initargs             ,(c2mop:slot-definition-initargs     def)
       :allocation           ,(c2mop:slot-definition-allocation   def)
       :initform             ,(c2mop:slot-definition-initform     def)
       :initfunction         ,(c2mop:slot-definition-initfunction def)
       :readers              ,(c2mop:slot-definition-readers      def)
       :writers              ,(c2mop:slot-definition-writers      def)
       :type                 ,(c2mop:slot-definition-type         def)
       :documentation        ,(documentation def t)))))

(defun objc-class-add-objc-properties (objc-class objc-properties)
  "Add ObjC OBJC-PROPERTIES to OBJC-CLASS.

Parameters:
+ OBJC-CLASS: should be coerce to objc class
+ OBJC-PROPERTIES: a list like:

     (NAME . OBJC-PROPERTY-SLOT-OPTIONS)

  OBJC-PROPERTY-SLOT-OPTIONS:

     objc-property-type
     objc-property
     objc-property-reader
     objc-property-writer
     objc-property-read-only
     documentation
"
  (let* ((class  (coerce-to-objc-class objc-class))
         (name   (class-name class))
         (super  (c2mop:class-direct-superclasses class))
         (direct (union (mapcar #'objc-property-slot-initargs (c2mop:class-direct-slots class))
                        (mapcar #'objc-property-slot-initargs objc-properties)
                        :key (lambda (plist) (getf plist :name)))))
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
      [direct-objc-property-slots]
      documentations...)

Parameters:
+ NAME: string of ObjC class name
+ DIRECT-OBJC-PROPERTY-SLOTS:

      (
         NAME
       { OBJC-PROPERTY-READER | (OBJC-PROPERTY-READER OBJC-PROPERTY-WRITER) }
        [:read-only]
        [documentation]
      )

  Dev Note:
  see `coca.objc::objc-class-add-objc-properties'.
+ DOCUMENTATIONS: documentation strings (joined by new line)
"
  (let ((class (gensym "OBJC-CLASS"))
        docstring
        slots)
    (cond ((endp documentations)
           (setf docstring (format nil "ObjC Class ~S. " name)))
          (t
           (setf slots (pop documentations))
           (unless (listp slots)
             (push slots documentations)
             (setf slots nil))
           (setf docstring (format nil "~{~A~^~%~%~}" documentations))))
    (when slots
      (loop :for (name objc-property . optional) :in slots
            :for objc-reader := (atomize objc-property #'first)
            :for objc-writer := (atomize objc-property #'second)
            :for read-only   := (etypecase (first optional)
                                  ((eql :read-only) t)
                                  (string           nil))
            :for document    := (cond ((stringp (first  optional)) (first  optional))
                                      ((stringp (second optional)) (second optional))
                                      (t nil))
            :collect `(:name                    ,name
                       :objc-property           ,objc-reader
                       :objc-property-reader    ,objc-reader
                       :objc-property-writer    ,objc-writer
                       :objc-property-read-only ,read-only
                       :documentation           ,document)
              :into slots*
            :finally (setf slots slots*)))
    `(let ((,class (coerce-to-objc-class ,name)))
       (setf (documentation ,class t) ,docstring)
       ,@(when slots `((objc-class-add-objc-properties ,class ',slots)))
       (class-name ,class))))

;;;; objc-property.lisp ends here
