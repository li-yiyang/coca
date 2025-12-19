;;;; struct.lisp --- ObjC Structure

(in-package :coca.objc)

(defparameter *objc-struct-names* (make-hash-table :test 'equal)
  "Cache of ObjC struct names in lisp.

KEY: ObjC struct name string
VAL: lisp struct name symbol")

(defun objc-struct-name (objc-struct-name)
  "Return the OBJC-STRUCT-NAME in lisp environment as a symbol. "
  (or (gethash objc-struct-name *objc-struct-names*)
      (error "Unknown ObjC struct ~A as lisp struct. " objc-struct-name)))

(defparameter *objc-struct-infos* (make-hash-table)
  "Cache of ObjC struct infomation.

Dev Note:
Every lisp struct able to be parsed as ObjC struct
should register it's name in `coca.objc::*objc-structs*'.

KEY: lisp struct symbol
VAL: `objc-struct'. ")

(defstruct objc-struct-info
  (name     "" :type string)
  (len      0  :type integer)
  (slots    () :type list)
  (types    () :type list))

(defun objc-struct-info (struct)
  "Return `coca.objc::objc-struct-info' of STRUCT.
Raise error if STRUCT is not a known ObjC compatible struct. "
  (declare (type (or symbol structure-object) struct))
  (let ((struct (etypecase struct
                  (symbol           struct)
                  (structure-object (type-of struct)))))
    (or (gethash struct *objc-struct-infos*)
        (error "~A is not a known ObjC compatible struct. " struct))))

(defgeneric struct-aref (struct index)
  (:documentation "Fetch element at STRUCT of INDEX.

Dev Note:
This should be used for turning any element into CFFI struct-like
element to be able to pass to ObjC runtime foreign call.

For example:

    (defmethod struct-aref ((rect cg-rect) index)
      (declare (type integer index))
      (coca.objc::case-struct-aref (rect index)
        (cg-point-x (cg-rect-origin rect))
        (cg-point-y (cg-rect-origin rect))
        (cg-size-w  (cg-rect-size   rect))
        (cg-size-h  (cg-rect-size   rect))))

Use `coca.objc::case-struct-aref' for readable `coca.objc::struct-aref'
implementation.

See also `coca.objc::struct-len'.
")
  (:method ((vector simple-vector) (index integer))
    "Struct could be vectorized.

Example:
+ `ns-rect' could also be written as #(x y w h)."
    (svref vector index))
  (:method ((struct structure-object) (index integer))
    "By default, use INDEX-th struct slot of STRUCT to access data.
This is slow but workable. "
    (let* ((info (objc-struct-info struct))
           (rule (nth index (objc-struct-info-slots info))))
      ;; TODO: struct of struct, nested
      (reduce (lambda (struct fetch) (funcall fetch struct))
              (listfy rule)
              :initial-value struct))))

(defmacro case-struct-aref ((struct idx) &body cases)
  "Case IDX by index to select STRUCT parts.
See also `coca.objc::struct-aref'.

Syntax:

    (case-struct-aref (struct idx)
      (expression)
      ...)

Example:

    (case-struct-aref (ns-rect idx)
      (ns-rect-x ns-rect)
      (ns-rect-y ns-rect)
      (ns-rect-w ns-rect)
      (ns-rect-h ns-rect))

"
  `(case ,idx
     ,@(loop :for offset :from 0
             :for case :in cases
             :collect (list offset case))
     (otherwise (error "Invalid index ~D for ~S, should be less than ~D. "
                       ,idx ,struct (objc-struct-info-len (objc-struct-info ,struct))))))

(defmacro define-objc-struct ((name foreign-name) &body slots)
  "Define a foreign structure for use with ObjC.

Syntax:

    (define-objc-struct (NAME FOREIGN-NAME)
       { (SLOT ENCODING :type TYPE) }*)

Parameters:
+ NAME: name of lisp struct
+ FOREIGN-NAME: ObjC name string of struct
+ SLOT: name of structure slot
+ ENCODING: see `objc-encoding'
+ TYPE: (optional) this would change how the slot is stored in Lisp side,
  note that this will toggle type coerce when compiling objc call method

Dev Note:
Sadly, currently does not support nested structure definition.
So the structure should be flattened.

Example:

    (define-objc-struct (ns-point \"CGPoint\")
      (x :double :type real)
      (y :double :type real))

    (define-objc-struct (ns-rect \"CGRect\")
      (x :double :type real)
      (y :double :type real)
      (w :double :type real)
      (h :double :type real))

"
  (declare (type symbol name)
           (type string foreign-name))
  (let ((class-name (intern (str:concat "%" (symbol-name name))))
        slot-names slot-types)
    (loop :for (slot encoding . options) :in slots
          :collect (intern (str:concat (symbol-name name) "-" (symbol-name slot))) :into names
          :collect (if (getf options :type) (cons :coerce encoding) encoding)      :into types
          :finally (setf slot-names names
                         slot-types types))
    `(progn
       (defstruct ,name
         ,@(loop :for (slot encoding . options) :in slots
                 :for type := (getf options :type (objc-encoding-lisp-type encoding))
                 :collect `(,slot ,(objc-encoding-init-value encoding) :type ,type)))
       (defmethod struct-aref ((struct ,name) (index integer))
         (case-struct-aref (struct index)
           ,@(loop :for name :in slot-names
                   :collect `(,name struct))))

       ;; TODO: remove CFFI struct support
       ;; so that i could implement nested objc-struct directly
       ;; but how to support struct return value?
       (defcstruct (,name :class ,class-name)
         ,@(loop :for (slot encoding) :in slots
                 :collect `(,slot ,(objc-encoding-cffi-type encoding))))
       (defmethod translate-from-foreign (ptr (type ,class-name))
         (with-foreign-slots (,(mapcar #'car slots) ptr (:struct ,name))
           (,(intern (str:concat "MAKE-" (symbol-name name)))
            ,@(loop :for (slot) :in slots
                    :collect (intern (symbol-name slot) :keyword)
                    :collect slot))))

       (setf (gethash ,foreign-name *objc-struct-names*) ',name
             (gethash ',name        *objc-struct-infos*)
             (make-objc-struct-info :name      ,foreign-name
                                    :len       ,(length (the list slots))
                                    :slots    ',slot-names
                                    :types    ',slot-types)))))

;;;; struct.lisp ends here
