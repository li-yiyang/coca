;;;; obj.lisp

(in-package :coca.appkit)

(defclass obj ()
  ((ptrs
    :initform (make-hash-table :test 'eq)
    :reader   obj-ptrs))
  (:documentation
   "A ObjC object pointer wrapper. "))

(defgeneric obj-ptr (obj &optional name)
  (:documentation
   "Return foreign-pointer to ObjC `obj'.

Dev Note:
+ the subclass of `obj' should define optional NAME"))

(flet ((expand (obj ptr*)
         (destructuring-bind (ptr &optional name)
             (alx:ensure-list ptr*)
           (assert (symbolp ptr))
           (if (keywordp name)
               `(,ptr (obj-ptr ,obj ,name))
               `(,ptr (obj-ptr ,obj))))))
  (defmacro with-ptr (obj ptr* &body body)
    "Binds PTR of OBJ within BODY.

Syntax:

    (with-ptr OBJ [PTR|(PTR NAME)]
      &body)

+ OBJ: the ObjC object expression
+ PTR: symbol of binded foreign-pointer
+ NAME: keyword refering the foreign-pointer
"
    (alx:with-gensyms (obj*)
      `(let ((,obj* ,obj))
         (let (,(expand obj* ptr*))
           ,@body))))

  (defmacro with-ptrs (obj ptrs &body body)
    "Binds PTRS of OBJ within BODY.

Syntax:

    (with-ptrs OBJ ([PTR|(PTR NAME)]*)
      &body)

+ OBJ: the ObjC object expression
+ PTR: symbol of binded foreign-pointer
+ NAME: keyword refering the foreign-pointer
"
    (alx:with-gensyms (obj*)
      `(let ((,obj* ,obj))
         (let (,(mapcar (alx:curry #'expand obj*) ptrs))
           ,@body)))))

;;;; obj.lisp ends here
