;;;; obj.lisp

(in-package :coca.appkit)

(defclass obj ()
  ((ptrs
    :initform (make-hash-table :test 'eq)
    :reader   objc-ptrs))
  (:documentation
   "A ObjC object pointer wrapper. "))

(defmethod initialize-instance :after ((obj obj) &key ptr)
  (when ptr
    (setf (gethash :ptr (objc-ptrs obj))
          (the foreign-pointer ptr))))

(defmethod print-object ((obj obj) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((ptr (gethash :ptr (objc-ptrs obj))))
      (if ptr
          (format stream "#x~X" (pointer-address ptr))
          (format stream "destroyed")))))

(defgeneric objc-ptr (obj name)
  (:documentation
   "Return foreign-pointer to ObjC `obj'.

Dev Note:
+ the subclass of `obj' should always define method
  `objc-ptr' with NAME as nil")
  (:method (obj name)
    (or (gethash name (objc-ptrs obj))
        (error "Unknow ObjC pointer of ~S for ~A. " name obj))))

(defmethod (setf objc-ptr) (ptr (obj obj) name)
  (declare (type foreign-pointer ptr))
  (setf (gethash name (objc-ptrs obj)) ptr))

(defun obj-ptr (obj &optional (name :ptr))
  "Return foreign-pointer to ObjC OBJ of NAME. "
  (the foreign-pointer
    (objc-ptr obj name)))

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


;;;; find-obj-mixin

(defvar *objs*
  (tg:make-weak-hash-table :weakness :value)
  "A lookup table for foreign ObjC pointer to find ObjC object.

Use `find-obj' to get the corresponding `obj' in lisp side.

Key: ObjC foreign-pointer address
Val: lisp `obj'

Dev Note:
+ use `find-obj-mixin' in your `obj' class")

(defun clear-objs () (clrhash *objs*))
(pushnew 'clear-objs *on-objc-initialization*)

(defclass find-obj-mixin () ()
  (:documentation
   "Mixin class for `obj' that can be find via `find-obj'.

Dev Note:
+ the instance of `find-obj-mixin' is not persistent,
  so you cannot expect your ObjC object restored
  automatically
+ you can use `define-global-objc-variable' if you
  want some persistence global variables
+ or you can register your own restore functions
  in `*on-objc-initialization*'
"))

(defun regist-obj-ptr (obj ptr)
  (declare (type find-obj-mixin obj)
           (type foreign-pointer ptr))
  (setf (gethash (pointer-address ptr) *objs*) obj))

(defmethod initialize-instance :after ((obj find-obj-mixin) &key)
  (alx:maphash-values (alx:curry #'regist-obj-ptr obj)
                      (objc-ptrs obj)))

(defmethod (setf objc-ptr) :after (ptr (obj find-obj-mixin) name)
  (regist-obj-ptr obj ptr))

(defun find-obj (ptr)
  "Find the `obj' of PTR or return nil. "
  (declare (type foreign-pointer ptr))
  (gethash (pointer-address ptr) *objs*))

(defmacro ensure-find-obj (ptr &body body)
  "Ensure `find-obj' with BODY result as missing value. "
  `(alx:ensure-gethash (pointer-address ,ptr) *objs* (progn ,@body)))


;;;; owned-mixin

(defclass owned-mixin ()
  ()
  (:documentation
   "The instance is owned by Lisp.

If initialized by wrapping :ptr, the ObjC PTR wrapped
is retained until lisp object is finalized.

If initialized with no :ptr, the ObjC PTR is allocated
with :objc-class and initialized using lisp process
:objc-init.

Initialize Parameters:
+ PTR: foreign-pointer to ObjC pointer to be wrapped
  if not given, the :ptr of obj will be initialized
  like:

      (funcall OBJC-INIT (alloc OBJC-CLASS))

+ OBJC-CLASS: if not given PTR, the ObjC pointer
  is allocated with OBJC-CLASS
+ OBJC-INIT: by default to be `init'
  should be a function given with allocated
  ObjC pointer
"))

(defmethod initialize-instance ((obj owned-mixin)
                                &key ptr
                                  (objc-class "NSObject")
                                  (objc-init #'init)
                                  init-in-main-p)
  (call-next-method)
  (let ((ptr (if ptr
                 (retain ptr)
                 (setf (gethash :ptr (objc-ptrs obj))
                       (let ((ptr (alloc objc-class)))
                         (if init-in-main-p
                             (dispatch-main ()
                               (funcall objc-init ptr))
                             (funcall objc-init ptr))
                         ptr)))))
    (flet ((finalize () (release ptr)))
      (tg:finalize obj #'finalize))))

(defgeneric destroy (obj)
  (:documentation
   "Destory OBJ, deowned in lisp. ")
  (:method (obj) ())
  (:method ((obj owned-mixin))
    (alx:maphash-values (lambda (ptr)
                          (remhash (pointer-address ptr) *objs*))
                        (objc-ptrs obj))
    (tg:cancel-finalization obj)
    (release (objc-ptr obj :ptr))
    (clrhash (objc-ptrs obj))))

;;;; obj.lisp ends here
