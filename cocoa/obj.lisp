;;;; obj.lisp

(in-package :coca.cocoa)


;;; dispatch-main patch

(defmacro dispatch-main ((&rest keys &key &allow-other-keys) &body body)
  "Invoke BODY within NSApp main thread. "
  (alx:with-gensyms (fn-in-main)
    `(flet ((,fn-in-main () ,@body))
       (if (tmt:main-thread-p)
           (,fn-in-main)
           (coca-app-dispatch (function ,fn-in-main) ,@keys)))))


;;; ObjC Objects

(defvar *objc-objects*
  ;; The `:synchronized' is used to make sure the `*objc-objects*'
  ;; is thread safe in SBCL.
  #+sbcl (tg:make-weak-hash-table :weakness :value :synchronized t)
  ;; For LispWorks, the default make-hash-table is thread safe.
  ;; ref: Hash table thread safety, lisp-hug@lispworks.com, 2026-03-31
  #-sbcl (tg:make-weak-hash-table :weakness :value)
  "ObjC objects cache.

Key: ObjC objects foreign-pointer address
Val: `objc-obj' responding to ObjC objects. ")

(defclass objc-obj ()
  ((ptr
    :initarg  :pointer
    :reader   objc-ptr)
   (objc-class
    :allocation :class
    :type       objc-class
    :reader     objc-class))
  (:documentation
   "Base class representing ObjC object pointer.

Dev Note:
+ use and only use `objc-ptr' to retrive foreign pointer
  do not use `slot-value', which is unsafe
+ use `coerce-to-objc-obj' to wrap foreign-pointer as lisp
  object and ensures it's initialized correctly
+ use `find-objc-obj' or `find-*' (defined by `defalias')
  when invoking from ObjC side (`define-objc-method')

  Example:

      (define-objc-method (CLASS SEL) RET (...)
        (alx:when-let ((obj (find-objc-obj self)))
          ...))

  this ensures calling lisp code from ObjC side is safe
+ use `remove-objc-obj' to unregist from cache
"))

(defgeneric alloc-init (obj)
  (:documentation "Invoke alloc init on OBJ.
Return the foreign-pointer to allocated and initialized OBJ.

Dev Note:
+ this should be called when `initialize-instance'")
  (:method :around ((obj objc-obj))
    (let ((ptr (the foreign-pointer (call-next-method))))
      (regist-objc-obj obj ptr)
      (setf (slot-value obj 'ptr) ptr)
      ptr))
  (:method ((obj objc-obj))
    (init (alloc (slot-value obj 'objc-class)))))

(defgeneric dealloc (obj)
  (:documentation "Called when dealloced from ObjC side.

Dev Note:
+ this will remove PTR (:after) and remhash OBJ from `*objc-objects*'
+ implementation of `dealloc' method should remove other foreign-pointer
  to binded with OBJ
")
  (:method ((obj objc-obj)) t)
  (:method :after ((obj objc-obj))
    (remhash (pointer-address (objc-ptr obj)) *objc-objects*)
    (slot-makunbound obj 'ptr)))

(defmacro with-ptr (objc-obj &body body)
  "Bind `objc-ptr' of OBJC-OBJ with local variable `ptr' within BODY.

Dev Note:
+ use only within `coca.cocoa' package since `ptr'
  may not exported outside package
+ use `with-ptr' within `coca.coca' as much as possible
  to keep code clean
"
  `(let ((ptr (objc-ptr ,objc-obj)))
     ,@body))

(defmethod print-object ((obj objc-obj) stream)
  (print-unreadable-object (obj stream :type t)
    (if (slot-boundp obj 'ptr)
        (format stream "#x~X" (pointer-address (objc-ptr obj)))
        (write-string "not init in ObjC" stream))))

(defun find-objc-obj (ptr)
  "Return cached `objc-obj' from cache by referring PTR's address.
Return values are OBJ (can be `nil') and `t' or `nil' for if found.

Dev Note:
+ use `alx:when-let' to wrap success code in `define-objc-method'
"
  (declare (type foreign-pointer ptr))
  (gethash (pointer-address ptr) *objc-objects*))

(defun regist-objc-obj (obj &optional (ptr (objc-ptr obj)))
  "Regist ObjC OBJ with PTR in `*objc-objects*'.
Return OBJ itself.

Dev Note:
+ regist it when `initialize-instance' to ensure OBJ is findable
  by `find-objc-obj'
"
  (declare (type objc-obj obj)
           (type foreign-pointer ptr))
  (setf (gethash (pointer-address ptr) *objc-objects*) obj))


;;;; Utils

(defmacro define-objc-mask (name &body binding)
  "Define ObjC mask generation function of NAME.

Syntax:

    (define-objc-mask NAME
      [DOCSTRING]
      (KEYWORD FLAG-VALUE)
      ...)

+ DOCSTRING: documentation string (optional)
+ KEYWORD, FLAG-VALUE: keyword of flag and responding enum value"
  (let ((doc (pop binding)))
    (unless (stringp doc) (push doc binding))
    `(defun ,name (flags)
       ,@ (when (stringp doc) (list doc))
       (declare (type (or keyword list) flags))
       (flet ((encode (flag) (ecase flag ,@binding)))
         (reduce #'logior (mapcar #'encode (alx:ensure-list flags)))))))

(defvar *global-objc-objects-variables* ()
  "A list of global ObjC object variables names.

These should be cleared as `nil' everytime initialize.
Use `define-objc-global-variable' to define global
variable accessor function. ")

(defun ensure-global-objc-objects ()
  (dolist (var *global-objc-objects-variables*)
    (setf (symbol-value var) nil)))

(defmacro define-objc-global-variable (name initialize-form &optional documentation)
  "Define ObjC global variable accessor function of NAME.
The global variable is initialized with INITIALIZE-FORM. "
  (let ((var (intern (str:concat "*" (string name) "*"))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,var nil)
       (pushnew ',var *global-objc-objects-variables*)
       (defun ,name () ,documentation
         (or ,var (setf ,var ,initialize-form))))))

;;;; obj.lisp ends here
