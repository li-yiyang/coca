;;;; resources.lisp --- ObjC Resources CFFI Wrapper

;; This file manages ObjC runtime resources.

;; Design of a minimum ObjC binding layer
;; The ideas are based on the project github:li-yiyang/coca.
;; + classes, sels and objects are stored as plain struct
;; + invoke, invoke-super are thin wrapper layer of `cffi:foreign-funcall'
;; + only support M-series chip (arm64)

(in-package :coca.objc)

(defvar *objc-libraries* ()
  "A list of foreign libraries names.

To load new ObjC libraries:

    (define-foreign-library lib-name
      (:darwin (:framework \"...\")))
    (load-foreign-library 'lib-name)
    (pushnew 'lib-name *objc-libraries*)

")

(define-foreign-library foundation
  (:darwin (:framework "Foundation")))
(load-foreign-library 'foundation)
(pushnew 'foundation *objc-libraries*)


;;; Define ObjC class

(defvar *dynamic-objc-classes* ())

(defun %define-objc-class (class superclass &rest protocols)
  (declare (type string class superclass))
  (when (null-pointer-p (objc_getClass class))
    (let* ((super     (coerce-to-objc-class superclass))
           (class-ptr (foreign-funcall "objc_allocateClassPair"
                                       :pointer (objc-class-ptr super)
                                       :string  class
                                       :size    0
                                       :pointer)))
      (dolist (protocol protocols)
        (declare (type string protocol))
        (let ((protocol-ptr (foreign-funcall "objc_getProtocol"
                                             :string protocol
                                             :pointer)))
          (unless (null-pointer-p protocol-ptr)
            (let ((res (foreign-funcall "class_addProtocol"
                                        :pointer class-ptr
                                        :pointer protocol-ptr
                                        :bool)))
              (unless res
                (warn "Failed to add Protocol ~A to Class ~A"
                      protocol class))))))
      (foreign-funcall "objc_registerClassPair"
                       :pointer class-ptr)
      (pushnew `(,class ,superclass ,@protocols) *dynamic-objc-classes*
               :key #'first :test #'string=)
      ;; force update objc-class-ptr
      (let ((objc-class (coerce-to-objc-class class)))
        (setf (objc-class-ptr objc-class) class-ptr)))))

(defmacro define-objc-class (class superclass &rest protocols)
  "Define ObjC Class if needed.

This is equal to calling ObjC Code:

@interface CLASS : SUPERCLASS < PROTOCOLS... >
@end

+ CLASS:
  string of ObjC Class
+ SUPERCLASS:
  string of superclass of ObjC Class
+ PROTOCOLS:
  strings of ObjC Protocols
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%define-objc-class ,class ,superclass ,@protocols)))

(defcfun (class_replaceMethod "class_replaceMethod") :bool
  (class :pointer)
  (sel   :pointer)
  (imp   :pointer)
  (types :pointer))

(defparameter *dynamic-objc-methods* (make-hash-table :test 'equal))

(defun %define-objc-method (class sel encoding callback
                            &optional persistance)
  (declare (type objc-class       class)
           (type sel              sel)
           (type (or null string) encoding)
           (type symbol           callback))
  (if encoding
      (with-foreign-string (type encoding)
        (class_replaceMethod
         (objc-class-ptr class)
         (sel-ptr        sel)
         (get-callback   callback)
         type))
      (class_replaceMethod
       (objc-class-ptr class)
       (sel-ptr        sel)
       (get-callback   callback)
       (null-pointer)))
  (when persistance
    (setf (gethash (list class sel) *dynamic-objc-methods*)
          (list class sel encoding callback)))
  callback)

(defmacro define-objc-method ((class sel &key encoding callback)
                              return-type
                              lambda-list
                              &body body)
  "Define ObjC instance method.

Syntax:

    (define-objc-method (CLASS SEL &key ENCODING CALLBACK)
         RETURN-TYPE
         LAMBDA-LIST
      &body
      SELF...)

+ CLASS:
  literal string of ObjC class name
+ SEL:
  literal string of ObjC SEL name
+ ENCODING:
  method encoding type string
  if not given, will use class_replaceMethod with types as NULL pointer;
  if given, should be like a string starting with @:
+ CALLBACK
  custom CFFI callback symbol name
+ RETURN-TYPE:
  cffi return type with ObjC type enhanced
  + `:object'  same as :pointer
  + `:class'   return value will be wrapped as `objc-class-ptr'
  + `:sel'     return value will be wrapped as `sel-ptr'
  + otherwise see CFFI foreign types
+ LAMBDA-LIST:
  same as `cffi:defcallback', or defined in `define-objc-typing'
+ BODY
  method body,
  within the BODY, self is bound to foreign-pointer of ObjC object
"
  (declare (type string class sel)
           (type (or null string) encoding)
           (type symbol callback))
  (let ((sel*     (gensym "SEL"))
        (callback (or callback
                      (intern (concatenate 'string
                                           "%OBJC-METHOD-"
                                           (string class)
                                           "-"
                                           (string sel)))))
        (declaration ())
        (progn       ()))
    (loop :for (decl . rest) :on body
          :if (and (listp decl) (eq (car decl) 'declare))
            :do (push decl declaration)
          :else
            :return (setf progn (cons decl rest)))
    `(progn
       (defcallback ,callback ,(case return-type
                                 ((:object :class :sel) :pointer)
                                 (otherwise return-type))
           ((self  :pointer)
            (,sel* :pointer)
            ,@(loop :for (arg* type*) :in lambda-list
                    :for expanded := (expand-invoke-arg type* arg*)
                    :nconc (loop :for (type) :on expanded :by #'cddr
                                 :for var :in (alx:ensure-list arg*)
                                 :collect (list var type))))
         (declare (ignore ,sel*)
                  (ignorable self))
         ,@declaration
         (with-autorelease-pool
           ,(case return-type
              (:class    `(objc-class-ptr (the objc-class (progn ,@progn))))
              (:sel      `(sel-ptr        (the sel        (progn ,@progn))))
              (:void     `(restart-case (progn ,@progn)
                            (ignore ()
                              :report ,(format nil "Ignore error for [~A ~A]" class sel))))
              (otherwise `(restart-case (progn ,@progn)
                            (return-value (value)
                              :report ,(format nil "Return CFFI value of ~S for [~A ~A]"
                                               return-type class sel)
                              :interactive (lambda () (multiple-value-list (eval (read))))
                              value))))))
       (%define-objc-method (coerce-to-objc-class ,class)
                            (coerce-to-selector   ,sel)
                            ,encoding
                            ',callback
                            t)
       ',callback)))


;;; Image Dump

(declaim (type (or null (cons (or function symbol) list))
               *on-objc-initialization*))
(defvar *on-objc-initialization* ()
  "A list of ObjC initialize callbacks.

The element should be callback function symbol
or function to call after `ensure-objc-initialized'.

The function will be called with no argument. ")

(defun ensure-objc-initialized ()
  "Restore ObjC environment after restore image.

Dev Note:
1. the ObjC libraries will be reloaded first

   Example: add custom ObjC library

   (define-foreign-library OBJC-LIB
     (:darwin (:framework \"ObjCLibrary\"))
   (pushnew 'OBJC-LIB *objc-libraries*)

2. the SEL and Class pointer will be rebinded,
   and for thoese dynamic created ObjC class,
   they will be registed at this stage

   it is strongly not recommanded to directly
   store foreign-pointer of `sel' or `objc-class'

3. the global ObjC variables defined by
   `define-objc-global-variable' will be re-initialized
   using their INITIALIZE-FORM

4. call hooks in `*on-objc-initialization*'"
  (mapcar #'load-foreign-library *objc-libraries*)
  (maphash (lambda (name sel)
             (setf (sel-ptr sel) (sel_registerName name)))
           *sels*)
  (maphash (lambda (name class)
             (setf (objc-class-ptr class) (objc_getClass name)))
           *objc-classes*)
  ;; regist dynamic class
  (dolist (args *dynamic-objc-classes*)
    (apply #'%define-objc-class args))
  ;; regist objc methods
  (alx:maphash-values
   (lambda (args) (apply #'%define-objc-method args))
   *dynamic-objc-methods*)
  ;; global objc variables
  (dolist (var *global-objc-objects-variables*)
    (setf (symbol-value var) nil))
  ;; callback trigger
  (mapcar #'funcall *on-objc-initialization*)
  t)

;;; global objc variables

(defvar *global-objc-objects-variables* ()
  "A list of global ObjC object variables names.

These should be cleared as `nil' everytime initialize.
Use `define-objc-global-variable' to define global
variable accessor function. ")

(defmacro define-objc-global-variable (name initialize-form &optional documentation)
  "Define ObjC global variable accessor function of NAME.
The global variable is initialized with INITIALIZE-FORM.

Syntax:

    (define-objc-global-variable NAME
        INITIALIZE-FORM
      [DOCUMENTATION])

+ NAME: symbol of global reader function name
+ INITIALIZE-FORM: initialization form
+ DOCUMENTATION: optional documentation string for NAME function

Dev Note:
+ there would also be a global variable with name like `*NAME*'
  it should not be used directly since it should be cleared
  when ObjC runtime initialized and might not properly initialized;
+ this macro acts like `defvar', so if the global variable
  is defined already (and non-nil), it's INITIALIZE-FORM should
  only run once.
"
  (let ((var (symbol-concat "*" name "*")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,var nil)
       (pushnew ',var *global-objc-objects-variables*)
       (defun ,name () ,documentation
         (or ,var (setf ,var ,initialize-form))))))


;;; Memory Managment

(defcfun (objc_autoreleasePoolPush "objc_autoreleasePoolPush") :pointer)

(defcfun (objc_autoreleasePoolPop "objc_autoreleasePoolPop") :void
  (pool :pointer))

(defmacro with-autorelease-pool (&body body)
  "Wrap BODY within ObjC autorelease pool block. "
  (let ((pool (gensym "POOL")))
    `(let ((,pool (objc_autoreleasePoolPush)))
       (unwind-protect (progn ,@body)
         (objc_autoreleasePoolPop ,pool)))))


;;; Utils

(defun objc-symbol-value (symbol type &optional (library :default))
  "Lookup foreign ObjC SYMBOL value as TYPE.

Possible TYPEs:
+ :object same as :pointer
+ :class  interned as `objc-class'
+ :sel    interned as `sel'
+ otherwise, CFFI foreign types
"
  (declare (type string symbol))
  (let ((ptr (foreign-symbol-pointer symbol :library library)))
    (case type
      (:object (mem-ref ptr :pointer))
      (:class  (coerce-to-objc-class (mem-ref ptr :pointer)))
      (:sel    (coerce-to-selector (mem-ref ptr :pointer)))
      (otherwise (mem-ref ptr type)))))

;;;; resources.lisp ends here
