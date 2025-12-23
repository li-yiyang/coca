;;;; const.lisp

(in-package :coca.objc)

(defmacro define-objc-const (name (objc-name objc-encoding &optional (library :default))
                             &optional documentation)
  "Define a ObjC constant of OBJC-NAME in lisp as NAME.

Syntax:

    (define-objc-const (name objc-name objc-encoding &optional library)
      [documentation])

Parameters:
+ NAME: lisp constant name
+ OBJC-NAME: objc global varible name
+ OBJC-ENCODING: should be ObjC-encoding literally
+ LIBRARY: library to search the ObjC global const
+ DOCUMENTATION: optional documentation"
  (declare (type symbol name)
           (type string objc-name)
           (type objc-encoding objc-encoding)
           (type (or null string) documentation))
  (case (atomize objc-encoding)
    ((:union :array)  (error "Don't support `:union' yet. "))
    ((:struct :pointer)
     `(defparameter ,name
        (cffi:mem-ref (cffi::fs-pointer-or-lose ,objc-name ',library)
                      ,(objc-encoding-cffi-type objc-encoding))
        ,documentation))
    ((:object)
     `(defparameter ,name
        (coerce-to-objc-object
         (cffi:mem-ref (cffi::fs-pointer-or-lose ,objc-name ',library)
                       :pointer))
        ,documentation))
    ((:class)
     `(defparameter ,name
        (coerce-to-objc-class
         (cffi:mem-ref (cffi::fs-pointer-or-lose ,objc-name ',library)
                       :pointer))
        ,documentation))
    ((:sel)
     `(defparameter ,name
        (coerce-to-selector
         (cffi:mem-ref (cffi::fs-pointer-or-lose ,objc-name ',library)
                       :pointer))
        ,documentation))
    (otherwise
     `(defconstant ,name
        (cffi:mem-ref (cffi::fs-pointer-or-lose ,objc-name ',library)
                      ,(objc-encoding-cffi-type objc-encoding))
        ,documentation))))

(trivial-indent:define-indentation define-objc-const (2 4 &body))

;;;; const.lisp ends here
