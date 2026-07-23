;;;; typing.lisp --- Extensive ObjC type for `invoke' macro

(in-package :coca.objc)


;;;; Data types

;; Concepts Mapping
;;
;; | ObjC  | Lisp            |
;; |-------+-----------------|
;; | Class | objc-class      |
;; | SEL   | sel             |
;; | id    | foreign-pointer |
;;
;; `objc-class', `sel' are cached by name
;; id is mapped as `foreign-pointer' directly

(defvar *objc-classes* (make-hash-table :test 'equal)
  "Cache of `objc-class'.
Key: string of ObjC class name
Val: `objc-class' for ObjC Class")

(defvar *sels* (make-hash-table :test 'equal)
  "Cache of `sel'.
Key: string of `sel' name.
Val: `sel' for ObjC SEL")

(defstruct objc-class
  (ptr   nil :type foreign-pointer)
  (name  ""  :type string))

(defcfun (objc_getClass "objc_getClass") :pointer
  (class :string))

(defun coerce-to-objc-class (class)
  "Return cached `objc-class' for ObjC Class. "
  (declare (type (or string foreign-pointer objc-class) class))
  (the objc-class
    (etypecase class
      (string
       (alx:ensure-gethash class *objc-classes*
           (let ((ptr (objc_getClass class)))
             (when (null-pointer-p ptr)
               (error "Unknown ObjC Class ~A. " class))
             (make-objc-class :name class
                              :ptr ptr))))
      (foreign-pointer
       (unless (foreign-funcall "object_isClass"
                                :pointer class
                                :bool)
         (error "~A is not a ObjC pointer to Class. " class))
       (let ((name (foreign-funcall "class_getName"
                                    :pointer class
                                    :string)))
         (alx:ensure-gethash name *objc-classes*
             (make-objc-class :name name
                              :ptr class))))
      (objc-class class))))

(defmethod make-load-form ((class objc-class) &optional environment)
  (declare (ignore environment))
  `(coerce-to-objc-class ,(objc-class-name class)))

(defmethod print-object ((class objc-class) stream)
  (print-unreadable-object (class stream)
    (format stream
            "~A ~X"
            (objc-class-name class)
            (pointer-address (objc-class-ptr class)))))

(defstruct sel
  (ptr   nil :type foreign-pointer)
  (name  ""  :type string))

(defcfun (sel_registerName "sel_registerName") :pointer
  (sel :string))

(defun coerce-to-selector (sel)
  "Return cached `sel' for ObjC SEL. "
  (declare (type (or string foreign-pointer sel) sel))
  (the sel
    (etypecase sel
      (string
       (alx:ensure-gethash sel *sels*
           (make-sel :name sel
                     :ptr (sel_registerName sel))))
      (foreign-pointer
       (let ((name (foreign-funcall "sel_getName"
                                    :pointer sel
                                    :string)))
         (alx:ensure-gethash name *sels*
             (make-sel :ptr sel
                       :name name))))
      (sel sel))))

(defmethod make-load-form ((sel sel) &optional environment)
  (declare (ignore environment))
  `(coerce-to-selector ,(sel-name sel)))

(defmethod print-object ((sel sel) stream)
  (print-unreadable-object (sel stream :type t)
    (format stream
            "~A ~X"
            (sel-name sel)
            (pointer-address (sel-ptr sel)))))

(defun objc-pointer (thing)
  "Return `foreign-pointer' of THING. "
  (declare (type (or foreign-pointer string objc-class sel null) thing))
  (the foreign-pointer
    (etypecase thing
      (foreign-pointer thing)
      (string          (objc-class-ptr (coerce-to-objc-class thing)))
      (objc-class      (objc-class-ptr thing))
      (sel             (sel-ptr thing))
      (null            (null-pointer)))))


;;; ObjC Structure

(defstruct (ns-point (:constructor %ns-point (x y)))
  (x 0d0 :type double-float)
  (y 0d0 :type double-float))

(defstruct (ns-size (:constructor %ns-size (w h)))
  (w 100d0 :type (double-float 0d0))
  (h 100d0 :type (double-float 0d0)))

(defstruct (ns-rect (:constructor %ns-rect (origin size)))
  (origin (ns-point) :type ns-point)
  (size   (ns-size)  :type ns-size))

(defun ns-point (&key (x 0d0) (y 0d0))
  "Return a `ns-point' at X, Y. "
  (declare (type real x y))
  (%ns-point (coerce x 'double-float)
             (coerce y 'double-float)))

(defun ns-size (&key (w 0d0) (h 0d0))
  "Return a `ns-point' at W, H. "
  (declare (type (real 0) w h))
  (%ns-size (coerce w 'double-float)
            (coerce h 'double-float)))

(defun ns-rect (&key origin size (x 0d0) (y 0d0) (w 100d0) (h 100d0))
  "Return a `ns-rect' of ORIGIN and SIZE.
The given X, Y, W, H will update ORIGIN and SIZE. "
  (declare (type real x y)
           (type (real 0) w h))
  (%ns-rect (or origin (%ns-point (coerce x 'double-float)
                                  (coerce y 'double-float)))
            (or size   (%ns-size  (coerce w 'double-float)
                                  (coerce h 'double-float)))))

(declaim (inline ns-rect-x ns-rect-y ns-rect-w ns-rect-h))

(defun ns-rect-x (ns-rect)
  (declare (type ns-rect ns-rect))
  (ns-point-x (ns-rect-origin ns-rect)))

(defun (setf ns-rect-x) (value ns-rect)
  (declare (type ns-rect ns-rect)
           (type real value))
  (setf (ns-point-x (ns-rect-origin ns-rect))
        (coerce value 'double-float)))

(defun ns-rect-y (ns-rect)
  (declare (type ns-rect ns-rect))
  (ns-point-y (ns-rect-origin ns-rect)))

(defun (setf ns-rect-y) (value ns-rect)
  (declare (type ns-rect ns-rect)
           (type real value))
  (setf (ns-point-y (ns-rect-origin ns-rect))
        (coerce value 'double-float)))

(defun ns-rect-w (ns-rect)
  (declare (type ns-rect ns-rect))
  (ns-size-w (ns-rect-size ns-rect)))

(defun (setf ns-rect-w) (value ns-rect)
  (declare (type ns-rect ns-rect)
           (type real value))
  (setf (ns-size-w (ns-rect-size ns-rect))
        (coerce value 'double-float)))

(defun ns-rect-h (ns-rect)
  (declare (type ns-rect ns-rect))
  (ns-size-h (ns-rect-size ns-rect)))

(defun (setf ns-rect-h) (value ns-rect)
  (declare (type ns-rect ns-rect)
           (type real value))
  (setf (ns-size-h (ns-rect-size ns-rect))
        (coerce value 'double-float)))

(defcstruct (%c-ns-rect :class c-ns-rect)
  (x :double)
  (y :double)
  (w :double)
  (h :double))

(defmethod translate-from-foreign (ptr (type c-ns-rect))
  (with-foreign-slots ((x y w h) ptr (:struct %c-ns-rect))
    (declare (type double-float x y w h))
    (%ns-rect (%ns-point x y) (%ns-size w h))))

(defmethod expand-from-foreign (ptr (type c-ns-rect))
  `(with-foreign-slots ((x y w h) ,ptr (:struct %c-ns-rect))
     (declare (type double-float x y w h))
     (%ns-rect (%ns-point x y) (%ns-size w h))))

(defmethod translate-into-foreign-memory ((rect ns-rect) (type c-ns-rect) ptr)
  (with-foreign-slots ((x y w h) ptr (:struct %c-ns-rect))
    (setf x (ns-rect-x rect)
          y (ns-rect-y rect)
          w (ns-rect-w rect)
          h (ns-rect-h rect))))

(defcstruct (%c-ns-point :class c-ns-point)
  (x :double)
  (y :double))

(defmethod translate-from-foreign (ptr (type c-ns-point))
  (with-foreign-slots ((x y) ptr (:struct %c-ns-rect))
    (ns-point :x x :y y)))

(defmethod expand-from-foreign (ptr (type c-ns-point))
  `(with-foreign-slots ((x y) ,ptr (:struct %c-ns-rect))
     (ns-point :x x :y y)))

(defmethod translate-into-foreign-memory ((point ns-point) (type c-ns-point) ptr)
  (with-foreign-slots ((x y) ptr (:struct %c-ns-point))
    (setf x (ns-point-x point)
          y (ns-point-y point))))

(defcstruct (%c-ns-size :class c-ns-size)
  (w :double)
  (h :double))

(defmethod translate-from-foreign (ptr (type c-ns-size))
  (with-foreign-slots ((w h) ptr (:struct %c-ns-size))
    (ns-size :w w :h h)))

(defmethod expand-from-foreign (ptr (type c-ns-size))
  `(with-foreign-slots ((w h) ,ptr (:struct %c-ns-size))
     (ns-size :w w :h h)))

(defmethod translate-into-foreign-memory ((size ns-size) (type c-ns-size) ptr)
  (with-foreign-slots ((w h) ptr (:struct %c-ns-size))
    (setf w (ns-size-w size)
          h (ns-size-h size))))


;;;; Typing alias and mapping

(defvar *objc-invoke-typing* (make-hash-table)
  "Typing expending and wrapping rules for `invoke'.

Key: ObjC typing
Val: property list as `invoke' macro rules. ")

(defun wrap-invoke-result (typing expr)
  "For those `invoke' returned EXPR marked as encoding of TYPING,
wrap the EXPR by rules if needed.

Rule:
+ (:wrap FN) -> (FN EXPR)
"
  (let ((rule (getf (gethash typing *objc-invoke-typing*) :wrap)))
    (if rule (list rule expr) expr)))

(defun invoke-result-cffi-type (typing)
  "The `invoke' result encoding of TYPING is replaced by rule.

Rule:
+ (:result CFFI-TYPE) -> CFFI-TYPE
"
  (let ((rule (getf (gethash typing *objc-invoke-typing*) :result)))
    (if rule rule typing)))

(defun expand-invoke-arg (typing expr)
  "Expand `invoke' argument EXPR of TYPING as a list.

Rule:
+ (:arg TRANSFORM)
  + TRANSFORM : CFFI-TYPE
  + TRANSFORM : FUNCTION
"
  (let ((rule (getf (gethash typing *objc-invoke-typing*) :arg)))
    (cond ((null      rule) (list    typing expr))
          ((functionp rule) (funcall rule   expr))
          (t                (list    rule   expr)))))

(defun expand-invoke-args (args)
  (loop :for (typing expr) :on args :by #'cddr
        :for (call . rewrite*) := (multiple-value-list
                                   (expand-invoke-arg typing expr))
        :for rewrite := (car rewrite*)
        :if rewrite
          :collect rewrite :into cffi-rewrites
        :nconc call :into cffi-args
        :finally (return (values cffi-args cffi-rewrites))))

(defun %define-objc-typing (name &key alias result arg wrap)
  (let ((plist (copy-list (and alias (gethash alias *objc-invoke-typing*)))))
    (flet ((setp (key val) (setf (getf plist key) val)))
      (cond (plist ;; ALIAS is registed ObjC typing
             (when result (setp :result result))
             (when arg    (setp :arg    arg))
             (when wrap   (setp :wrap   wrap)))
            (t ;; consider ALIAS as CFFI-TYPE
             (setp :result (or result alias))
             (setp :arg    (or arg    alias))
             (when wrap (setp :wrap wrap)))))
    (setf (gethash name *objc-invoke-typing*) plist)
    name))

(defun rewrite-invoke (rewrites invoke)
  (if (endp rewrites)
      invoke
      (rewrite-invoke (cdr rewrites)
                      `(,@(alx:ensure-list (car rewrites))
                        ,invoke))))

(defmacro define-objc-typing (name &key alias result arg)
  "Define ObjC typing of NAME.

Syntax:
+ NAME:  (recommanded to be) a keyword for ObjC typing name
+ ALIAS:
  + CFFI typing name
  + another ObjC typing NAME
+ RESULT:
  + CFFI type as `invoke' result
  + (CFFI-TYPE WRAP)
    + WRAP as `invoke' result wrap function
+ ARG:
  + CFFI type name as argument type
  + a list of (PATTERN EXPAND)

    + see `optima:match' for rule of PATTERN,
    + EXPAND will be evaluated to expand the result
    + if not PATTERN is matched, throw error when `expand-invoke-arg'
    + EXPAND can return two values (EXPAND REWRITE)
      + REWRITE will be used to wrap invoke like:

        (,@REWRITE (invoke ...))
"
  (destructuring-bind (cffi-type &optional wrap)
      (alx:ensure-list (or result alias))
    `(%define-objc-typing
      ',name
      :alias  ',alias
      :result ',cffi-type
      :wrap   ',wrap
      :arg    ,(if (atom arg)
                   `',arg
                   (alx:with-gensyms (expr)
                     `(lambda (,expr)
                        (m:match ,expr
                          ,@arg
                          (_ (error "Cannot expand ~A for ObjC typing ~S. "
                                    ,expr ',name)))))))))


;;;; ObjC typing from resources.lisp

(define-objc-typing :class
  :result (:pointer coerce-to-objc-class)
  :arg    (((and (type string) class)
            `(:pointer (objc-class-ptr ,(coerce-to-objc-class class))))
           (class
            `(:pointer (objc-class-ptr ,class)))))

(define-objc-typing :sel
  :result (:pointer coerce-to-selector)
  :arg    (((and (type string) sel)
            `(:pointer (sel-ptr ,(coerce-to-selector sel))))
           (sel
            `(:pointer (sel-ptr ,sel)))))

(define-objc-typing :object
  :alias :pointer)

(defun %expr-as-double (expr)
  (if (realp expr)
      (coerce expr 'double-float)
      `(coerce ,expr 'double-float)))

(define-objc-typing :ns-rect
  :result ((:struct %c-ns-rect))
  :arg    (((vector x y w h)
            `(:double ,(%expr-as-double x)
              :double ,(%expr-as-double y)
              :double ,(%expr-as-double w)
              :double ,(%expr-as-double h)))
           (ns-rect
            (let ((frame (gensym "NS-RECT")))
              (values `(:double (ns-rect-x ,frame)
                        :double (ns-rect-y ,frame)
                        :double (ns-rect-w ,frame)
                        :double (ns-rect-h ,frame))
                      `(let ((,frame ,ns-rect))))))))

(define-objc-typing :ns-point
  :result ((:struct %c-ns-point))
  :arg    (((vector x y)
            `(:double ,(%expr-as-double x)
              :double ,(%expr-as-double y)))
           (ns-point
            (let ((pos (gensym "NS-POINT")))
              (values `(:double (ns-point-x ,pos)
                        :double (ns-point-y ,pos))
                      `(let ((,pos ,ns-point))))))))

(define-objc-typing :ns-size
  :result ((:struct %c-ns-size))
  :arg    (((vector w h)
            `(:double ,(%expr-as-double w)
              :double ,(%expr-as-double h)))
           (ns-size
            (let ((size (gensym "NS-SIZE")))
              (values `(:double (ns-size-w ,size)
                        :double (ns-size-h ,size))
                      `(let ((,size ,ns-size))))))))

(define-objc-typing :ns-string
  :result (:pointer ns-string-to-string)
  :arg    ((str `(:pointer (string-to-ns-string ,str)))))

(define-objc-typing :ns-number
  :alias  :pointer
  :arg    ((num `(:pointer (ns-number ,num)))) )

(define-objc-typing :ns-array
  :result (:pointer ns-array-to-list)
  :arg    ((_ (error "(:ns-array) Not implemented yet... "))))

;;;; typing.lisp ends here
