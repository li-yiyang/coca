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

    (define-objc-typing NAME
      :alias  ALIAS
      :result RESULT
      :arg    ARG)

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

(defun literal-mask-flags (enc flags)
  (if (listp flags)
      (loop :for flag :in flags
            :if (or (keywordp flag)
                    (integerp flag))
              :collect (funcall enc flag) :into literals
            :else
              :collect flag :into exprs
            :finally (return
                       (let ((literal (reduce #'logior literals)))
                         (if exprs
                             (if (zerop literal)
                                 `(,enc ,@exprs)
                                 `(logior ,literal (,enc ,@exprs)))
                             literal))))
      `(,enc ,flags)))

(defmacro define-objc-mask (typing* &body binding)
  "Define ObjC mask typing.

Syntax:

    (define-objc-mask [TYPING|(TYPING &key alias result wrap arg)]
      [DOCSTRING]
      (KEYWORD FLAG-VALUE)
      ...)

+ DOCSTRING: documentation string (optional)
+ KEYWORD, FLAG-VALUE: keyword of flag and responding enum value

Like `define-objc-typing':
+ ALIAS: by default is :unsigned-long
+ RESULT: by default is parsed with DECODE-<TYPING>
+ ARG: additional matching pattern

Two functions are created:
+ (AS-<TYPING> &rest FLAGS) to encode FLAGS as ObjC mask value
+ (DECODE-<TYPING> MASK) to decode MASK as flag(s)

See also `define-objc-enum'. "
  (destructuring-bind (typing &key
                                (alias :unsigned-long)
                                result
                                wrap arg)
      (alx:ensure-list typing*)
    (let ((doc (pop binding))
          (enc (symbol-concat "AS-"     typing))
          (dec (symbol-concat "DECODE-" typing)))
      (unless (stringp doc) (push doc binding))
      `(eval-when (:compile-toplevel :execute :load-toplevel)
         (defun ,enc (&rest flags)
           ,@(when (stringp doc) (list doc))
           (flet ((encode (flag)
                    (etypecase flag
                      (integer flag)
                      (keyword (ecase flag ,@binding))
                      (list    (apply #',enc flag)))))
             (reduce #'logior (mapcar #'encode flags))))
         (defun ,dec (mask)
           ,(format nil "Decode ObjC mask integer ~S.
Return values are decoded flag(s) and original mask.

See also `~A'. "
                    typing enc)
           (declare (type integer mask))
           (let ((flags ()))
             ,@(loop :with zero-flag := nil
                     :for (name val) :in binding
                     :if (zerop val)
                       :do (setf zero-flag name)
                     :else
                       :collect `(unless (zerop (logand ,val mask))
                                   (push ,name flags))
                         :into acc
                     :finally (return
                                (if zero-flag
                                    `(,@acc
                                      (when (null flags)
                                        (return-from ,dec
                                          (values ,zero-flag mask))))
                                    acc)))
             (if (null (cdr flags))
                 (car flags)
                 flags)))
         (define-objc-typing ,typing
           :alias ,alias
           :arg   (((list* flags)
                    (list ,alias (literal-mask-flags ',enc flags)))
                   ((and (type integer) flag)
                    (list ,alias flag))
                   ((and (type keyword) flag)
                    (list ,alias (,enc flag)))
                   ,@arg
                   (flag
                    (list ,alias (list ',enc flag))))
           :result (,alias ,(or result dec))
           ,@(when wrap `(:wrap ,wrap)))))))

(defmacro define-objc-enum (typing* &body binding)
  "Define ObjC enum typing.

Syntax:

    (define-objc-mask [TYPING|(TYPING &key alias result wrap arg)]
      [DOCSTRING]
      (KEYWORD FLAG-VALUE)
      ...)

+ DOCSTRING: documentation string (optional)
+ KEYWORD, FLAG-VALUE: keyword of flag and responding enum value

Like `define-objc-typing':
+ ALIAS: by default is :unsigned-long
+ RESULT: by default is parsed with DECODE-<TYPING>
+ ARG: additional matching pattern

Two functions are created:
+ (AS-<TYPING> FLAG) to encode FLAG as ObjC mask value
+ (DECODE-<TYPING> MASK) to decode MASK as flag

See also `define-objc-mask'. "
  (destructuring-bind (typing &key
                                (alias :unsigned-long)
                                result
                                wrap arg)
      (alx:ensure-list typing*)
    (let ((doc (pop binding))
          (enc (symbol-concat "AS-"     typing))
          (dec (symbol-concat "DECODE-" typing)))
      (unless (stringp doc) (push doc binding))
      `(eval-when (:compile-toplevel :execute :load-toplevel)
         (defun ,enc (flag)
           ,@(when (stringp doc) (list doc))
           (etypecase flag
             (integer flag)
             (keyword (ecase flag ,@binding))))
         (defun ,dec (enum)
           ,(format nil "Decode ObjC ENUM integer ~S.
Return values are decoded flag and original ENUM.

See also `~A'. "
                    typing enc)
           (declare (type integer enum))
           (values
            (case enum
              ,@(loop :for (enum val) :in binding
                      :collect (list val enum))
              (otherwise enum))
            enum))
         (define-objc-typing ,typing
           :alias  ,alias
           :arg    (((and (type keyword) enum)
                     (list ,alias (,enc enum)))
                    ((and (type integer) enum)
                     (list ,alias enum))
                    ,@arg
                    (enum
                     (list ,alias (list ',enc enum))))
           :result (,alias ,(or result dec))
           ,@(when wrap `(:wrap ,wrap)))))))


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

(define-objc-typing :ns-string
  :result (:pointer ns-string-to-string)
  :arg    ((str `(:pointer (string-to-ns-string ,str)))))

(define-objc-typing :ns-url
  :result (:pointer ns-url-to-pathname)
  :arg    ((str `(:pointer (pathname-to-ns-url ,str)))))

(define-objc-typing :ns-uint
  :alias :unsigned-long)

(define-objc-typing :ns-int
  :alias :long)

(define-objc-typing :ns-number
  :alias  :pointer
  :arg    ((num `(:pointer (ns-number ,num)))) )

(define-objc-typing :ns-array
  :result (:pointer ns-array-to-list)
  :arg    ((_ (error "(:ns-array) Not implemented yet... "))))

;;;; typing.lisp ends here
