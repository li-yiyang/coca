;;;; block.lisp ---- Implementation of ObjC block bindings

(in-package :coca.objc)


;;;; Block Descriptor

;; Dev Note:
;; see https://clang.llvm.org/docs/Block-ABI-Apple.html
;; for Block Layout definition
;; or use clang -rewrite-objc balabala.m to modify the
;; `block-layout' and `block-descriptor'

(defcstruct block-descriptor
  (reserved :unsigned-long)
  (size     :unsigned-long))

(defcstruct block-layout
  (isa        :pointer)
  (flags      :int)
  (reserved   :int)
  (invoke     :pointer)
  (descriptor :pointer)
  ;; The UUID is used to fetch Lisp callback function
  (uuid       :int))

(define-objc-global-variable block-layout-descriptor
    (let ((desc (foreign-alloc '(:struct block-descriptor))))
      (setf (foreign-slot-value desc '(:struct block-descriptor) 'reserved) 0)
      (setf (foreign-slot-value desc '(:struct block-descriptor) 'size)
            (foreign-type-size '(:struct block-layout)))
      desc)
  "Descriptor to `block-layout'. ")


;;;; Block Layout

(defconstant +block-callback-max+ 128
  "Maximum number of ObjC Blocks.

Dev Note:
+ change this to larger value if you encounted `*block-callback*' full
  error (is this possible? )")

(defvar *block-callback*
  (make-array +block-callback-max+ :initial-element nil)
  "Block callback registed in Lisp side.

Dev Note:
+ use `%regist-block-callback' to store lisp function as ObjC Block callback
+ use `%remove-block-callback' to remove callback
+ these two functions should not be called by user
")

(defvar *block-callback-lock* (bt:make-lock "BLOCK-CALLBACK-LOCK")
  "Protect modification to `*block-callback*'. ")

(let ((uuid 0))
  (defun %regist-block-callback (callback)
    "Regist Lisp CALLBACK function in `*block-callback*'.
Return UUID of CALLBACK.

Parameter:
+ CALLBACK: a function as ObjC Block callback in lisp side

Conditions:
+ If `*block-callback*' is full, throw error. "
    (declare (type function callback))
    (bt:with-lock-held (*block-callback-lock*)
      (loop :for i :below +block-callback-max+
            :for id := uuid :then (mod (1+ id) +block-callback-max+)
            :for slot := (aref *block-callback* id)
            :if (null slot) ;; empty slot
              :return (setf (aref *block-callback* id) callback
                            uuid id)
            :finally (error "`*block-callback*' is full... ")))))

(defun %remove-block-callback (uuid)
  (declare (type integer uuid))
  (bt:with-lock-held (*block-callback-lock*)
    (setf (aref *block-callback* uuid) nil)))

(defun %block-layout-call-uuid (block-layout &rest args)
  (declare (type foreign-pointer block-layout))
  (let* ((uuid (foreign-slot-value block-layout '(:struct block-layout) 'uuid))
         (call (aref *block-callback* uuid)))
    (unwind-protect
         (restart-case (apply (the function call) args)
           (ignore () :report "Ignore block callback"))
      (%remove-block-callback uuid))))


;;;; Block Type definition

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun gen-with-block (block-type)
    (symbol-concat "WITH-OBJC-" block-type "-BLOCK"))

  (defun gen-block-callback (block-type)
    (symbol-concat "%OBJC-" block-type "-BLOCK-CALLBACK"))

  (defun gen-with-block-expand (block-type ptr lambda-list body)
    `(with-foreign-object (,ptr '(:struct block-layout))
       (macrolet ((,block-type (,lambda-list &body block-body)
                    `(%fill-objc-block-layout
                      ,',ptr ',',(gen-block-callback block-type)
                      ,(cons 'lambda (cons (,@(cons 'list lambda-list))
                                           block-body)))))
         ,@body))))

(defun %fill-objc-block-layout (ptr cffi-callback fn)
  (declare (type foreign-pointer ptr)
           (type symbol cffi-callback)
           (type function fn))
  (macrolet ((blok (slot)
               `(foreign-slot-value ptr '(:struct block-layout) ',slot)))
    (setf (blok isa)        (foreign-symbol-pointer "_NSConcreteGlobalBlock")
          (blok flags)      (ash 1 29)
          (blok reserved)   0
          (blok invoke)     (get-callback cffi-callback)
          (blok descriptor) (block-layout-descriptor)
          (blok uuid)       (%regist-block-callback fn))
    ptr))

(defmacro define-objc-block (block-type lambda-list)
  "Define ObjC Block of type BLOCK-TYPE.

Syntax:

    (define-objc-block BLOCK-TYPE LAMBDA-LIST)

+ BLOCK-TYPE: symbol of block type name
+ LAMBDA-LIST: CFFI lambda list element as (VAR CFFI-TYPE)

Dev Note:
this will define:
+ CFFI callback with name %objc-<BLOCK-TYPE>-block-callback
+ a macro of name with-objc-<BLOCK-TYPE>-block,
  within macro, use (BLOCK-TYPE lambda-list &body body)
  as callback
"
  (alx:with-gensyms (block-layout)
    (let ((flattened (mapcar #'car lambda-list)))
      `(progn
         (defcallback ,(gen-block-callback block-type) :void
             ((,block-layout :pointer) ,@lambda-list)
           (%block-layout-call-uuid ,block-layout ,@flattened))
         (defmacro ,(gen-with-block block-type) ((block-ptr) &body body)
           (gen-with-block-expand ',block-type block-ptr ',flattened body))))))


;;;; ObjC typing

;;; (invoke obj sel :block (BLOCK-TYPE (lambda-list) ,@body))
;;; => (with-objc-<BLOCK-TYPE>-block (BLOCK-PTR)
;;;      (invoke obj sel :pointer (BLOCK-TYPE (lambda-list) ,@body)))

(define-objc-typing :block
  :alias :pointer
  :arg   (((cons (and (type symbol) block-type)
                 (cons (and (type list) lambda-list) body))
           (alx:with-gensyms (block-ptr)
             (values `(:pointer (,block-type ,lambda-list ,@body))
                     `(,(gen-with-block block-type) (,block-ptr)))))))

;;;; block.lisp ends here
