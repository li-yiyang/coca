;;;; block.lisp ---- Implementation of ObjC block bindings

(in-package :coca.objc)


;;;; Block Descriptor

;; Dev Note:
;; see https://clang.llvm.org/docs/Block-ABI-Apple.html
;; for Block Layout definition
;; or use clang -rewrite-objc balabala.m to modify the
;; `block-layout' and `block-descriptor'

(defcstruct block-descriptor
  (reserved  :unsigned-long)
  (size      :unsigned-long)
  ;; optional, since missing these fields didn't
  ;; affects the results, I'll just leave them
  ;; commented. If future usage encounters some
  ;; real issues, uncomment them and do so more
  ;; works. 
  ;; (copy      :pointer)
  ;; (dispose   :pointer)
  ;; (signature :string)
  )

(defcstruct block-layout
  (isa        :pointer)
  (flags      :int)
  (reserved   :int)
  ;; Invoke is the C callback function pointer
  (invoke     :pointer) 
  (descriptor :pointer))

(define-objc-global-variable block-descriptor
    (let ((desc (foreign-alloc '(:struct block-descriptor))))
      (macrolet ((slot (name)
                   `(foreign-slot-value desc
                                        '(:struct block-descriptor)
                                        ',name)))
        (setf (slot reserved) 0
              (slot size)     (foreign-type-size '(:struct block-layout)))
        desc))
  "The shared ObjC block descriptor. ")

(define-objc-global-variable ns-concrete-global-block
    (foreign-symbol-pointer "_NSConcreteGlobalBlock")
  "The isa for `block-layout'. ")


;;;; Block Layout

(defstruct (objc-block (:constructor make-objc-block (ptr callback)))
  (ptr      (null-pointer) :type foreign-pointer)
  (callback (constantly 0) :type (or symbol function)))

(defvar *objc-blocks* (make-hash-table)
  "All the registed ObjC blocks. 

Key: foreign pointer address 
Val: `objc-block' instance
")

(flet ((make-objc-block-layout ()
         (let ((blk (foreign-alloc '(:struct block-layout))))
           (macrolet ((slot (name)
                        `(foreign-slot-value blk
                                             '(:struct block-layout)
                                             ',name)))
             (setf (slot isa)        (ns-concrete-global-block)
                   (slot flags)      #.(ash 1 29)
                   (slot reserved)   0
                   (slot invoke)     (get-callback c-callback)
                   (slot descriptor) (block-descriptor)))
           blk)))
  
  (defun alloc-objc-block (c-callback lisp-callback)
    (let ((blk (make-objc-block-layout)))
      (setf (gethash (pointer-address blk) *objc-blocks*)
            (make-objc-block blk lisp-callback))))

  (defun ensure-objc-block-initialized ()
    "Dev Note: this should only be invoked by `ensure-objc-initialized'. "
    (let ((objc-blocks (alx:hash-table-values *objc-blocks*)))
      (clrhash *objc-blocks*)
      (dolist (objc-block objc-blocks)
        (let ((blk (make-objc-block-layout)))
          (setf (objc-block-ptr objc-block)                   blk
                (gethash (pointer-address blk) *objc-blocks*) objc-block)))))

  (pushnew 'ensure-objc-block-initialized *on-objc-initialization*))

(defun release-objc-block (objc-block)
  (declare (type objc-block objc-block))
  (let ((ptr (objc-block-ptr objc-block)))
    (remhash (pointer-address ptr) *objc-blocks*)
    (foreign-free ptr)))

(defun call-objc-block (objc-block* &rest args)
  (declare (type foreign-pointer objc-block*))
  (alx:when-let* ((objc-block (gethash (pointer-address objc-block*)
                                       *objc-blocks*))
                  (callback   (objc-block-callback objc-block)))
    (apply callback args)))

(defmacro with-objc-block ((objc-block c-callback lisp-callback) &body body)
  `(let ((,objc-block (alloc-objc-block ,c-callback ,lisp-callback)))
     (unwind-protect (progn ,@body)
       (release-objc-block ,objc-block))))


;;;; Block Type definition

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun gen-block-callback (block-type)
    (symbol-concat "%OBJC-" block-type "-BLOCK-CALLBACK")))

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
    (let ((c-callback (gen-block-callback block-type)))
      `(defcallback ,c-callback :void ((,block-layout :pointer) ,@lambda-list)
         (call-objc-block ,block-layout ,@(mapcar #'car lambda-list))))))


;;;; ObjC typing

;;; (invoke obj sel :block (BLOCK-TYPE (lambda-list) ,@body))
;;; (invoke obj sel :block (BLOCK-TYPE #'function))
;;; (invoke obj sel :block (BLOCK-TYPE 'function))
;;; => (with-objc-<BLOCK-TYPE>-block (OBJC-BLOCK FUNC)
;;;      (invoke obj sel :pointer (objc-block-ptr OBJC-BLOCK)))

(define-objc-typing :block
  :alias :pointer
  :arg   (((list* (and (type symbol) block-type)
                  (and (type list)   lambda-list)
                  body)
           (alx:with-gensyms (objc-block)
             (values `(:pointer (objc-block-ptr ,objc-block))
                     `(with-objc-block (,objc-block
                                        ,(gen-block-callback block-type)
                                        (lambda ,lambda-list ,@body))))))
          ((list (and (type symbol) block-type)
                 (list 'function (and (type symbol) func-name)))
           (alx:with-gensyms (objc-block)
             (values `(:pointer (objc-block-ptr ,objc-block))
                     `(with-objc-block (,objc-block
                                        ,(gen-block-callback block-type)
                                        #',func-name)))))
          ((list (and (type symbol) block-type) func-expr)
           (alx:with-gensyms (objc-block)
             (values `(:pointer (objc-block-ptr ,objc-block))
                     `(with-objc-block (,objc-block
                                        ,(gen-block-callback block-type)
                                        ,func-expr)))))))

;;;; block.lisp ends here
