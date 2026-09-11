;;;; block.lisp ---- Implementation of ObjC block bindings

(in-package :coca.objc)


;;;; ObjC Block in lisp side

(defvar *objc-blocks* (make-hash-table)
  "All the registed ObjC blocks. 

Key: foreign pointer address 
Val: `objc-block' instance
")

(defstruct (objc-block (:constructor make-objc-block
                           (ptr c-callback callback)))
  "Representation of ObjC Block in lisp side. 

Slot Values:
+ PTR: foreign-pointer to `block-layout' struct 
+ C-CALLBACK: the C callback symbol 
+ CALLBACK: lisp callback function or symbol

Dev Note: 
+ use `alloc-objc-block' (internally) 
+ use `release-objc-block' to free
+ use `find-objc-block' to look up block with foreign-pointer
"
  (ptr        (null-pointer) :type foreign-pointer)
  (c-callback nil            :type symbol)
  (callback   (constantly 0) :type (or symbol function)))

(declaim (inline find-objc-block))
(defun find-objc-block (blk*)
  "Find the `objc-block' of BLK*, or return `nil'. "
  (declare (type foreign-pointer blk*))
  (gethash (pointer-address blk*) *objc-blocks*))

(flet ((make-objc-block-layout (c-callback)
         (let ((blk (foreign-alloc '(:struct block-layout))))
           (macrolet ((slot (name)
                        `(foreign-slot-value blk
                                             '(:struct block-layout)
                                             ',name)))
             (setf (slot isa)        (foreign-symbol-pointer
                                      "_NSConcreteStackBlock")
                   (slot flags)      #.(logior (ash 1 29)  ; has descriptor
                                               (ash 1 25)) ; has copy dispose
                   (slot reserved)   0
                   (slot invoke)     (get-callback c-callback)
                   (slot descriptor) (block-descriptor)))
           blk)))
  
  (defun alloc-objc-block (c-callback lisp-callback)
    "Allocate `objc-block' instance with C-CALLBACK and LISP-CALLBACK. 
Return `objc-block' instance.

Parameters: 
+ C-CALLBACK: symbol of C callback function 
+ LISP-CALLBACK: symbol or function as callback function 
"
    (declare (type symbol c-callback)
             (type (or symbol function) lisp-callback))
    (let ((blk (make-objc-block-layout c-callback)))
      (setf (gethash (pointer-address blk) *objc-blocks*)
            (make-objc-block blk c-callback lisp-callback))))

  (defun ensure-objc-block-initialized ()
    "Dev Note: this should only be invoked by `ensure-objc-initialized'. "
    (let ((objc-blocks (alx:hash-table-values *objc-blocks*)))
      (clrhash *objc-blocks*)
      ;; Note: no need to foreign-free previous blk,
      ;; since they are invalid after image dump. 
      (dolist (objc-block objc-blocks)
        (let ((blk (make-objc-block-layout
                    (objc-block-c-callback objc-block))))
          (setf (objc-block-ptr objc-block)                   blk
                (gethash (pointer-address blk) *objc-blocks*) objc-block)))))

  (pushnew 'ensure-objc-block-initialized *on-objc-initialization*))

(defun release-objc-block (objc-block)
  "Free OBJC-BLOCK foreign ObjC Block struct and unregist it. 
Return OBJC-BLOCK itself. "
  (declare (type objc-block objc-block))
  (let ((ptr (objc-block-ptr objc-block)))
    (remhash (pointer-address ptr) *objc-blocks*)
    (foreign-free ptr)
    objc-block))

(defun call-objc-block (objc-block* &rest args)
  (declare (type foreign-pointer objc-block*))
  (alx:when-let* ((objc-block (find-objc-block objc-block*))
                  (callback   (objc-block-callback objc-block)))
    (apply callback args)))

(defmacro with-objc-block ((objc-block c-callback lisp-callback) &body body)
  `(let ((,objc-block (alloc-objc-block ,c-callback ,lisp-callback)))
     (unwind-protect (progn ,@body)
       (release-objc-block ,objc-block))))


;;;; Block Descriptor

;; Dev Note:
;; see https://clang.llvm.org/docs/Block-ABI-Apple.html
;; for Block Layout definition
;; or use clang -rewrite-objc balabala.m to modify the
;; `block-layout' and `block-descriptor'

(defcstruct block-descriptor
    (reserved  :unsigned-long)
  (size      :unsigned-long)
  (copy      :pointer)
  (dispose   :pointer))

(defcstruct block-layout
  (isa        :pointer)
  (flags      :int)
  (reserved   :int)
  ;; Invoke is the C callback function pointer
  (invoke     :pointer) 
  (descriptor :pointer))

(defcallback copy-block-layout :void ((dst :pointer)
                                      (src :pointer))
  (alx:when-let ((objc-block (find-objc-block src)))
    (setf (gethash (pointer-address dst) *objc-blocks*) objc-block)))

(defcallback dispose-block-layout :void ((blk :pointer))
  ;; the dispose should not free blk
  (remhash (pointer-address blk) *objc-blocks*))

(define-objc-global-variable block-descriptor
    (let ((desc (foreign-alloc '(:struct block-descriptor))))
      (macrolet ((slot (name)
                   `(foreign-slot-value desc
                                        '(:struct block-descriptor)
                                        ',name)))
        (setf (slot reserved) 0
              (slot size)     (foreign-type-size '(:struct block-layout))
              (slot copy)     (get-callback 'copy-block-layout)
              (slot dispose)  (get-callback 'dispose-block-layout))
        desc))
  "The shared ObjC block descriptor. ")


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
;;; (invoke obj sel :block OBJC-BLOCK)
;;; => (with-objc-<BLOCK-TYPE>-block (OBJC-BLOCK FUNC)
;;;      (invoke obj sel :pointer (objc-block-ptr OBJC-BLOCK)))

(define-objc-typing :block
  :alias :pointer
  :arg   (((list (and (type symbol) block-type)
                 (list 'function (and (type symbol) func-name)))
           (alx:with-gensyms (objc-block)
             (values `(:pointer (objc-block-ptr ,objc-block))
                     `(with-objc-block (,objc-block
                                        ',(gen-block-callback block-type)
                                        #',func-name)))))
          ((list* (and (type symbol) block-type)
                  (and (type list)   lambda-list)
                  body)
           (alx:with-gensyms (objc-block)
             (values `(:pointer (objc-block-ptr ,objc-block))
                     `(with-objc-block (,objc-block
                                        ',(gen-block-callback block-type)
                                        (lambda ,lambda-list ,@body))))))
          ((list (and (type symbol) block-type) func-expr)
           (alx:with-gensyms (objc-block)
             (values `(:pointer (objc-block-ptr ,objc-block))
                     `(with-objc-block (,objc-block
                                        ',(gen-block-callback block-type)
                                        ,func-expr)))))
          (objc-block
           `(:pointer (objc-block-ptr ,objc-block)))))

;;;; block.lisp ends here
