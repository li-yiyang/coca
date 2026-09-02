;;;; typing.lisp

(in-package :coca.metal)

(defstruct mtl-size
  (width  1 :type (unsigned-byte 64))
  (height 1 :type (unsigned-byte 64))
  (depth  1 :type (unsigned-byte 64)))

(defcstruct (%c-mtl-size :class c-mtl-size)
  (width  :unsigned-long)
  (height :unsigned-long)
  (depth  :unsigned-long))

(defmethod cffi:translate-from-foreign (ptr (type c-mtl-size))
  (with-foreign-slots ((width height depth)
                       ptr
                       (:struct %c-mtl-size))
    (values width height depth)))

(defmethod expand-from-foreign (ptr (type c-mtl-size))
  `(with-foreign-slots ((width height depth)
                        ,ptr
                        (:struct %c-mtl-size))
     (values width height depth)))

(defmethod translate-into-foreign-memory
    ((size mtl-size) (type c-mtl-size) ptr)
  (with-foreign-slots ((width height depth)
                       ptr
                       (:struct %c-mtl-size))
    (setf width  (mtl-size-width  size)
          height (mtl-size-height size)
          depth  (mtl-size-depth  size))))

(defmethod translate-into-foreign-memory
    ((size vector) (type c-mtl-size) ptr)
  (with-foreign-slots ((width height depth)
                       ptr
                       (:struct %c-mtl-size))
    (setf width  (aref size 0)
          height (aref size 1)
          depth  (aref size 2))))

(define-objc-typing :mtl-size
  :result ((:struct %c-mtl-size))
  :arg    (((vector width height depth)
            `((:struct %c-mtl-size) #(,width ,height ,depth)))
           ((list   width height depth)
            `((:struct %c-mtl-size) #(,width ,height ,depth)))
           (size
            `((:struct %c-mtl-size) (the mtl-size ,size)))))

;;;; typing.lisp ends here
