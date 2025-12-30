;;;; asdf.lisp --- Extend CFFI-GROVEL with ObjC-File

(in-package :cffi-grovel)

;;; Dev Note:
;; this is basically a copy of `cffi::wrapper-file'

(defclass objc-file (process-op-input cc-flags-mixin)
  ((soname :initform nil :initarg :soname :accessor soname-of))
  (:default-initargs
   :generated-lisp-file-type "processed-objc-file")
  (:documentation
   "This ASDF component represents an input file that is processed
by `cffi::process-objc-file'. This generates a foreign ObjC library
and matching CFFI bindings that are subsequently compiled and loaded. "))

(defmethod output-files ((op process-op) (c objc-file))
  (let* ((input-file (first (input-files op c)))
         (output-file (make-pathname :type (generated-lisp-file-type c)
                                     :defaults input-file))
         (c-file (make-c-file-name output-file "__wrapper"))
         (o-file (make-o-file-name output-file "__wrapper"))
         (lib-soname (wrapper-soname c)))
    (list output-file
          (make-so-file-name (make-soname lib-soname output-file))
          c-file
          o-file)))

(defmethod perform ((op process-op) (c objc-file))
  (let* ((output-file (first (output-files op c)))
         (input-file  (first (input-files  op c)))
         (tmp-file    (process-objc-file
                       input-file
                       :output-defaults output-file
                       :lib-soname (wrapper-soname c))))
      (unwind-protect
           (alexandria:copy-file tmp-file output-file :if-to-exists :supersede)
        (delete-file tmp-file))))

(setf (find-class 'asdf::objc-file) (find-class 'objc-file))

;;;; asdf.lisp ends here
