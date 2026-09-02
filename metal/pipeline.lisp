;;;; pipeline.lisp --- Get the pipeline (kernel)

(in-package :coca.metal)

(defstruct (library (:constructor %make-library))
  (ptr (null-pointer)
   :type foreign-pointer
   :read-only t)
  (type :executable
   :type (member :executable :dynamic)
   :read-only t)
  (name ""
   :type string
   :read-only t)
  (device (default-device)
   :type device
   :read-only t)
  (functions ()
   :type list
   :read-only t))

(define-objc-enum (:mtl-math-mode :alias :long)
  "An indication of whether the compiler can perform
optimizations for floating-point arithmetic that may
violate the IEEE 754 standard."
  (:fast    2)
  (:relaxed 1)
  (:safe    0))

(define-objc-enum :mtl-library-optimization-level
  "The optimization options for the Metal compiler. "
  (:default 0)
  (:size    1))

(define-objc-enum (:mtl-library-type :alias :long)
  "A set of options for Metal library types. "
  (:executable 0)
  (:dynamic    1))

(defun make-library (device ptr)
  (declare (type device device)
           (type foreign-pointer ptr))
  (let* ((name (invoke ptr "installName" :ns-string))
         (type (invoke ptr "type" :mtl-library-type))
         (func (when (eq type :executable)
                 (mapcar #'ns-string-to-string
                         (invoke ptr "functionNames" :ns-array)))))
    (%make-library :ptr       ptr
                   :device    device
                   :name      name
                   :type      type
                   :functions func)))

(defun make-library-with-file (device pathname)
  "Load .metallib file at PATHNAME.
Return a foreign-pointer to MTLLibrary.

Parameters:
+ DEVICE:
+ PATHNAME: string or pathname to .metallib"
  (declare (type device device)
           (type (or string pathname) pathname))
  (assert (uiop:file-exists-p pathname))
  (with-foreign-object (err :pointer)
    (let ((lib (invoke (device-ptr device)
                       "newLibraryWithURL:error:"
                       :ns-url  pathname
                       :pointer err
                       :object)))
      (unless (null-pointer-p (mem-ref err :pointer))
        (error "Failed to open Metal library file ~A. ~%~A"
               pathname
               (description (mem-ref err :pointer))))
      (make-library device lib))))

(defun make-library-with-source (device source &rest options
                                 &key
                                   (math-mode    :safe)
                                   (library-type :executable)
                                   (optimize     :default)
                                   (name "MTLLibrary" name?)
                                   (libraries ()))
  "Create a MTLLibrary with SOURCE.
Return a foreign-pointer to MTLLibrary.

Parameters:
+ DEVICE: an instance of `device'
+ SOURCE: string of MSL codes
+ MATH-MODE:
  whether the compiler can perform optimizations for
  floating-point arithmetic that may violate the IEEE
  754 standard.
  + `:fast' make aggressive, potentially lossy assumptions
    about floating-point math.
  + `:relaxed' make aggressive, potentially lossy
    assumptions about floating-point math, while honoring Inf/NaN.
  + `:safe' disable unsafe floating-point optimizations
    by preventing the compiler from making any transformations
    that could affect the results.
+ LIBRARY-TYPE:
  + `:executable' library that can create pipeline state objects
  + `:dynamic' library can be dynamically linked from other libraries.
+ NAME: library name to use when installing the library
  only effective when LIBARY-TYPE is `:dynamic'
+ OPTIMIZE: optimization level
  + `:default' prioritizes runtime performance
  + `:size' prioritizes minimizing the size of its output binaries
    which may also reduce compile time
+ LIBRARIES: a list of dynamic libraries the Metal compiler
  links against
"
  (declare (type device device)
           (type string source)
           (type (member :safe :relaxed :fast) math-mode)
           (type (member :executable :dynamic) library-type)
           (type (member :default :size)       optimize)
           (type string name)
           (type list libraries))
  (with-autorelease-pool
    (let ((option (if options
                      (init (alloc "MTLCompileOptions"))
                      (null-pointer))))
      (when options
        (invoke option "setMathMode:" :mtl-math-mode math-mode)
        (invoke option "setLibraryType:" :mtl-library-type library-type)
        (invoke option
                "setOptimizationLevel:"
                :mtl-library-optimization-level optimize)
        (when (eq library-type :dynamic)
          (unless name?
            (error ":name is required when LIBRARY-TYPE is :dynamic"))
          (invoke option "setInstallName:" :ns-string name))
        (when libraries
          (assert (every (alx:rcurry #'typep 'foreign-pointer) libraries))
          (let ((libs (invoke "NSMutableArray" "array" :object)))
            (dolist (lib libraries)
              (invoke libs "addObject:" :object lib))
            (invoke option "setLibraries:" :object libs))))
      (unwind-protect
           (with-foreign-object (err :pointer)
             (let ((lib (invoke (device-ptr device)
                                "newLibraryWithSource:options:error:"
                                :ns-string source
                                :object    option
                                :pointer   err
                                :object)))
               (unless (null-pointer-p (mem-ref err :pointer))
                 (error "Failed to compile Metal library.~%~A"
                        (description (mem-ref err :pointer))))
               (make-library device lib)))
        (release option)))))

(defun library-compute-pipeline (library name)
  "Return foreign-pointer to MTLFunction. "
  (declare (type library library)
           (type string name))
  (unless (eq (library-type library) :executable)
    (error "~A is not a executable MTLLibrary. " library))
  (unless (find name (library-functions library) :test #'string=)
    (error "~A is not external MTLFunction for ~A. " name library))
  (with-foreign-object (err :pointer)
    (let* ((function (invoke (library-ptr library)
                             "newFunctionWithName:"
                             :ns-string name
                             :object))
           (pipeline (invoke (device-ptr (library-device library))
                             "newComputePipelineStateWithFunction:error:"
                             :object  function
                             :pointer err
                             :object)))
      (unless (null-pointer-p (mem-ref err :pointer))
        (error "Failed to get compute pipeline. ~%~A"
               (description (mem-ref err :pointer))))
      pipeline)))

(defstruct mtl-size
  (width  1 :type (unsigned-byte 64))
  (height 1 :type (unsigned-byte 64))
  (depth  1 :type (unsigned-byte 64)))

(defcstruct (%c-mtl-size :class c-mtl-size)
  (width  :unsigned-long)
  (height :unsigned-long)
  (depth  :unsigned-long))

(defun %execute-compute-pipeline
    (queue pipeline buffers grid-size group-size)
  (declare (type command-queue queue)
           (type foreign-pointer pipeline)
           (type list buffers)
           (type mtl-size grid-size group-size))
  (let* ((cmd (invoke (command-queue-ptr queue)
                      "commandBuffer"
                      :object))
         (enc (invoke cmd "computeCommandEncoder" :object))
         (idx -1))
    (invoke enc "setComputePipelineState:" :object pipeline)
    (dolist (buffer buffers)
      (declare (type foreign-pointer buffer))
      (invoke enc "setBuffer:offset:atIndex:"
              :object  buffer
              :ns-uint 0
              :ns-uint (incf idx)))
    (invoke enc "dispatchThreads:threadsPerThreadgroup:"
            :unsigned-long (mtl-size-width  grid-size)
            :unsigned-long (mtl-size-height grid-size)
            :unsigned-long (mtl-size-depth  grid-size)
            :unsigned-long (mtl-size-width  group-size)
            :unsigned-long (mtl-size-height group-size)
            :unsigned-long (mtl-size-depth  group-size))
    (invoke enc "endEncoding")
    (invoke cmd "commit")
    (invoke cmd "waitUntilCompleted")))

;;;; pipeline.lisp ends here
