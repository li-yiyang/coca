;;;; pipeline.lisp --- Get the pipeline (kernel)

(in-package :coca.metal)

(defstruct (library (:constructor %make-library))
  "Wrapper of MTLLibrary.

A collection of Metal shader functions.

Slot Values:
+ PTR: foreign-pointer to MTLLibrary
+ TYPE:
  + `:executable'
  + `:dynamic'
+ NAME: optional MTLLibrary name
+ DEVICE: the `device' the library is in
+ FUNCTIONS: a list of functions names
"
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
                         (invoke ptr "functionNames" :ns-array))))
         (lib  (%make-library :ptr       ptr
                              :device    device
                              :name      name
                              :type      type
                              :functions func)))
    (tg:finalize lib (lambda () (release ptr)))
    lib))

(defun make-library-with-file (device pathname)
  "Load .metallib file at PATHNAME.
Return a `library' instance to MTLLibrary.

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
Return a `library' instance to MTLLibrary.

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

(define-objc-mask :mtl-pipline-option
  "Options that determine how Metal prepares the pipeline. "
  (:none                        0)
  (:buffer-type-info            1)
  (:fail-on-binary-archive-miss 2)
  (:binding-info                4))

(define-objc-enum :mtl-binding-type
  "Types of MTLBinding. "
  (:buffer                           0)
  (:threadgroup-memory               1)
  (:texture                          2)
  (:sampler                          3)
  (:imageblock                       4)
  (:imageblock-data                  5)
  (:instance-acceleration-structure	 6)
  (:primitive-acceleration-structure 7)
  (:intersection-function-table      8)
  (:visible-function-table           9)
  (:object-payload                   10)
  (:tensor                           11))

(defun command-queue-make-command-buffer-encoder (queue type)
  (declare (type command-queue queue)
           (type (member :render :compute :blit :parallel-render)
                 type))
  (let* ((cmd (invoke (command-queue-ptr queue)
                      "commandBuffer"
                      :object))
         ;; TODO: maybe fix this
         (enc (ecase type
                (:render
                 (invoke cmd "renderCommandEncoder"  :object))
                (:compute
                 (invoke cmd "computeCommandEncoder" :object))
                (:blit
                 (invoke cmd "blitCommandEncoder"    :object))
                (:parallel-render
                 (invoke cmd "parallelRenderCommandEncoder" :object)))))
    (values cmd enc)))

(defun command-buffer-commit-and-wait (command-buffer encoder)
  (declare (type foreign-pointer command-buffer encoder))
  (invoke encoder "endEncoding")
  (invoke command-buffer "commit")
  (invoke command-buffer "waitUntilCompleted"))

(defun encoder-set-compute-pipeline-state (encoder pipeline)
  (declare (type foreign-pointer encoder pipeline))
  (invoke encoder "setComputePipelineState:" :object pipeline))

(defun encoder-set-buffer (encoder buffer &key (offset 0) (index 0))
  (declare (type foreign-pointer encoder buffer)
           (type fixnum offset index))
  (invoke encoder "setBuffer:offset:atIndex:"
          :object  buffer
          :ns-uint offset
          :ns-uint index))

(defun encoder-set-grid-size-group-size (encoder grid-size group-size)
  (declare (type foreign-pointer encoder)
           (type mtl-size grid-size group-size))
  (invoke encoder "dispatchThreads:threadsPerThreadgroup:"
          :mtl-size grid-size
          :mtl-size group-size))

(defmacro with-command-buffer
    (command-queue
     (type encoder &optional (command-buffer (gensym "COMMAND-BUFFER")))
     &body body)
  "Create and encode the command buffer.

Syntax:

    (with-command-buffer COMMAND-QUEUE
        (TYPE ENCODER &optional COMMAND-BUFFER)
      &body
      [:return ...])

+ TYPE:
  + `:render'
  + `:compute'
  + `:blit'
  + `:parallel-render'
+ ENCODER: variable binded with MTLCommandEncoder
+ COMMAND-BUFFER: variable binded with MTLCommandBuffer
+ RETURN:
"
  (alx:with-gensyms (successp result)
    (let* ((return-pos (position :return body))
           (returns    (when return-pos
                         (subseq body return-pos)))
           (body       (subseq body 0 return-pos)))
      `(let ((,successp t)
             ,result)
         (multiple-value-bind (,command-buffer ,encoder)
             (command-queue-make-command-buffer-encoder ,command-queue ,type)
           (unwind-protect
                (handler-case
                    (progn
                      ,@body
                      (command-buffer-commit-and-wait ,command-buffer ,encoder))
                  (error (err)
                    (setf ,successp nil)
                    (error err)))
             (when ,successp
               (setf ,result (progn ,@returns)))
             (release ,encoder)
             (release ,command-buffer)))
         (when ,successp
           ,result)))))

(defun %library-compute-pipeline (library name)
  "Return values are foreign-pointer to MTLFunction, a list of (VAR INDEX). "
  (declare (type library library)
           (type string name))
  (unless (eq (library-type library) :executable)
    (error "~A is not a executable MTLLibrary. " library))
  (unless (find name (library-functions library) :test #'string=)
    (error "~A is not external MTLFunction for ~A. " name library))
  (with-autorelease-pool
    (with-foreign-objects ((err        :pointer)
                           (reflection :pointer))
      (let* ((function (invoke (library-ptr library)
                               "newFunctionWithName:"
                               :ns-string name
                               :object))
             (pipeline (invoke (device-ptr (library-device library))
                               "newComputePipelineStateWithFunction:options:reflection:error:"
                               :object             function
                               :mtl-pipline-option :binding-info
                               :pointer            reflection
                               :pointer            err
                               :object)))
        (unless (null-pointer-p (mem-ref err :pointer))
          (error "Failed to get compute pipeline. ~%~A"
                 (description (mem-ref err :pointer))))
        (values pipeline
                (loop :for binding :in (invoke (mem-ref reflection :pointer)
                                               "bindings"
                                               :ns-array)
                      :for type := (invoke binding "type" :mtl-binding-type)
                      :if (eq type :buffer)
                        :collect (let ((name (invoke binding "name"  :ns-string))
                                       (idx  (invoke binding "index" :ns-uint)))
                                   (list (intern (string-upcase name))
                                         idx))))))))

(defun library-compute-pipeline (library name &key debug)
  "Get the callable function in LIBRARY of NAME.
Return a function with lambda list like

    (lambda (BUFFER... &key COMMAND-QUEUE GRID-SIZE GROUP-SIZE))

Parameters:
+ LIBRARY: a `library'
+ NAME: a string for the library compute MTLFunction
+ DEBUG: if non-nil, print the function lambda expression in `*debug-io*'
"
  (declare (type library library)
           (type string  name))
  (multiple-value-bind (pipeline args)
      (%library-compute-pipeline library name)
    (let* ((offset (loop :for (name) :in args
                         :collect (intern (concatenate 'string
                                                       (string name)
                                                       "-OFFSET"))))
           (expr `(lambda (,@(mapcar #'first args)
                           &key
                             (command-queue (default-command-queue))
                             (grid-size     (mtl-size 8 1 1))
                             (group-size    (mtl-size 8 1 1))
                             ,@(loop :for off :in offset
                                     :collect `(,off 0)))
                    (declare (type foreign-pointer ,@(mapcar #'first args))
                             (type command-queue command-queue)
                             (type (or mtl-size (vector integer 3))
                                   grid-size group-size)
                             (type (unsigned-byte 64) ,@offset))
                    (with-command-buffer command-queue (:compute encoder)
                      (encoder-set-compute-pipeline-state encoder ,pipeline)
                      ,@(loop :for (name idx) :in args
                              :for off :in offset
                              :collect `(encoder-set-buffer encoder ,name
                                                            :index  ,idx
                                                            :offset ,off))
                      (encoder-set-grid-size-group-size encoder grid-size group-size))))
           (fn   (eval expr)))
      (when debug (print expr *debug-io*))
      (tg:finalize fn (lambda () (release pipeline)))
      (the function fn))))

;;;; pipeline.lisp ends here
