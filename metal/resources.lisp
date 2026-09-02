;;;; resources.lisp --- Resource Creation

(in-package :coca.metal)

(define-objc-mask :mtl-resource-options
  "The behavior of a MTLBuffer resources. "
  (:cpu-cache-default         #x000)
  (:cpu-cache-write-combined  #x001)
  (:hazard-tracking-default   #x000)
  (:hazard-tracking-tracked   #x200)
  (:hazard-tracking-untracked #x100)
  (:shared                    #x000)
  (:private                   #x020)
  (:memoryless                #x030))

(deftype msl-dtype ()
  '(member
    :uint8 :uint16 :uint32 :uint64
    :int8  :int16  :int32  :int64
    :bool :float :size

    ;; TODO
    ;; :half :half2 :half4
    ;; :float2 :float3 :float4
    ;; :int3 :uint3
    ;; :packed-float3
    ))

(defun msl-dtype-size (type)
  (declare (type msl-dtype type))
  (ecase type
    ((:uint8 :uint16 :uint32 :uint64
      :int8  :int16  :int32  :int64
      :float :size :bool)
     (foreign-type-size type))))

(defun make-buffer (type data-or-length
                    &key
                      (device  (default-device))
                      (no-copy nil)
                      (options :shared)
                    &allow-other-keys)
  "Make a MTLBuffer.
Return a foreign-pointer to MTLBuffer.

Parameters:
+ TYPE: MTLBuffer data type (`msl-dtype')
  + `:uint8',  `:int8'
  + `:uint16', `:int16'
  + `:uint32', `:int32'
  + `:uint64', `:int64' (MSL 2.2+)
  + `:bool'
  + `:float' (single-float)
  + `:size'

  TODO:
  + `:half', `:half2', `:half4'
  + `:float2', `:float3', `:float4'
  + `:int3', `:uint3'
  + `:packed-float3'
+ DATA-OR-LENGTH
  + LENGTH: length of allocated MTLBuffer size
  + DATA:
    + array (row major indexed) copied to MTLBuffer
    + (cons LENGTH foreign-pointer-to-array)
      or (cons foreign-pointer-to-array LENGTH)
+ NO-COPY: if non-nil, the data is manimuplate with no copy

  NOTE: only effective when DATA-OR-LENGTH is like (cons ptr len)
+ DEVICE:
  the `device' to allocate the MTLBuffer
  Note: invoke `release' after finish using MTLBuffer
+ OPTIONS:
  can be a keyword below or a list of keywords below:

  Storage Mode:
  + `:shared'
  + `:private'
  + `:memoryless'

  CPU Cache Mode:
  + `:cpu-cache-default'
  + `:cpu-cache-write-combined'

  Hazard Tracking Mode:
  + `:hazard-tracking-default'
  + `:hazard-tracking-tracked'
  + `:hazard-tracking-untracked'
"
  (declare (type msl-dtype type)
           (type (or (integer 0)
                     array
                     (cons foreign-pointer (integer 0))
                     (cons (integer 0) foreign-pointer))
                 data-or-length)
           (type device device)
           (type (or keyword list) options))
  (flet ((allocate (len)
           (invoke (device-ptr device)
                   "newBufferWithLength:options:"
                   :ns-uint (* len (msl-dtype-size type))
                   :mtl-resource-options options
                   :object))
         (convert (data len)
           (if no-copy
               (invoke (device-ptr device)
                       "newBufferWithBytesNoCopy:length:options:deallocator:"
                       :pointer data
                       :ns-uint (* len (msl-dtype-size type))
                       :mtl-resource-options options
                       :pointer (null-pointer)
                       :object)
               (invoke (device-ptr device)
                       "newBufferWithBytes:length:options:"
                       :pointer data
                       :ns-uint len
                       :mtl-resource-options options
                       :object))))
    (the foreign-pointer
      (m:match data-or-length
        ((and (type (integer 0)) length)
         (allocate length))
        ((and (type array) array)
         (let* ((length  (reduce #'* (array-dimensions array)))
                (buffer  (allocate length))
                (content (invoke buffer "contents" :pointer)))
           (ecase type
             ((:uint8 :uint16 :uint32 :uint64
               :int8  :int16  :int32  :int64
               :float :size :bool)
              (lisp-array-to-foreign
               array content
               `(:array ,type ,@(array-dimensions array)))))
           buffer))
        ((cons (and (type (integer 0)) length)
               (and (type foreign-pointer) data))
         (convert data length))
        ((cons (and (type foreign-pointer) data)
               (and (type (integer 0)) length))
         (convert data length))
        (_
         (error "Invalid DATA-OR-LENGTH ~S. " data-or-length))))))

;;;; resources.lisp ends here
