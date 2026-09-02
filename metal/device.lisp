;;;; device.lisp --- Device

(in-package :coca.metal)

(defstruct (device (:constructor %make-device))
  (ptr          (null-pointer) :type foreign-pointer    :read-only t)
  (id           0              :type (unsigned-byte 64) :read-only t)
  (name         ""             :type string             :read-only t)
  (architecture ""             :type string             :read-only t))

(defun make-device (pointer)
  "Make Metal `device' from MTLDevice POINTER.
Return a `device' object.

Parameter:
+ POINTER: `foreign-pointer' to MTLDevice

  NOTE: make sure it is a MTLDevice pointer
"
  (declare (type foreign-pointer pointer))
  (flet ((name (objc)
           (invoke objc "name" :ns-string)))
    (let ((id   (invoke pointer "registryID" :uint64))
          (name (name pointer))
          (arch (name (invoke pointer "architecture" :object))))
      (%make-device :ptr          pointer
                    :id           id
                    :name         name
                    :architecture arch))))

(define-objc-global-variable default-device
    (with-fp-traps-masked
      (let ((ptr (foreign-funcall
                  "MTLCreateSystemDefaultDevice"
                  :pointer)))
        (if (null-pointer-p ptr)
            (error "Metal is not avaliable on this machine. ")
            (make-device ptr))))
  "The `device' instance Metal selects as the default. ")

(define-objc-enum :mtl-gpu-family
  "Represents the functionality for families of GPUs.

Check whether a GPU supports the features of a specific family
by calling the `device-supports-family' of `device' instance."
  (:metal4 5002)
  (:metal3 5001)
  (:apple9 1009)
  (:apple8 1008)
  (:apple7 1007))

(defun device-supports-family-p (device family)
  "Test if DEVICE supports MTLGPUFamily FAMILY.

Parameters:
+ DEVICE: a `device'
+ FAMILY: one of

  Represents the Metal features
  + `:metal4'
  + `:metal3'

  Represents the Apple family X GPU features
  + `:apple9' correspond to the M3, and M4 GPUs.
  + `:apple8' correspond to the M2 GPUs.
  + `:apple7' correspond to the M1 GPUs.
"
  (declare (type device device)
           (type (member :metal4 :metal3 :apple9 :apple8 :apple7)
                 family))
  (invoke (device-ptr device) "supportsFamily:"
          :mtl-gpu-family family
          :bool))

;; Checking a GPU device’s memory

(defun device-current-allocated-size (device)
  "The total amount of memory DEVICE is using.
Return size in bytes.

Parameter:
+ DEVICE: the `device' instance"
  (declare (type device device))
  (invoke (device-ptr device)
          "currentAllocatedSize"
          :ns-uint))

;;;; device.lisp ends here
