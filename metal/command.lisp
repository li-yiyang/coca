;;;; command.lisp --- Work submission

(in-package :coca.metal)

;; To execute commands on the GPU, follow this process:
;;
;; + Create a command buffer from a command queue.
;;
;; + Create a command encoder using the command buffer.
;;
;; + Add the commands to the command buffer using the
;;   command encoder.
;;
;; + Get callbacks when the GPU schedules and executes the
;;   commands by setting completion handlers.
;;
;; + Commit the command buffer.
;;
;; If you’re performing animation as part of a rendering loop,
;; do this for each frame of the animation. You also follow this
;; process to execute one-off image processing, or machine
;; learning tasks.

(defstruct (command-queue (:constructor %make-command-queue))
  "Wrapper of MTLCommandQueue.

An instance you use to create, submit, and schedule
command buffers to a specific GPU device to run the
commands within those buffers.

Slot Values:
+ PTR: foreign-pointer to MTLCommandQueue
+ DEVICE: the `device' command queue is attached;
+ MAX-BUFFER: maximum number of uncompleted command buffers
  the queue can allow.
"
  (ptr        (null-pointer)   :type foreign-pointer :read-only t)
  (device     (default-device) :type device          :read-only t)
  (max-buffer 64               :type fixnum          :read-only t))

(defun make-command-queue (&optional (device (default-device))
                           &key (max-buffer 64)
                           &allow-other-keys)
  "Create a `command-queue' to submit rendering and computation.
Return a `command-queue' instance.

Parameters:
+ DEVICE: where the `command-queue' is attached
+ MAX-BUFFER: the maximum number of uncompleted command buffers
  the `command-queue' can allow. (default 64)
"
  (declare (type device device)
           (type fixnum max-buffer))
  (assert (<= 1 max-buffer))
  (let* ((ptr   (invoke (device-ptr device)
                        "newCommandQueueWithMaxCommandBufferCount:"
                        :ns-uint max-buffer
                        :object))
         (queue (if (null-pointer-p ptr)
                    (error "Failed to create a `command-queue'. ")
                    (%make-command-queue :ptr        ptr
                                         :device     device
                                         :max-buffer max-buffer))))
    (tg:finalize queue (lambda () (release ptr)))
    queue))

(define-objc-global-variable default-command-queue
    (make-command-queue)
  "Default `command-queue' to create, submit, and schedule
command buffers to `default-device' to run the commands
within those buffers. ")

;;;; command.lisp ends here
