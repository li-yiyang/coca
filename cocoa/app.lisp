;;;; app.lisp

(in-package :coca.cocoa)


;;; Coca App

(defvar *app* nil
  "The foreign-pointer to NSApp. ")

;; `app' is patch to `define-objc-global-variable'
;; use function `app' everytime if need referring
;; NSApp foreign-pointer
(defun app ()
  "Return the foreign-pointer to NSApp. "
  (the foreign-pointer (or *app* (coca-app-run))))
(pushnew '*app* *global-objc-objects-variables*)

;; The `*app-lock*' and `*app-cvar*' is used when modifying NSApp
(defvar *app-lock* (bt:make-lock "APP-LOCK"))
(defvar *app-cvar* (bt:make-condition-variable :name "APP-CVAR"))

(defun coca-app-loop ()
  "Setup NSApp run loop.

+ NSApplicationActivationPolicyRegular
  Application is an ordinary app that appears in the Dock
  and may user interface.
"
  (unless (tmt:main-thread-p)
    (error "Coca App loop can only run in main thread. "))
  (with-fp-traps-masked
    (with-autorelease-pool ()
      (bt:with-lock-held (*app-lock*)
        (ensure-objc-initialized)
        (clrhash *objc-objects*)
        (setf *app* (invoke "NSApplication" "sharedApplication" :object)
              *app-main-queue* (foreign-symbol-pointer "_dispatch_main_q"))

        (unless (invoke *app* "isRunning" :bool)
          (invoke *app* "setActivationPolicy:" :unsigned-long 0) ; regular
          (invoke *app* "finishLaunching")
          (invoke *app* "activate"))

        (bt:condition-notify *app-cvar*))

      ;; For LispWorks or other lisp env with NSApp already running,
      ;; do not invoke NSApp running.
      (unless (invoke *app* "isRunning" :bool)
        (invoke *app* "run")))))

(defun coca-app-run ()
  "Start NSApp run loop.
Return foreign-pointer to NSApp. "
  (when (tmt:main-thread-p)
    (warn "`coca-app-run' should not run in main thread.
If you are not triggering it, it's an error.
Please create issue at McCLIM-Coca backend. "))
  (bt:with-lock-held (*app-lock*)
    (tmt:swap-main-thread #'coca-app-loop)
    (bt:condition-wait *app-cvar* *app-lock*))
  *app*)

(defun coca-app-terminate ()
  "Terminate NSApp. "
  (alx:when-let ((app *app*))
    (invoke app "terminate:" :object app)))


;;; Coca dispatch

;; The `*app-dispatch*', `*app-dispatch-error*', `*app-dispatch-result*'
;; is global status used in `dispatch-main'.
(declaim (type (or null function)  *app-dispatch*)
         (type (or null condition) *app-dispatch-error*)
         (type list                *app-dispatch-result*))

(defvar *app-dispatch*                   nil)
(defvar *app-dispatch-throw-to-toplevel* nil)
(defvar *app-dispatch-error*             nil)
(defvar *app-dispatch-result*            ())

(defvar *app-main-queue* nil
  "The dispatch_get_main_queue is internally a macro to symbol _dispatch_main_q

The foreign symbol pointer should be updated everytime when `coca-app-loop'
starts -- this ensures CFFI environment correct after image restarts. ")
(pushnew '*app-main-queue* *global-objc-objects-variables*)

(defun app-main-queue ()
  "Return the foreign-pointer of dispatch main queue. "
  (unless *app-main-queue*
    (coca-app-run))
  (the foreign-pointer *app-main-queue*))

(cffi:defcallback ns-app-dispatch :void ((context :pointer))
  (declare (ignore context))
  (alx:when-let ((dispatch *app-dispatch*))
    (with-autorelease-pool
      (if *app-dispatch-throw-to-toplevel*
          (handler-case
              (setf *app-dispatch-result*
                    (multiple-value-list
                     (funcall (the function dispatch))))
            (error (err) (setf *app-dispatch-error* err)))
          (restart-case
              (setf *app-dispatch-result*
                    (multiple-value-list
                     (funcall (the function dispatch))))
            (ignore ()
              :report "Ignore error and return `nil' to call thread. ")
            (ignore-with-return-value (return-value)
              :report "Ignore error and return with return value to call thread. "
              :interactive (lambda () (list (read)))
              return-value)
            (terminate ()
              :report "Terminate NSApp and quit lisp"
              (coca-app-terminate)))))))

(defun coca-app-dispatch (function &key throw-to-toplevel &allow-other-keys)
  "Send FUNCTION to run in NSApp main thread.
Return values of FUNCTION return values.

If FUNCTION throw error, the error would be captured and
throwed within calling thread. This may lost error calling
stack infomation.
"
  (declare (type (or symbol function) function))
  (let ((main-queue (app-main-queue)))
    (bt:with-lock-held (*app-lock*)
      (unwind-protect
           (progn
             (setf *app-dispatch* (etypecase function
                                    (symbol   (symbol-function function))
                                    (function function))
                   *app-dispatch-throw-to-toplevel* throw-to-toplevel
                   *app-dispatch-error*             nil
                   *app-dispatch-result*            nil)
             (foreign-funcall "dispatch_sync_f"
                              :pointer main-queue
                              :pointer (null-pointer)
                              :pointer (get-callback 'ns-app-dispatch))
             (when *app-dispatch-error*
               (error *app-dispatch-error*))
             (values-list *app-dispatch-result*))
        (setf *app-dispatch*        nil
              *app-dispatch-error*  nil
              *app-dispatch-result* nil)))))

;;;; app.lisp ends here
