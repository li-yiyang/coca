;;;; cursor.lisp

(in-package :coca.cocoa)

(defclass cursor (objc-obj)
  ((objc-class
    :initform (coerce-to-objc-class "NSCursor")))
  (:documentation
   "Wraps NSCursor. "))

(defmethod initialize-instance :after ((cursor cursor) &key)
  (regist-objc-obj cursor))

(defgeneric find-cursor (style)
  (:documentation
   "Find `cursor' of STYLE.
Return `cursor' object. ")
  (:method ((cursor cursor)) cursor)
  (:method ((default null))
    (declare (ignore default))
    (%arrow-cursor)))

(macrolet ((defcursor (keyword sel doc)
             (let ((fn  (intern (str:concat "%" (string keyword) "-CURSOR")))
                   (sym (intern (str:concat "*" (string keyword) "-CURSOR*"))))
               `(progn
                  (define-objc-global-variable ,fn
                      (make-instance 'cursor
                                     :pointer (invoke "NSCursor" ,sel :object))
                    ,doc)
                  (defmethod find-cursor ((style (eql ,keyword)))
                    (,fn))
                  (define-symbol-macro ,sym (,fn))))))

  (defcursor :arrow "arrowCursor"
    "The arrow cursor. ")

  (defcursor :i-beam-cursor "IBeamCursor"
    "The I-beam cursor for indicating insertion points. ")

  (defcursor :pointing-hand "pointingHandCursor"
    "The pointing-hand cursor. ")

  (defcursor :closed-hand "closedHandCursor"
    "The closed-hand cursor. ")

  (defcursor :open-hand "openHandCursor"
    "The open-hand cursor. ")

  (defcursor :cross-hair "crosshairCursor"
    "The corss-hair cursor. "))

(defun current-cursor ()
  (let ((cursor* (invoke "NSCursor" "currentCursor" :object)))
    (the cursor
      (or (find-objc-obj cursor*)
          (make-instance 'cursor :pointer cursor*)))))

(defun (setf current-cursor) (cursor)
  (ignore-errors
   (let ((cursor (find-cursor cursor)))
     (with-ptr cursor
       (dispatch-main ()
         (invoke ptr "set")))))
  (current-cursor))

(defun set-cursor (cursor)
  "Set current cursor to CURSOR.
If the CURSOR cannot be found via `find-cursor',
this does a no-op. "
  (setf (current-cursor) cursor))

(define-symbol-macro *current-cursor* (current-cursor))

;;;; cursor.lisp ends here
