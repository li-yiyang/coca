;;;; utils.lisp --- Utils mixin for building NSViews-like widgets

(in-package :coca.appkit)

(define-objc-enum (:ns-control-state :alias :long)
  "Whether a control is on, off, or in a mixed state. "
  (:on    1)
  (:off   0)
  (:mixed -1))

(defclass state-mixin () ()
  (:documentation
   "Mixin class for obj support state method. "))

(defgeneric state (state-mixin)
  (:method ((obj state-mixin))
    (with-ptr obj ptr
      (invoke ptr "state" :ns-control-state))))

(defmethod (setf state) (state (obj state-mixin))
  (declare (type (member :on :off :mixed) state))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setState:" :ns-control-state state))))

(defclass bordered-mixin () ()
  (:documentation
   "Mixin class for obj support isBordered method. "))

(defgeneric borderedp (bordered-mixin)
  (:documentation
   "Get/Set if BORDERED-MIXIN is bordered or not. ")
  (:method ((obj bordered-mixin))
    (with-ptr obj ptr
      (invoke ptr "isBordered" :bool))))

(defmethod initialize-instance :after ((obj bordered-mixin) &key bordered)
  (setf (borderedp obj) bordered))

(defmethod (setf borderedp) (bordered (obj bordered-mixin)
                             &aux (borderedp (and bordered t)))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setBordered:" :bool borderedp))
    borderedp))

(defclass value-mixin () ()
  (:documentation
   "Base mixin class for those who support `value' method.

Dev Note:
+ don't use it but use it's subclasses.
"))

(defmethod initialize-instance :after ((obj value-mixin) &key (value nil value?))
  (when value? (setf (value obj) value)))

(defclass string-value-mixin (value-mixin) ()
  (:documentation
   "Mixin class for obj support stringValue method. "))

(defclass double-value-mixin (value-mixin) ()
  (:documentation
   "Mixin class for obj support doubleValue method. "))

(defgeneric value (widget)
  (:documentation
   "Return value of WIDGET. ")
  (:method ((obj string-value-mixin))
    (with-ptr obj ptr
      (invoke ptr "stringValue" :ns-string)))
  (:method ((obj double-value-mixin))
    (with-ptr obj ptr
      (invoke ptr "doubleValue" :double))))

(defmethod (setf value) ((value string) (obj string-value-mixin))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setStringValue:" :ns-string value))))

(defmethod (setf value) ((value number) (obj double-value-mixin))
  (with-ptr obj ptr
    (let ((val (coerce value 'double-float)))
      (dispatch-main ()
        (invoke ptr "setDoubleValue:" :double val)))))

(defclass font-mixin ()
  ((font
    :initarg  :font
    :initform (make-font)
    :type     font
    :reader   font))
  (:documentation
   "Mixin class for those who support font method. "))

(defmethod initialize-instance :after ((obj font-mixin) &key)
  (with-slots (font) obj
    (setf (font obj) font)))

(defgeneric font (widget)
  (:documentation
   "Return the `font' of WIDGET. "))

(defmethod (setf font) ((font font) (obj font-mixin))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setFont:" :ns-font font))
    (setf (slot-value obj 'font) font)))


;;;; define-coca-base-view

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *base-view-type-mapping* (make-hash-table :test 'eq)
    "
KEY: ObjC type keyword
VAL: an alist of (KEY . EXPAND-FORM)

Possible KEY:
+ `:objc-getf' a function called with (OBJ-VAR SLOT)
  should return a form like (slot-value OBJ-VAR 'SLOT),
  used to generate ObjC property get function
+ `:objc-setf' a function called with (NAME ATTR)
  should return a values like
  + (setf ATTR)
  + (optional) defmethod LAMDBA-LIST
+ `:objc-slot' a function called with (ATTR INIT-VALUE)
  should return a form like (:initform (...) :type ...)
  like defclass slot definition
")

  (defun get-base-view-type-rule (type rule &optional default)
    (declare (type keyword type rule))
    (let ((cons (assoc rule (gethash type *base-view-type-mapping*))))
      (if cons
          (cdr cons)
          default)))

  (defun (setf get-base-view-type-rule) (value type rule &optional default)
    (declare (type keyword type rule)
             (ignore default))
    (let ((cons (assoc rule (gethash type *base-view-type-mapping*))))
      (if cons
          (setf (cdr cons) value)
          (push (cons rule value) (gethash type *base-view-type-mapping*)))))

  (defun expand-base-view-getf (class attr type sel &optional cached)
    (if cached
        (alx:when-let* ((rule (get-base-view-type-rule type :objc-getf))
                        (expr (funcall rule class cached)))
          `(defmethod ,attr ((,class ,class))
             (with-slots (,cached) ,class
               ,expr)))
        `(defmethod ,attr ((,class ,class))
           (invoke (obj-ptr ,class) ,sel ,type))))

  (defun expand-base-view-setf (class attr type set-sel &optional cached)
    (flet ((default (name attr sel cached)
             `(defmethod (setf ,attr) (value (,name ,name))
                (with-ptr ,name ptr
                  (dispatch-main ()
                    (invoke ptr ,sel ,type value)))
                ,(if cached
                     (let ((slot (objc::symbol-concat "%" attr)))
                       `(setf (slot-value ,name ',slot) value))
                     'value))))
      (funcall (get-base-view-type-rule type :objc-setf #'default)
               class attr set-sel cached)))

  (defun expand-base-view-slot (slot attr type init-value)
    (flet ((default (attr init-value)
             `(:initarg  ,(intern (string attr) :keyword)
               :initform ,init-value
               :reader   ,attr)))
      (cons slot
            (funcall (get-base-view-type-rule type :objc-slot #'default)
                     attr init-value))))

  (defun expand-base-view-init (name attr type var var? init-value)
    (flet ((default (name attr var var? init-value)
             (declare (ignore var?))
             (values `(,var ,init-value)
                     `(setf (,attr ,name) ,var))))
      (funcall (get-base-view-type-rule type :objc-init #'default)
               name attr var var? init-value))))

(defmacro define-coca-base-view
    ((name objc-class &rest mixins)
     &body options
     &aux
       (direct-slots (cdr (assoc :direct-slots options)))
       gf-definition
       (init-keys     (reverse (cdr (assoc :init-keys     options))))
       (init-binding  (reverse (cdr (assoc :init-binding  options))))
       (init-prelude  (reverse (cdr (assoc :init-prelude  options))))
       (init          (reverse (cdr (assoc :init          options))))
       (init-postlude (reverse (cdr (assoc :init-postlude options)))))
  "Define a `base-view' subclass of NAME.

Syntax:

    (define-coca-base-view (NAME OBJC-CLASS . MIXINS)

      (:DIRECT-SLOTS
        ...)

      (:OBJC-PROPERTY
        (ATTR OBJC-TYPE SEL [DOCUMENTATION*])
        ...)

      (:DOCUMENTATION ...)

      (:DEFAULT-INITARGS ...)

      (:INIT-KEYS     . INIT-KEYS)
      (:INIT-BINDING  . INIT-BINDING)
      (:INIT          . INIT)
      (:INIT-POSTLUDE . INIT-POSTLUDE))

+ DIRECT-SLOTS:
  direct slots of NAME, same as `defclass'
+ OBJC-PROPERTY:
  + ATTR: ObjC property access generic function
  + OBJC-TYPE: ObjC type
    see `define-objc-typing'

    NOTE: there's some special ObjC typing like:
    + `:bool'
    + `:ns-rect'
      stored as #(X Y W H), use (set-ATTR obj X Y W H) when setting
    + `:ns-point'
      stored as #(X Y), use (set-ATTR obj X Y) when setting
    + `:ns-size'
      stored as #(W H), use (set-ATTR obj W H) when setting
  + SEL: ObjC property getter and setter SEL method
    + SEL: equal to (SEL setSel:) (auto generated SETTER)
    + (GETTER SETTER): specify SETTER directly
    + (GETTER :read-only): read-only property
    + (INITARG INIT-VALUE SETTER): property cached in slots
  + DOCUMENTATION: optional documentation
+ DOCUMENTATION:
  ObjC class definition documentation
+ INIT-KEYS: lambda list key definitions for custom keys
  used in `initialize-instance' :after
+ INIT, INIT-PRELUDE, INIT-POSTLUDE:
  the init progress be like:

      (let INIT-BINDING
        INIT-PRELUDE
        (dispatch-main ()
          INIT
          #|auto generated property bindings|#)
        INIT-POSTLUDE)

"
  (loop :for (attr type sel* . doc*) :in (cdr (assoc :objc-property options)) :do
    (when doc*
      (push `(defgeneric ,attr (,name)
               (:documentation ,(apply #'concatenate 'string doc*)))
            gf-definition))
    (macrolet ((when-push (expr place)
                 `(alx:when-let ((expr ,expr))
                    (push ,expr ,place))))
      (m:match sel*
        ;; SEL
        ((and (type string) sel)
         (let ((set-sel (concatenate 'string
                                     "set"
                                     (string-upcase sel :end 1)
                                     ":")))
           (when-push (expand-base-view-getf name attr type sel)     gf-definition)
           (when-push (expand-base-view-setf name attr type set-sel) gf-definition)))
        ;; (SEL :read-only)
        ((list (and (type string) sel)
               :read-only)
         (when-push (expand-base-view-getf name attr type sel) gf-definition))
        ;; (SEL SET-SEL)
        ((list (and (type string) sel)
               (and (type string) set-sel))
         (when-push (expand-base-view-getf name attr type sel)     gf-definition)
         (when-push (expand-base-view-setf name attr type set-sel) gf-definition))
        ;; (INITARG INIT-VALUE SET-SEL)
        ((list (and (type keyword) initarg)
               init-value
               (and (type string) set-sel))
         (let ((slot (objc::symbol-concat "%" attr)))
           (when-push (expand-base-view-getf name attr type nil     slot) gf-definition)
           (when-push (expand-base-view-setf name attr type set-sel slot) gf-definition)
           (when-push (expand-base-view-slot slot attr type init-value)   direct-slots)

           (let ((var  (objc::symbol-concat initarg))
                 (var? (objc::symbol-concat initarg "?")))
             (multiple-value-bind (key-def inits prelude postlude)
                 (expand-base-view-init name attr type var var? init-value)
               (when key-def
                 (push key-def       init-keys)
                 (when-push inits    init)
                 (when-push prelude  init-prelude)
                 (when-push postlude init-postlude)))))))))
  `(progn
     (defclass ,name (base-view ,@mixins)
       ,direct-slots
       (:default-initargs
        :objc-class ,objc-class
        ,@(cdr (assoc :default-initargs options)))
       (:documentation
        ,(or (second (assoc :documentation options))
             (format nil "Wrapper of ~A. " objc-class))))
     ,(when (or init-prelude init init-postlude)
        `(defmethod initialize-instance :after
             ((,name ,name) &key ,@init-keys)
           (let ,init-binding
             ,@(reverse init-prelude)
             ,@(when init `((dispatch-main () ,@ (reverse init))))
             ,@(reverse init-postlude))))
     ,@(reverse gf-definition)))

(defmacro define-coca-base-view-rule (typing &body options
                                 &aux )
  "Define expanding rules for ObjC TYPING used when `define-coca-base-view'.

Syntax:

    (define-coca-base-view-rule TYPING
      ((:objc-getf|:objc-setf|:objc-slot . LAMBDA-LIST) &body)
      (:objc-getf|:objc-setf|:objc-slot FUNCTION))

Rules:
+ :objc-getf
  expression of how to get cached value
+ :objc-setf
  expression of how to set property,
  if CACHED, it is adviced to update the caced value
+ :objc-slot
  how the property should be cached in lisp class slot

Example:

  ((:objc-getf NAME SLOT)
    `(slot-value ,NAME ',SLOT))

  ((:objc-setf NAME ATTR SEL CACHED)
    `(defmethod (setf ,ATTR) (VALUE (NAME ,NAME))
       (with-ptr ,NAME ptr
         (dispatch-main ()
           (invoke ptr ,sel ,type value)))
       ,(if CACHED
            `(setf (slot-value ,name ',CACHED) value)
            ',value)))

  ((:objc-slot ATTR INIT-VALUE)
    `(:initarg  ,(intern (string ATTR) :keyword)
      :initform ,init-value))
"
  (declare (type keyword typing))
  (flet ((set! (rule value)
           `(setf (get-base-view-type-rule ,typing ,rule) ,value))
         (check (lambda-list expect)
           (unless (= (length lambda-list)
                      (length expect))
             (error "Wrong lambda-list ~S expecting ~S. "
                    lambda-list expect))))
    `(progn
       ,@(loop :for (rule* . body) :in options
               :if (listp rule*)
                 :collect
                 (let ((rule    (first rule*))
                       (lambda? (rest  rule*)))
                   (ecase rule
                     (:objc-getf (check lambda? '(name slot)))
                     (:objc-setf (check lambda? '(name attr sel cached)))
                     (:objc-slot (check lambda? '(attr init-value)))
                     (:objc-init (check lambda? '(name attr var var? init-value))))
                   (set! rule `(lambda ,lambda? ,@body)))
               :else
                 :collect (if (endp (cdr body))
                              (set! rule* (car body))
                              (set! rule* (cons 'progn body)))))))

(define-coca-base-view-rule :ns-rect
  ((:objc-getf name slot)
   (declare (ignore name))
   `(values (aref ,slot 0)
            (aref ,slot 1)
            (aref ,slot 2)
            (aref ,slot 3)))
  ((:objc-setf name attr sel cached)
   (let ((setf (objc::symbol-concat "SET-" attr)))
     `(defmethod ,setf ((,name ,name) (x real) (y real) (w real) (h real))
        (declare (type framed-size w h))
        (with-ptr ,name ptr
          (let ((x (coerce x 'double-float))
                (y (coerce y 'double-float))
                (w (coerce w 'double-float))
                (h (coerce h 'double-float)))
            (dispatch-main ()
              (invoke ptr ,sel
                      :double x
                      :double y
                      :double w
                      :double h))
            ,@(when cached
                `((with-slots (,cached) ,name
                    (setf (aref ,cached 0) x
                          (aref ,cached 1) y
                          (aref ,cached 2) w
                          (aref ,cached 3) h)))))
          ,name))))
  ((:objc-slot attr init-value)
   (declare (ignore attr))
   `(:initform (make-array 4 :initial-contents ,init-value
                             :element-type 'double-float)
     :type     (simple-array double-float (4))))
  ((:objc-init name attr var var? init-value)
   (declare (ignore init-value))
   (values `(,var nil ,var?)
           (let ((slot (objc::symbol-concat "%"    attr))
                 (setf (objc::symbol-concat "SET-" attr)))
             `(if ,var?
                  (m:ematch ,var
                    ((list   x y w h) (,setf ,name x y w h))
                    ((vector x y w h) (,setf ,name x y w h)))
                  (with-slots (,slot) ,name
                    (,setf ,name
                           (aref ,slot 0)
                           (aref ,slot 1)
                           (aref ,slot 2)
                           (aref ,slot 3))))))))

(define-coca-base-view-rule :ns-point
  ((:objc-getf name slot)
   (declare (ignore name))
   `(values (aref ,slot 0)
            (aref ,slot 1)))
  ((:objc-setf name attr sel cached)
   (let ((setf (objc::symbol-concat "SET-" attr)))
     `(defmethod ,setf ((,name ,name) (x real) (y real))
        (with-ptr ,name ptr
          (let ((x (coerce x 'double-float))
                (y (coerce y 'double-float)))
            (dispatch-main ()
              (invoke ptr ,sel :double x :double y))
            ,@(when cached
                `((with-slots (,cached) ,name
                    (setf (aref ,cached 0) x
                          (aref ,cached 1) y)))))
          ,name))))
  ((:objc-slot attr init-value)
   (declare (ignore attr))
   `(:initform (make-array 2 :initial-contents ,init-value
                             :element-type 'double-float)
     :type     (simple-array double-float (2))))
  ((:objc-init name attr var var? init-value)
   (declare (ignore init-value))
   (values `(,var nil ,var?)
           (let ((slot (objc::symbol-concat "%"    attr))
                 (setf (objc::symbol-concat "SET-" attr)))
             `(if ,var?
                  (m:ematch ,var
                    ((list   x y) (,setf ,name x y))
                    ((vector x y) (,setf ,name x y)))
                  (with-slots (,slot) ,name
                    (,setf ,name
                           (aref ,slot 0)
                           (aref ,slot 1))))))))

(define-coca-base-view-rule :ns-size
  ((:objc-getf name slot)
   (declare (ignore name))
   `(values (aref ,slot 0)
            (aref ,slot 1)))
  ((:objc-setf name attr sel cached)
   (let ((setf (objc::symbol-concat "SET-" attr)))
     `(defmethod ,setf ((,name ,name) (w real) (h real))
        (declare (type framed-size w h))
        (with-ptr ,name ptr
          (let ((w (coerce w 'double-float))
                (h (coerce h 'double-float)))
            (dispatch-main ()
              ;; patch of using `:ns-point',
              ;; this skips coerce process in main thread
              ;; to save some time
              (invoke ptr ,sel :double w :double h))
            ,@(when cached
                `((with-slots (,cached) ,name
                    (setf (aref ,cached 0) w
                          (aref ,cached 1) h)))))
          ,name))))
  ((:objc-slot attr init-value)
   (declare (ignore attr))
   `(:initform (make-array 2 :initial-contents ,init-value
                             :element-type 'double-float)
     :type     (simple-array double-float (2))))
  ((:objc-init name attr var var? init-value)
   (declare (ignore init-value))
   (values `(,var nil ,var?)
           (let ((slot (objc::symbol-concat "%"    attr))
                 (setf (objc::symbol-concat "SET-" attr)))
             `(if ,var?
                  (m:match ,var
                    ((list   w h) (,setf ,name w h))
                    ((vector w h) (,setf ,name w h)))
                  (with-slots (,slot) ,name
                    (,setf ,name
                           (aref ,slot 0)
                           (aref ,slot 1))))))))

(define-coca-base-view-rule :double
  ((:objc-setf name attr sel cached)
   `(defmethod (setf ,attr) ((value real) (,name ,name)
                             &aux (val (coerce value 'double-float)))
      (with-ptr ,name ptr
        (dispatch-main ()
          (invoke ptr ,sel :double val))
        ,(if cached
             `(setf (slot-value ,name ',cached) val)
             'val)))))

(define-coca-base-view-rule :ns-string
  ((:objc-setf name attr sel cached)
   `(defmethod (setf ,attr) ((value string) (,name ,name))
      (with-ptr ,name ptr
        (dispatch-main ()
          (invoke ptr ,sel :ns-string value))
        ,(if cached
             `(setf (slot-value ,name ',cached) value)
             'value)))))

;;;; utils.lisp ends here
