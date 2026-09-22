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

(defmacro define-coca-base-view
    ((name objc-class &rest mixins)
     &rest options
     &aux
       (direct-slots (cdr (assoc :direct-slots options)))
       def-gfs keys
       cached-let cached-init cached cached-after)
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

      (:AFTER-INITIALIZE LAMBDA-LIST ...))

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
+ AFTER-INITIALIZE:
  + LAMBDA-LIST: additional key parameter definitions
  + BODY: called in main thread to initialize, use NAME to refer itself
"
  (flet ((objc-set-init (attr type initarg init-value)
           (case type
             ((:ns-point :ns-size)
              (let ((slot   (objc::symbol-concat "%"    attr))
                    (setter (objc::symbol-concat "SET-" attr))
                    (key    (objc::symbol-concat initarg))
                    (key?   (objc::symbol-concat initarg "?")))
                (alx:with-gensyms (val)
                  (push (list key nil key?) keys)
                  (push `(,setter ,name (aref ,val 0) (aref ,val 1)) cached)
                  (push `((,val (slot-value ,name ',slot))) cached-let)
                  (push `(when ,key?
                           (m:match ,key
                             ((list val1 val2)
                              (setf (aref ,val 0) val1
                                    (aref ,val 1) val2))
                             ((vector val1 val2)
                              (setf (aref ,val 0) val1
                                    (aref ,val 1) val2))
                             (_
                              (error ,(format nil
                                              "Malformed `:~A'.
Expecting ~A. "
                                              attr
                                              (case type
                                                (:ns-point
                                                 "(X Y) or #(X Y)")
                                                (:ns-size
                                                 "(W H) or #(W H)")))))))
                        cached-init))))
             ((:ns-rect)
              (let ((slot   (objc::symbol-concat "%"    attr))
                    (setter (objc::symbol-concat "SET-" attr))
                    (key    (objc::symbol-concat initarg))
                    (key?   (objc::symbol-concat initarg "?")))
                (alx:with-gensyms (val)
                  (push (list key nil key?) keys)
                  (push `(,setter ,name
                                  (aref ,val 0)
                                  (aref ,val 1)
                                  (aref ,val 2)
                                  (aref ,val 3))
                        cached)
                  (push `((,val (slot-value 'name ',slot))) cached-let)
                  (push `(when ,key?
                           (m:match ,key
                             ((list x y w h)
                              (setf (aref ,key 0) x
                                    (aref ,key 1) y
                                    (aref ,key 2) w
                                    (aref ,key 3) h))
                             ((vector x y w h)
                              (setf (aref ,key 0) x
                                    (aref ,key 1) y
                                    (aref ,key 2) w
                                    (aref ,key 3) h))
                             (_
                              (error ,(format nil
                                              "Malformed `:ns-size'.
Expecting (X Y W H) or #(X Y W H). ")))))
                        cached-init))))
             (otherwise
              (let ((key (objc::symbol-concat initarg)))
                (push `(,key ,init-value) keys)
                (push `(setf (,attr ,name) ,key)        cached)))))
         (objc-setf (attr type sel)
           (case type
             (:ns-point
              (let ((setterf (objc::symbol-concat "SET-" attr)))
                `(defmethod ,setterf ((,name ,name) (x real) (y real))
                   (with-ptr ,name ptr
                     (dispatch-main ()
                       (invoke ptr ,sel :ns-point (x y)))))))
             (:ns-size
              (let ((setterf (objc::symbol-concat "SET-" attr)))
                `(defmethod ,setterf ((,name ,name) (w real) (h real))
                   (with-ptr ,name ptr
                     (dispatch-main ()
                       (invoke ptr ,sel :ns-size (w h)))))))
             (:ns-rect
              (let ((setterf (objc::symbol-concat "SET-" attr)))
                `(defmethod ,setterf
                     ((,name ,name) (x real) (y real) (w real) (h real))
                   (with-ptr ,name ptr
                     (dispatch-main ()
                       (invoke ptr ,sel :ns-rect (x y w h)))))))
             (:double
              `(defmethod (setf ,attr) ((value real) (,name ,name)
                                        &aux (val (coerce value 'double-float)))
                 (with-ptr ,name ptr
                   (dispatch-main ()
                     (invoke ptr ,sel ,type val)))))
             (:bool
              `(defmethod (setf ,attr) (value (,name ,name)
                                        &aux (val (and value t)))
                 (with-ptr ,name ptr
                   (dispatch-main ()
                     (invoke ptr ,sel ,type val)))))
             (otherwise
              `(defmethod (setf ,attr) (value (,name ,name))
                 (with-ptr ,name ptr
                   (dispatch-main ()
                     (invoke ptr ,sel ,type value)))))))
         (objc-getf (attr type sel)
           `(defmethod ,attr ((,name ,name))
              (invoke (obj-ptr ,name) ,sel ,type)))
         (objc-slot (attr type initarg init-value)
           (case type
             ((:ns-size :ns-point)
              (let ((slot (objc::symbol-concat "%" attr)))
                `(,slot :initarg  ,initarg
                        :initform (make-array 2
                                              :element-type 'double-float
                                              :initial-contents ,init-value))))
             (:ns-rect
              (let ((slot (objc::symbol-concat "%" attr)))
                `(,slot :initarg  ,initarg
                        :initform (make-array 4
                                              :element-type 'double-float
                                              :initial-contents ,init-value))))
             (otherwise
              `(,attr :initarg  ,initarg
                      :initform ,init-value))))
         (sel->set-sel (sel)
           ;; convert somethingLikeThis to setSomethingLikeThis:
           (declare (type string sel))
           (concatenate 'string
                        "set"
                        (string-upcase sel :end 1)
                        ":"))
         (split (list &rest keys)
           (loop :with subseqs := ()
                 :with collection := ()
                 :for elem :in list
                 :if (find elem keys)
                   :do (setf subseqs    (cons (reverse collection) subseqs)
                             collection (list elem))
                 :else
                   :do (push elem collection)
                 :finally (return (reverse (cons (reverse collection)
                                                 subseqs))))))
    (alx:when-let ((after (cdr (assoc :after-initialize options))))
      (setf keys (first after))
      (destructuring-bind (body . rest-body)
          (split (rest after) :prelude :postlude :binding)
        (setf cached body)
        (loop :for (type . body) :in rest-body
              :do (case type
                    (:binding  (setf cached-let   (list body)))
                    (:prelude  (setf cached-init  body))
                    (:postlude (setf cached-after body))))))
    (loop :for (attr type sel* . doc*) :in (cdr (assoc :objc-property options))
          :do (when doc*
                (push `(defgeneric ,attr (,name)
                         (:documentation ,(apply #'concatenate 'string doc*)))
                      def-gfs))
              (m:match sel*
                ;; SEL
                ((and (type string) sel)
                 (let ((set-sel (sel->set-sel sel)))
                   (push (objc-getf attr type sel)     def-gfs)
                   (push (objc-setf attr type set-sel) def-gfs)))
                ;; (SEL :read-only)
                ((list (and (type string) sel)
                       :read-only)
                 (push (objc-getf attr type sel)     def-gfs))
                ;; (SEL SET-SEL)
                ((list (and (type string) sel)
                       (and (type string) set-sel))
                 (push (objc-getf attr type sel)     def-gfs)
                 (push (objc-setf attr type set-sel) def-gfs))
                ;; (INITARG INIT-VALUE SET-SEL)
                ((list (and (type keyword) initarg)
                       init-value
                       (and (type string) set-sel))
                 (push (objc-setf attr type set-sel)            def-gfs)
                 (push (objc-slot attr type initarg init-value) direct-slots)
                 (objc-set-init attr type initarg init-value))
                (_
                 (error "Malformed SEL definition in OBJC-TYPE.
Expecting SEL, (SEL SET-SEL), (SEL :read-only) or (INIT-VALUE SET-SEL),
but got ~S. "
                        sel*))))
    `(progn
       (defclass ,name (base-view ,@mixins)
         ,direct-slots
         (:default-initargs
          :objc-class ,objc-class
          ,@(cdr (assoc :default-initargs options)))
         (:documentation
          ,@(or (cdr (assoc :documentation options))
                (list (format nil "Wrapper of ~A. " objc-class)))))
       ,@(when cached
           `((defmethod initialize-instance :after ((,name ,name) &key ,@keys)
               (let ,(apply #'append cached-let)
                 ,@cached-init
                 (dispatch-main ()
                   ,@cached)
                 ,@cached-after))))
       ,@(reverse def-gfs))))

;;;; utils.lisp ends here
