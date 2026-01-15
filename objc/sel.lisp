;;;; sel.lisp --- Wrap for ObjC SEL

(in-package :coca.objc)

(defvar *sels* (make-hash-table :test 'equal)
  "Cache of ObjC SEL.

KEY: string of SEL name
VAL: `sel' object. ")

(defclass sel (objc-pointer)
  ((name :initarg :name
         :reader  sel-name))
  (:documentation
   "A foreign type for Objective-C method selectors.

Use `coerce-to-selector' to make a `sel' object.

Dev Note:
Every `sel' instance should be cached within `coca.objc::*sels*'
So not recommanded to use `make-instance' to alloc `sel' instance."))

(defmethod print-object ((sel sel) stream)
  (with-slots (name) sel
    (print-unreadable-object (sel stream)
      (format stream
              "~S ~A ~X"
              (class-name (class-of sel))
              name
              (pointer-address (objc-object-pointer sel))))))

(defvar *objc-methods* (make-hash-table))

(defclass objc-generic-function (c2mop:standard-generic-function objc-pointer)
  ((objc-class
    :type    objc-class
    :initarg :objc-class
    :documentation
    "The most root class of ObjC generic function rewrites.

If setted with new ObjC class:
+ the new class is super class of old ObjC class:
  replace with new ObjC class and updating the objc-object-pointer
+ the new class is subclass of old ObjC class:
  do nothing

")
   (objc-type
    :type    string
    :initarg :objc-type
    :documentation
    "ObjC method type encoding string.
see `coca.objc::encode-objc-type-encoding'. ")
   (sel
    :type    sel
    :initarg :sel))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation
   "Generic function of ObjC object. "))

(defun objc-generic-function-p (gf)
  "Test if GF is `coca.objc::objc-generic-function'. "
  (typep gf 'objc-generic-function))

(defun coerce-to-selector (method)
  "Coerces its argument to an Objective-C method selector.
Return `sel'.

Parameters:
+ METHOD: string, `sel' or foreign-pointer.
  + `sel':
    return itself
  + string:
    get the `sel' by name
    see `coca.objc::sel_registerName'
  + foreign-pointer:
    assuming it is a pointer to SEL (not checked)
    see `coca.objc::sel_getName'
"
  (declare (type (or string sel foreign-pointer symbol objc-generic-function) method))
  (the sel
    (etypecase method
      (sel    method)
      (string
       (with-cached (method *sels*)
         (make-instance 'sel
                        :objc-object-pointer (sel_registerName method)
                        :name                method)))
      (foreign-pointer
       (let ((name (sel_getName method)))
         (with-cached (name *sels*)
           (make-instance 'sel
                          :objc-object-pointer method
                          :name                name))))
      (symbol
       (let ((gf (the objc-generic-function (symbol-function method))))
         (slot-value gf 'sel)))
      (objc-generic-function
       (slot-value method 'sel)))))

(defmethod initialize-instance :after ((sel sel) &key)
  (unless (slot-boundp sel 'objc-object-pointer)
    (setf (slot-value sel 'objc-object-pointer) (sel_registerName (sel-name sel)))))

(defmethod reinitialize-instance :around ((sel sel) &key)
  (setf (slot-value sel 'objc-object-pointer) (sel_registerName (sel-name sel)))
  (call-next-method))

;;;; sel.lisp ends here
