;;;; cffi.lisp --- CFFI bindings to ObjC Runtime

(in-package :coca.objc)


;;;; Lowlevel

(define-foreign-library foundation
  (:darwin (:framework "Foundation")))
(use-foreign-library foundation)

(define-foreign-library cocoa
  (:darwin (:framework "Cocoa")))
(use-foreign-library cocoa)

;;; SEL

(defcfun (sel_registerName "sel_registerName") :pointer
  "extern SEL sel_registerName(const char * str);

Registers a method with the Objective-C runtime system,
maps the method name to a selector, and returns the selector value.

Return a pointer of type SEL specifying the selector for the named method.

See https://developer.apple.com/documentation/objectivec/sel_registername(_:)?language=objc"
  (name :string))

(defcfun (sel_getName "sel_getName") :string
  "extern const char * sel_getName(SEL sel);

Returns the name of the method specified by a given selector.

See https://developer.apple.com/documentation/objectivec/sel_getname(_:)?language=objc"
  (sel :pointer))

;;; ObjC

(defcfun (objc_getClass "objc_getClass") :pointer
  "extern id objc_getClass(const char * name);

Returns the class definition of a specified class.

See https://developer.apple.com/documentation/objectivec/objc_getclass(_:)?language=objc"
  (name :string))

(defcfun (objc_getClassList "objc_getClassList") :int
  "extern int objc_getClassList(Class * buffer, int bufferCount);

Obtains the list of registered class definitions.

Return an integer value indicating the total number of registered classes.

Parameters:
+ BUFFER
  An array of Class values. On output, each Class value points to one class definition,
  up to either bufferCount or the total number of registered classes, whichever is less.
  You can pass NULL to obtain the total number of registered class definitions without
  actually retrieving any class definitions.
+ BUFFER-COUNT
  An integer value. Pass the number of pointers for which you have allocated space in buffer.
  On return, this function fills in only this number of elements. If this number is less than
  the number of registered classes, this function returns an arbitrary subset of the registered
  classes.

See https://developer.apple.com/documentation/objectivec/objc_getclasslist(_:_:)?language=objc"
  (buffer       :pointer)
  (buffer-count :int))

;;; Class

(defcfun (class_getName "class_getName") :string
  "extern const char * class_getName(Class cls);

Return the name of the class, or the empty string if cls is Nil.

See https://developer.apple.com/documentation/objectivec/class_getname(_:)?language=objc"
  (class :pointer))

(defcfun (class_getSuperclass "class_getSuperclass") :pointer
  "extern Class class_getSuperclass(Class cls);

Return the superclass of the class, or Nil if cls is a root class, or Nil if cls is Nil.

See https://developer.apple.com/documentation/objectivec/class_getsuperclass(_:)?language=objc"
  (class :pointer))

(defcfun (class_getClassMethod "class_getClassMethod") :pointer
  "extern Method class_getClassMethod(Class cls, SEL name);

Return a pointer to the Method data structure that corresponds to the
implementation of the selector specified by aSelector for the class
specified by aClass, or NULL if the specified class or its superclasses
do not contain a class method with the specified selector.

See https://developer.apple.com/documentation/objectivec/class_getclassmethod(_:_:)?language=objc"
  (class :pointer)
  (sel   :pointer))

(defcfun (class_getInstanceMethod "class_getInstanceMethod") :pointer
  "extern Method class_getInstanceMethod(Class cls, SEL name);

Returns a specified instance method for a given class.
The method that corresponds to the implementation of the selector specified
by aSelector for the class specified by aClass, or NULL if the specified
class or its superclasses do not contain an instance method with the
specified selector.

See https://developer.apple.com/documentation/objectivec/class_getinstancemethod(_:_:)?language=objc"
  (class :pointer)
  (sel   :pointer))

(defcfun (class_addMethod "class_addMethod") :bool
  "extern BOOL class_addMethod(Class cls, SEL name, IMP imp, const char * types);

Adds a new method to a class with a given name and implementation.

Return `t' if the method was added successfully, otherwise `nil'
(for example, the class already contains a method implementation with that name).

Discussion:
`coca.objc::class_addMethod' will add an override of a superclass’s implementation,
but will not replace an existing implementation in this class. To change an existing
implementation, use `coca.objc::method_setImplementation'.

An Objective-C method is simply a C function that take at least two arguments:
`self' and `sel'.

For example, given the following function:

void myMethodIMP(id self, SEL _cmd)
{
    // implementation ....
}

you can dynamically add it to a class as a method (called resolveThisMethodDynamically)
like this:

class_addMethod([self class], @selector(resolveThisMethodDynamically), (IMP) myMethodIMP, \"v@:\");

See: https://developer.apple.com/documentation/objectivec/class_addmethod(_:_:_:_:)?language=objc
"
  (class :pointer)
  (sel   :pointer)
  (imp   :pointer)
  (types :string))

(defcfun (class_copyMethodList "class_copyMethodList") :pointer
  "extern Method * class_copyMethodList(Class cls, unsigned int * outCount);

Describes the instance methods implemented by a class.

Return an array of pointers of type Method describing the instance methods
implemented by the class—any instance methods implemented by superclasses are
not included. The array contains *outCount pointers followed by a NULL
terminator. You must free the array with free().

If cls implements no instance methods, or cls is Nil, returns NULL and *outCount is 0.

Parameters:
+ CLASS: Class you want to insepct
+ COUNT: On return, contains the length of the returned array.
  If outCount is NULL, the length is not returned.
"
  (class :pointer)
  (count :pointer))

;;; Object

(defcfun (object_isClass "object_isClass") :bool
  "extern BOOL object_isClass(id obj);

See https://developer.apple.com/documentation/objectivec/object_isclass(_:)?language=objc"
  (object :pointer))

(defcfun (object_getClassName "object_getClassName") :string
  "extern const char * object_getClassName(id obj);

Returns the class name of a given object.

See https://developer.apple.com/documentation/objectivec/object_getclassname(_:)?language=objc"
  (object :pointer))

;;; Method

(defcfun (method_getTypeEncoding "method_getTypeEncoding") :string
  "extern const char * method_getTypeEncoding(Method m);

Returns a string describing a method’s parameter and return types,
which should be a C string. The string may be NULL.

See https://developer.apple.com/documentation/objectivec/method_gettypeencoding(_:)?language=objc"
  (method :pointer))

(defcfun (method_getName "method_getName") :pointer
  "extern SEL method_getName(Method m);

A pointer of type SEL.

To get the method name as a C string, call sel_getName(method_getName(method)).

See https://developer.apple.com/documentation/objectivec/method_getname(_:)?language=objc"
  (method :pointer))


;;; Highlevel

(defclass objc-pointer ()
  ((objc-object-pointer
    :initarg       :objc-object-pointer
    :reader        objc-object-pointer
    :documentation "Return the foreign-pointer in ObjC. "))
  (:documentation
   "Object pointer of ObjC.

Use `objc-object-pointer' to fetch CFFI foreign-pointer.

Dev Note:
Subclass of `objc-pointer' should implement function whose name
should be like: `coerce-to-{...}', which take in a `foreign-pointer'
and return the instance of `objc-pointer'. "))

(defgeneric objc-object-pointer (objc-pointer)
  (:documentation
   "Returns the ObjC foreign pointer associated with a given Lisp object.")
  (:method (foreign-pointer)
    "Return foreign-pointer directly. "
    (declare (type foreign-pointer foreign-pointer))
    foreign-pointer))

(defclass standard-objc-object (objc-pointer)
  ()
  (:documentation
   "The class from which all classes that implement an ObjC class should inherit.

Dev Note:
You may not need to directly use `standard-objc-object'.
It's just a handy CLOS notation of `coca.objc::objc-pointer',
making ObjC object different if you want to test type of `coca.objc::objc-pointer'. "))

;;;; cffi.lisp ends here
