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

(defcfun (objc_getProtocol "objc_getProtocol") :pointer
  "extern Protocol * objc_getProtocol(const char * name);

Returns a specified protocol, or `null-pointer' if cannot find.

Parameters:
+ NAME: The name of a protocol

See https://developer.apple.com/documentation/objectivec/objc_getprotocol(_:)?changes=_4__8_5__4&language=objc"
  (name         :string))

(defcfun (objc_allocateProtocol "objc_allocateProtocol") :pointer
  "extern Protocol * objc_allocateProtocol(const char * name);

Creates a new protocol instance.

Return a new protocol instance or nil if a protocol with the same name as name already exists.

Parameters:
+ NAME: the name of the protocol you want to create

See https://developer.apple.com/documentation/objectivec/objc_allocateprotocol(_:)?changes=_4__8_5__4&language=objc"
  (name         :string))

(defcfun (objc_registerProtocol "objc_registerProtocol") :void
  "extern void objc_registerProtocol(Protocol * proto);

Registers a newly created protocol with the Objective-C runtime.

Parameters:
+ PROTOCOL: The protocol you want to register with the Objective-C runtime.

When you create a new protocol using the objc_allocateProtocol, you
then register it with the Objective-C runtime by calling this
function. After a protocol is successfully registered, it is immutable
and ready to use."
  (protocol     :pointer))

(defcfun (objc_autoreleasePoolPush "objc_autoreleasePoolPush") :pointer
  "Creates a new autorelease pool that is enclosed by the current pool,
makes that the current pool, and returns an opaque “handle” to it.

See https://clang.llvm.org/docs/AutomaticReferenceCounting.html#void-objc-autoreleasepoolpush-void")

(defcfun (objc_autoreleasePoolPop "objc_autoreleasePoolPop") :void
  "Releases all the objects added to the given autorelease pool and
any autorelease pools it encloses, then sets the current autorelease pool to
the pool directly enclosing pool.

Parameters:
+ POOL: pointer of previous `objc_autoreleasePoolPush' returned pointer.

See https://clang.llvm.org/docs/AutomaticReferenceCounting.html#void-objc-autoreleasepoolpop-void-pool"
  (pool :pointer))

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

(defcfun (class_replaceMethod "class_replaceMethod") :pointer
  "extern IMP class_replaceMethod(Class cls, SEL name, IMP imp, const char * types);

Replaces the implementation of a method for a given class.

Parameters:
+ CLASS: The class you want to modify.
+ NAME: A selector that identifies the method whose implementation
  you want to replace.
+ IMP: The new implementation for the method identified by name
  for the class identified by cls.
+ TYPES:  An array of characters that describe the types of the
  arguments to the method. For possible values, see Objective-C Runtime
  Programming Guide > Type Encodings. Since the function must take at
  least two arguments—self and _cmd, the second and third characters
  must be “@:” (the first character is the return type).

This function behaves in two different ways:
+ If the method identified by name does not yet exist, it is added as
  if `class_addMethod' were called. The type encoding specified by types
  is used as given.
+ If the method identified by name does exist, its IMP is replaced as
  if `method_setImplementation' were called. The type encoding specified
  by types is ignored.

See https://developer.apple.com/documentation/objectivec/class_replacemethod(_:_:_:_:)?language=objc"
  (class      :pointer)
  (name       :pointer)
  (imp        :pointer)
  (types      :string))

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

(defcfun (class_copyPropertyList "class_copyPropertyList") :pointer
  "extern objc_property_t * class_copyPropertyList(Class cls, unsigned int * outCount);

Describes the properties declared by a class.

Parameters:
+ CLASS: ObjC class to inspect
+ OUT-COUNT: On return, contains the length of the returned array.
  If outCount is NULL, the length is not returned.

An array of pointers of type objc_property_t describing the properties
declared by the class. Any properties declared by superclasses are not
included. The array contains *outCount pointers followed by a NULL
terminator. You must free the array with free().  If cls declares no
properties, or cls is Nil, returns NULL and *outCount is 0.

see https://developer.apple.com/documentation/objectivec/class_copypropertylist(_:_:)?language=objc"
  (class     :pointer)
  (out-count :pointer))

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

(defcfun (method_getImplementation "method_getImplementation") :pointer
  "extern IMP method_getImplementation(Method m);

Returns the implementation of a method.

Parameters:
+ METHOD: pointer to method

See https://developer.apple.com/documentation/objectivec/method_getimplementation(_:)?language=objc"
  (method :pointer))

;;; Protocol

(defcfun (protocol_getName "protocol_getName") :string
  "extern const char * protocol_getName(Protocol * proto);

Returns the name of a protocol.

Parameters:
+ PROTOCOL: foreign-pointer of ObjC Protocol

See https://developer.apple.com/documentation/objectivec/protocol_getname(_:)?changes=_4__8_5__4&language=objc"
  (protocol :pointer))

(defcfun (protocol_addProtocol "protocol_addProtocol") :void
  "extern void protocol_addProtocol(Protocol * proto, Protocol * addition);

Adds a registered protocol to another protocol that is under construction.

Parameters:
+ PROTOCOL: The protocol you want to add the registered protocol to.
+ ADDITION: The registered protocol you want to add to proto.

The protocol you want to add to (proto) must be under
construction—allocated but not yet registered with the Objective-C
runtime. The protocol you want to add (addition) must be registered
already.

See https://developer.apple.com/documentation/objectivec/protocol_addprotocol(_:_:)?language=objc"
  (protocol :pointer)
  (addition :pointer))

(defcfun (protocol_addMethodDescription "protocol_addMethodDescription") :void
  "extern void protocol_addMethodDescription(Protocol * proto, SEL name, const char * types, BOOL isRequiredMethod, BOOL isInstanceMethod);

Adds a method to a protocol.

Parameters:
+ PROTOCOL:   The protocol you want to add a method to.
+ SEL:        The name of the method you want to add.
+ ENCODING:   A C string representing the signature of the method you want to add.
+ REQUIRED-P: A Boolean indicating whether the method is a required method of the proto protocol.
  If `t', the method is a required method;
  if `nil', the method is an optional method.
+ INSTANCE-P: A Boolean indicating whether the method is an instance method.
  If `t', the method is an instance method; if `nil', the method is a class method.

To add a method to a protocol using this function, the protocol must be under construction.
That is, you must add any methods to proto before you register it with the Objective-C runtime
(via the `objc_registerProtocol' function)."
  (protocol     :pointer)
  (sel          :pointer)
  (encoding     :string)
  (required-p   :bool)
  (instance-p   :bool))

(defcfun (protocol_conformsToProtocol "protocol_conformsToProtocol") :bool
  "extern BOOL protocol_conformsToProtocol(Protocol * proto, Protocol * other);

Returns a Boolean value that indicates whether one protocol conforms to another protocol.

Parameters:
+ PROTOCOL: a protocol pointer
+ OTHER: a protocol pointer

One protocol can incorporate other protocols using the same syntax that
classes use to adopt a protocol:

    @protocol ProtocolName < protocol list >

All the protocols listed between angle brackets are considered part of
the ProtocolName protocol."
  (protocol     :pointer)
  (other        :pointer))

;;; Property

(defcfun (property_getName "property_getName") :string
  "extern const char * property_getName(objc_property_t property);

Returns the name of a property.

see https://developer.apple.com/documentation/objectivec/property_getname(_:)?language=objc"
  (property  :pointer))

(defcfun (property_getAttributes "property_getAttributes") :string
  "extern const char * property_getAttributes(objc_property_t property);

Returns the attribute string of a property.

see https://developer.apple.com/documentation/objectivec/property_getattributes(_:)?language=objc"
  (property  :pointer))


;;; LibFFI
;; the wrapper function `%coca_objc_msgSend' is defined in `wrapper.lisp'.


;;; Highlevel

(defclass objc-pointer ()
  ((objc-object-pointer
    :type          foreign-pointer
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

(defmethod print-object ((objc objc-pointer) stream)
  "The OBJC or `coca.objc::objc-pointer' is printed like #<CLASS POINTER>. "
  (print-unreadable-object (objc stream)
    (format stream "~S ~X"
            (class-name (class-of objc))
            (pointer-address (the foreign-pointer (objc-object-pointer objc))))))

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
It's just a handy CLOS notation of `coca.objc::objc-pointer', making
ObjC object different if you want to test type of `coca.objc::objc-pointer'. "))

;;;; cffi.lisp ends here
