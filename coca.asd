;;;; coca.asd --- Cocoa in Common Lisp

(defsystem #:coca
  :author ("凉凉")
  :license "LGPL"
  :version "0.0.0"
  :description "Cocoa in Common Lisp"
  :long-description
  ""
  :depends-on (:coca/objc)
  :components ())

(defsystem #:coca/objc
  :author ("凉凉")
  :license "LGPL"
  ;; ROADMAP:
  ;; + 0.0.1: can call ObjC method
  ;; + 0.0.2: can define ObjC class, can implement ObjC method
  ;; + 0.0.3: documentation, bug fix
  :version "0.0.0"
  :description "ObjC Runtime bridging in Common Lisp"
  :long-description
  "Coca/ObjC is a Common Lisp bridge to ObjC Runtime.

The API is defined like LispWorks' OBJC API. You could consider Coca/ObjC
as a reimplementation on portable CFFI. But the API may not exactly same as
LispWorks' API.

Types:

  ObjC Type     Lisp Wrapper   Type Encoding `objc-encoding'
  -----------------------------------------------------------
  Class         `objc-class'   :class
  SEL           `sel'          :sel
  id            ...            :object

The ObjC type should be annoted by `objc-encoding', which is basically CFFI
foreign type declaration with some add-ons.

Struct:
Use `define-objc-struct' would define foreign ObjC struct representation
(CFFI (:struct NAME)) and lisp representation (a struct type in lisp).

Invoke:
Use `invoke' to call ObjC method (objc_msgSend).
For example, calling [window close] is equal to calling (invoke window \"close\").
And calling [box setWidth:10 height:20] is equal to (invoke \"setWidth:height:\" 10 20).

Use `can-invoke-p' to test if method could be called.

Class:
Every ObjC class is instance of meta class `objc-class'.

Use `coerce-to-objc-class' to intern ObjC class as lisp class.

Memory Management:

"
  :depends-on (:str
               :cffi
               :cffi-libffi
               :trivial-garbage
               :closer-mop
               :trivial-main-thread)
  :pathname "objc"
  :components
  ((:file        "package"
    :description "Package definition of coca.objc")
   (:file        "utils"
    :depends-on ("package")
    :description "Helper functions of coca.objc"
    :long-description
    "These utils functions should be used to keep the code base clean and readable.

Cache:
+ `coca.objc::with-cached' provide cache ability on hash-table,
  should be used in `coerce-to-objc-class', `coerce-to-selector' or else

Atomize, Listfy:
+ `coca.objc::atomize' should be used to turn (:struct ns-rect) like object into ns-rect
+ `coca.objc::listfy' is kinda like inversed version of `coca.objc::atomize'

Alist and Plist:
+ `coca.objc::select-plist', `coca.objc::remove-plist'
  `coca.objc::select-alist', `coca.objc::remove-alist'
 provide functionalities to filter alist and plist
")
   (:file        "cffi"
    :depends-on ("package")
    :description "CFFI bindings to ObjC Runtime"
    :long-description
    "Only needed ObjC Runtime functions are imported.

By default, the framework Foundation and Cocoa is loaded.

The documentation of each ObjC runtime functions could be found in Apple's documentation.
https://developer.apple.com/documentation/objectivec/objective-c-runtime?language=objc

All the object related with ObjC environment should be wrapped by `coca.objc::objc-pointer'.
Use `objc-object-pointer' to get the foreign pointer of the `coca.objc::objc-pointer'. ")
   (:file        "encoding"
    :depends-on ("utils")
    :description "Parse and manage ObjC type encoding"
    :long-description
    "The type encoding representation in coca.objc is basically CFFI type description.

  Type Encoding               ObjC Type Encoding
  :char                       c
  :unsigned-char              C
  :int                        i
  :unsigned-int               I
  :short                      s
  :unsigned-short             S
  :long                       l
  :unsigned-long              L
  :long-long                  q
  :unsigned-long-long         Q
  :float                      f
  :double                     d
  :bool                       B
  :void                       v
  :string                     *
  :object                     @
  :class                      #
  :sel                        :
  (:array TYPE)               [TYPE]
  (:struct OBJC-STRUCT)       {OBJC-STRUCT-NAME=...}
  (:union  OBJC-UNION)        (OBJC-UNION-NAME=...)
  (:bits   NUM)               bNUM
  :pointer                    ^TypeEncoding
  :unknown                    ?

See `coca.objc::decode-objc-type-encoding' for parsing type encoding.
See `coca.objc::encode-objc-type-encoding' for generating type encoding.
See `coca.objc::

Dev Note:
Use `objc-encoding' type as ObjC type encoding annotation

TODO:
+ (:union OBJC-UNION)
+ (:array TYPE)
+ (:bits  NUM)
")
   (:file        "objc-class"
    :depends-on ("cffi")
    :description "ObjC Class wrapper in Lisp"
    :long-description
    "Every ObjC class in lisp should be instance of `objc-class'.

The `objc-class' is subclass of `standard-objc-object'.

Use type `standard-objc-object' to test if object is ObjC object. ")
   (:file        "objc-object"
    :depends-on ("objc-class")
    :description "Implement ObjC Object wrapper"
    :long-description
    "TODO: finalize? ")
   (:file        "sel"
    :depends-on ("cffi")
    :description "Wrap for ObjC SEL"
    :long-description
    "Use `coerce-to-selector' to get `sel' object as ObjC SEL. ")
   (:file        "struct"
    :depends-on ("encoding")
    :description "Trivial implementation of lisp struct to ObjC annotation "
    :long-description
    "
Use `define-objc-struct' to define a new lisp structure representation of ObjC structure.

Currently implemented ObjC structures are:

    ObjC Structure         Lisp Structure
    CGSize                 ns-size
    CGPoint                ns-point
    CGRect                 ns-rect

Note that internally `coca.objc' use `coca.objc::struct-aref' to support `simple-vector'
to be passed in as foreign ObjC structure, so

Dev Note:
Use `coca.objc::objc-struct-info' to store meta data to generate `cffi:foreign-funcall'
as ObjC structure.

TODO: better and cleaner implementation.
")
   (:file        "method"
    :depends-on ("objc-class"
                 "sel"
                 "encoding"
                 "struct")
    :description "Implement ObjC method wrapper"
    :long-description
    "
Use `coca.objc::objc-method-calling-lambda-form' to generate lambda calling form
from type encoding.

TODO: better and cleaner implementation. ")))

;;;; coca.asd ends here
