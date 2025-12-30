;;;; coca.asd --- Cocoa in Common Lisp

(defsystem #:coca
  :author ("凉凉")
  :license "LGPL"
  :version "0.0.0"
  :description "Cocoa in Common Lisp"
  :depends-on (:coca/objc
               :coca/frameworks)
  :components ((:file "package")))

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
LispWorks' API."
  :depends-on (:str
               :cffi
               :cffi-libffi
               :trivial-garbage
               :closer-mop
               :trivial-main-thread)
  :defsystem-depends-on (:coca-grovel)
  :pathname "objc"
  :components
  ((:file        "package"
    :description "Package definition of coca.objc")
   (:objc-file   "wrapper"
    :depends-on ("package"))
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
    :depends-on ("package"
                 "wrapper")
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

Use `coca.objc:define-objc-typedef' to define an alias for the ObjC encoding.

See `coca.objc::decode-objc-type-encoding' for parsing type encoding.
See `coca.objc::encode-objc-type-encoding' for generating type encoding.

Dev Note:
Use `objc-encoding' type as ObjC type encoding annotation.

Use `coca.objc::objc-encoding-cffi-type', `coca.objc::objc-encoding-lisp-type'
for representing ObjC encoding in CFFI and lisp side.

Use `coca.objc::as-objc-encoding' to normalize input.

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

Use type `standard-objc-object' to test if object is ObjC object.

Use `doc-objc-class' to descript the documentation and the property of
as lisp slot. ")
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
    --------------------------------------
    CGSize                 ns-size
    CGPoint                ns-point
    CGRect                 ns-rect

Note that internally `coca.objc' use `coca.objc::struct-aref' to support `simple-vector'
to be passed in as foreign ObjC structure, so

Dev Note:
Use `coca.objc::objc-struct-name' to get lisp struct name symbol from given
ObjC struct name string.

Use `coca.objc::objc-struct-info' to store meta data to generate `cffi:foreign-funcall'
as ObjC structure. The `coca.objc::objc-struct-info' is a struct with slots:

    Slot        Notes
    -----------------------------------------------------------
    name        ObjC struct name, used in type encoding parsing
    len         Number of struct slots
    slots       A list of struct slot accessor function
    types       A list of struct slot CFFI type

The `coca.objc::struct-aref' is used to provide slot picking functionalities.
See objc/method.lisp for how it is used to get the struct elements.

Use `coca.objc::case-struct-aref' for manual definition of `coca.objc::struct-aref'.

TODO: better and cleaner implementation.
")
   (:file        "method"
    :depends-on ("objc-class"
                 "sel"
                 "encoding"
                 "struct")
    :description "Implement ObjC method wrapper"
    :long-description
    "Use `invoke' to call ObjC method.

Dev Note:
Use `coca.objc::objc-method-calling-lambda-form' to generate lambda calling form
from type encoding.

The compiled lambda expression are cached by type encoding string in `coca.objc::*methods*'.

The instance methods and class methods are seperately stored under `objc-class'
`coca.objc::class-instance-method' and `coca.objc::class-class-method'.")
   (:file        "enum"
    :depends-on ("encoding")
    :description "Wrapper of ObjC enum"
    :long-description
    "Use `define-objc-enum' to define enum wrapper function. ")
   (:file        "const"
    :depends-on ("encoding")
    :description "Define ObjC global variable as lisp const. "
    :long-description
    "Use `define-objc-const' to define ObjC global const. ")))

(defsystem #:coca/frameworks
  :author ("凉凉")
  :license "LGPL"
  ;; Version Rules
  ;; ================
  ;; major.minor.patch
  ;; major: stands for stable frameworks binding number
  ;; minor: stands for unstable frameworks bindings
  ;; patch: bug fixes
  :version "0.3.0"
  :description "Frameworks binding in Common Lisp"
  :long-description
  "This will only loads commonly used frameworks.
For other frameworks, load them indivisually like (ql:load :coca/frameworks/<name>).

Dev Note:
The framework ObjC binding should be like:

    (uiop:define-package #:coca.<framework>
      (:use :cl :coca.objc ...)
      (:export ...))
    (in-package :coca.<framework>)

    (define-objc-enum ...)

    (doc-objc-class <ObjC class name> ...)
    ...

This process could be automatically generated by parsing the Apple's
documentation.  Use devtools/parse-apple-docs.js to parse Apple's
documentation and devtools/gen-framework-export.el to generate
exported symbols.

TODO: need a standard way to do the generation... And the generated
documentation should be refined for real usage..."
  :depends-on (:coca/objc
               :coca/frameworks/foundation
               :coca/frameworks/appkit))

(defsystem #:coca/frameworks/appkit
  :author ("凉凉")
  :license "LGPL"
  :description "ObjC bindings of AppKit Framework"
  :long-description
  "Construct and manage a graphical, event-driven user interface for your macOS app.

AppKit contains the objects you need to build the user interface for a macOS app.
In addition to drawing windows, buttons, panels, and text fields, it handles all the
event management and interaction between your app, people, and macOS.

see https://developer.apple.com/documentation/AppKit?language=objc"
  :pathname "frameworks"
  :components ((:file "appkit"))
  :depends-on (:coca/objc
               :coca/frameworks/foundation))

(defsystem #:coca/frameworks/foundation
  :author ("凉凉")
  :license "LGPL"
  :description "ObjC bindings of Foundation Framework"
  :long-description
  "Access essential data types, collections, and operating-system services to
define the base layer of functionality for your app.

The Foundation framework provides a base layer of functionality for apps and
frameworks, including data storage and persistence, text processing, date and
time calculations, sorting and filtering, and networking. The classes, protocols,
and data types defined by Foundation are used throughout the macOS, iOS,
watchOS, and tvOS SDKs.

see https://developer.apple.com/documentation/Foundation?language=objc"
  :pathname "frameworks"
  :components ((:file "foundation"))
  :depends-on (:coca/objc))

(defsystem #:coca/frameworks/webkit
  :author ("凉凉")
  :license "LGPL"
  :description "ObjC bindings of Webkit Framework"
  :long-description
  "Integrate web content seamlessly into your app,
and customize content interactions to meet your app's needs.

Use the WebKit framework to integrate richly styled web content into your
app's native content. WebKit offers a full browsing experience for your content,
offering a platform-native view and supporting classes to:

+ Display rich web content using HTML, CSS, and JavaScript
+ Handle the incremental loading of page content
+ Display multiple MIME types and compound frame elements
+ Navigate between pages of content
+ Manage a forward-back list of recently visited pages

For more information about WebKit, go to https://webkit.org.

see https://developer.apple.com/documentation/WebKit?language=objc"
  :pathname "frameworks"
  :components ((:file "webkit"))
  :depends-on (:coca/objc
               :coca/frameworks/foundation
               :coca/frameworks/appkit))

;;;; coca.asd ends here
