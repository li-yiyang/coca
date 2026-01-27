;;;; uniform-type-identifiers.lisp --- Uniform Type Identifiers Framework

(uiop:define-package #:coca.uniform-type-identifiers
  (:nicknames :coca.uti)
  (:use :common-lisp :coca.objc :coca.foundation)
  (:documentation
   "The Uniform Type Identifiers framework provides a collection of
common types that map to MIME and file types. Use these types in your
project to describe the file types in your app. These descriptions
help the system properly handle file storage formats or in-memory data
for transfer — for example, transferring data to or from the
pasteboard. The identifier types can also identify other resources,
such as directories, volumes, or packages.

Explicitly specify relationships between types by marking them as
subtypes of other types. For example, the type UTTypePNG has the
identifier public.png and is a subtype of UTTypeImage
(public.image). UTTypeImage is in turn a subtype of both of the
following:

+ UTTypeContent (public.content)
  which implies the type can be a document
+ UTTypeData (public.data)
  which implies the type is representable as a byte stream

see https://developer.apple.com/documentation/uniformtypeidentifiers?language=objc")
  (:export
   #:ut-type
   #:as-ut-type
   #:identifier
   #:perferred-filename-extension
   #:preferred-mime-type
   #:declared-p
   #:public-type-p))

(in-package :coca.uniform-type-identifiers)

(define-objc-class "UTType" ()
  (;;; Identifying a type
   ("identifier"
    :reader identifier
    :after  ns-string-to-string
    :documentation
    "The string that represents the type.
see https://developer.apple.com/documentation/uniformtypeidentifiers/uttypereference?language=objc")
   ;;; Obtaining tags
   ("preferredFilenameExtension"
    :reader preferred-filename-extension
    :after  ns-string-to-string
    :documentation
    "The preferred filename extension for the type.

If available, the preferred (first available) tag of class
filenameExtension.

Many types require the generation of a filename; for example, when
saving a file to disk. If not nil, the value of this property is the
best available filename extension for this type.

see https://developer.apple.com/documentation/uniformtypeidentifiers/uttypereference/preferredfilenameextension?language=objc")
   ("preferredMIMEType"
    :reader preferred-mime-type
    :after  ns-string-to-string
    :documentation
    "The preferred MIME type for the type.

If available, the preferred (first available) tag of class
mimeType. If not nil, the value of this property is the best available
MIME type value for this type.

see https://developer.apple.com/documentation/uniformtypeidentifiers/uttypereference/preferredmimetype?language=objc")
   ("declared"
    :reader declared-p
    :documentation
    "A Boolean value that indicates whether the system declares the type.
see https://developer.apple.com/documentation/uniformtypeidentifiers/uttypereference/isdeclared?language=objc")
   ("publicType"
    :reader public-type-p
    :documentation
    "A Boolean value that indicates whether the type is in the public domain.
see https://developer.apple.com/documentation/uniformtypeidentifiers/uttypereference/ispublic?language=objc"))
  (:documentation
   "An object that represents a type of data to load, send, or receive.

The UTType object may represent files on disk, abstract data types
with no on-disk representation, or entirely unrelated hierarchical
classification systems, such as hardware. Each instance has a unique
identifier, and helpful properties, preferredFilenameExtension and
preferredMIMEType.

Note
The system includes static declarations for many common types, which
you can look up by identifier, filename extension, or MIME type.

The UTType object may provide additional information related to the
type. For example, it may include a localized user-facing description,
a reference URL to technical documentation about the type, or its
version number. You can look up types by their conformance to get
either a type or a list of types that are relevant to your use case.

To define your own types in your app’s Info.plist, see Defining file
and data types for your app.

see https://developer.apple.com/documentation/uniformtypeidentifiers/uttypereference?language=objc"))

(defparameter *ut-types* (make-hash-table)
  "Cache of `ut-type'.

KEY: keyword of `ut-type'
VAL: `ut-type' instance")

(defgeneric as-ut-type (type &key &allow-other-keys)
  (:documentation "Create an `ut-type' for TYPE. ")
  (:method ((extension string) &key)
    (invoke 'ut-type "typeWithFilenameExtension:"
            (string-to-ns-string extension)))
  (:method ((extension symbol) &key)
    (coca.objc::with-cached (extension *ut-types*)
      (invoke 'ut-type "typeWithFilenameExtension:"
              (string-to-ns-string (symbol-name extension))))))

(let ((types ()))
  (define-coca-init :pre
    (setf types ())
    (maphash (lambda (key ut-type) (declare (ignore ut-type))
               (push key types))
             *ut-types*)
    (clrhash *ut-types*))

  (define-coca-init :post
    (dolist (type types)
      (setf (gethash type *ut-types*) (as-ut-type type)))))

;;;; uniform-type-identifiers.lisp ends here
