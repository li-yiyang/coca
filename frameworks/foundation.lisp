;;;; foundation.lisp --- ObjC bindings for Foundation Framework
;; Access essential data types, collections, and operating-system
;; services to define the base layer of functionality for your app.
;;
;; The Foundation framework provides a base layer of functionality for
;; apps and frameworks, including data storage and persistence, text
;; processing, date and time calculations, sorting and filtering, and
;; networking. The classes, protocols, and data types defined by
;; Foundation are used throughout the macOS, iOS, watchOS, and tvOS
;; SDKs.
;;
;; https://developer.apple.com/documentation/foundation?language=objc

(uiop:define-package #:coca.foundation
  (:use :cl :coca.objc)
  (:export
   #:ns-rect
   #:ns-rect-p
   #:make-ns-rect
   #:ns-rect-x
   #:ns-rect-y
   #:ns-rect-w
   #:ns-rect-h
   #:ns-point
   #:ns-point-p
   #:make-ns-point
   #:ns-point-x
   #:ns-point-y
   #:ns-size
   #:ns-size-p
   #:make-ns-size
   #:ns-size-x
   #:ns-size-y

   #:string-to-ns-string
   #:ns-string-to-string
   #:description


   ;; Object Runtime

   ;; Numbers, Data, and Basic Values

   ;; Strings and Text

   ;; Collections

   ;; Dates and Times

   ;; Units and Measurement

   ;; Data Formatting

   ;; Filters and Sorting

   ;; Task Management

   ;; Resources

   ;; Notifications

   ;; App Extension Support

   ;; Errors and Exceptions
   #:ns-error
   #:ns-assertion-handler
   #:ns-exception

   ;; Scripting Support

   ;; File System

   ;; Archives and Serialization

   ;; Settings

   ;; Spotlight

   ;; iCloud

   ;; URL Loading System

   ;; Bonjour

   ;; XPC

   ;; Object Runtime

   ;; Processes and Threads

   ;; Streams, Sockets, and Ports
   ))

(in-package :coca.foundation)


;;;; Object Runtime
;; see https://developer.apple.com/documentation/foundation/object-runtime?language=objc

(doc-objc-class "NSObject"              ; ns-object
  "The root class of most Objective-C class hierarchies,
from which subclasses inherit a basic interface to the runtime system and
the ability to behave as Objective-C objects."
  "Use `description' to print readable inspection string for `ns-object'.
See also `print-object' implementation for `ns-object'. "
  "See https://developer.apple.com/documentation/ObjectiveC/NSObject-swift.class?language=objc. ")

(defun description (ns-object)
  "Returns a string that represents the contents of the receiving class.

Dev Note:
Internally this is equal to calling (ns-string-to-string (invoke ns-object \"description\")).

See https://developer.apple.com/documentation/objectivec/nsobject-swift.class/description()?language=objc"
  (declare (type ns-object ns-object))
  (ns-string-to-string (invoke ns-object "description")))

(defmethod print-object ((obj ns-object) stream)
  (write-string (description obj) stream))


;;;; Numbers, Data, and Basic Values
;; see https://developer.apple.com/documentation/foundation/numbers-data-and-basic-values?language=objc

(define-objc-struct (ns-size "CGSize")
  (w :double)
  (h :double))

(define-objc-struct (ns-point "CGPoint")
  (x :double)
  (y :double))

(define-objc-struct (ns-rect "CGRect")
  (x :double :type real)
  (y :double :type real)
  (w :double :type real)
  (h :double :type real))


;;;; Strings and Text
;; see https://developer.apple.com/documentation/foundation/strings-and-text?language=objc

(doc-objc-class "NSString"              ; ns-string
  "A static, plain-text Unicode string object."
  "Use `ns-string-to-string' and `string-to-ns-string' to convert between
lisp string and `ns-string' objects. "
  "See: https://developer.apple.com/documentation/foundation/nsstring?language=objc")

(doc-objc-class "NSMutableString"       ; ns-mutable-string
  "A dynamic plain-text Unicode string object."
  "See: https://developer.apple.com/documentation/foundation/nsmutablestring?language=objc")

(defun string-to-ns-string (string)
  "Converts a Lisp string to an Objective-C NSString.
Return `ns-string' object.

Dev Note:
Internally use ObjC method stringWithUTF8String:.
See https://developer.apple.com/documentation/foundation/nsstring/init(utf8string:)-8bcy8?language=objc"
  (declare (type string string))
  (the ns-string (invoke 'ns-string "stringWithUTF8String:" string)))

(defun ns-string-to-string (ns-string)
  "Converts an Objective-C NSString to a Lisp string.
Return string.

Dev Note:
Internally use ObjC method UTF8String.
See https://developer.apple.com/documentation/foundation/nsstring/utf8string?language=objc"
  (declare (type ns-string ns-string))
  (the string (invoke ns-string "UTF8String")))

(defmacro invoke-into-string (object method &rest args)
  "Call `invoke' and return string.

Dev Note:
The original METHOD should return `ns-string' and it would be turned into lisp string. "
  `(ns-string-to-string (invoke ,object ,method ,@args)))


;;;; Collections
;; Use arrays, dictionaries, sets, and specialized collections to store and iterate groups of objects or values.
;; https://developer.apple.com/documentation/foundation/collections?language=objc


;;;; Dates and Times
;; Compare dates and times, and perform calendar and time zone calculations.
;; https://developer.apple.com/documentation/foundation/dates-and-times?language=objc


;;;; Units and Measurement
;; Label numeric quantities with physical dimensions to allow locale-aware formatting
;; and conversion between related units.
;; https://developer.apple.com/documentation/foundation/units-and-measurement?language=objc


;;;; Data Formatting
;; Convert numbers, dates, measurements, and other values to and from locale-aware string representations.
;; https://developer.apple.com/documentation/foundation/data-formatting?language=objc


;;;; Filters and Sorting
;; Use predicates, expressions, and sort descriptors to examine elements in collections and other services.
;; https://developer.apple.com/documentation/foundation/filters-and-sorting?language=objc


;;;; Task Management
;; Manage your app’s work and how it interacts with system services like Handoff and Shortcuts.
;; https://developer.apple.com/documentation/foundation/task-management?language=objc


;;;; Resources
;; Access assets and other data bundled with your app.
;; https://developer.apple.com/documentation/foundation/resources?language=objc


;;;; Notifications
;; Design patterns for broadcasting information and for subscribing to broadcasts.
;; see https://developer.apple.com/documentation/foundation/notifications?language=objc


;;;; App Extension Support
;; Manage the interaction between an app extension and its hosting app.
;; see https://developer.apple.com/documentation/foundation/app-extension-support?language=objc


;;;; Errors and Exceptions
;; Respond to problem situations in your interactions with APIs, and fine-tune your app for better debugging.
;; see https://developer.apple.com/documentation/foundation/errors-and-exceptions?language=objc

;;; User-Relevant Errors

(doc-objc-class "NSError"               ; ns-error
  "Information about an error condition including a domain, a domain-specific error code,
and application-specific information."
  "see https://developer.apple.com/documentation/foundation/errors-and-exceptions?language=objc")

;;; Assertions

(doc-objc-class "NSAssertionHandler"    ; ns-assertion-handler
  "An object that logs an assertion to the console. "
  "see https://developer.apple.com/documentation/foundation/nsassertionhandler?language=objc")

;;; Exceptions

(doc-objc-class "NSException"           ; ns-exception
  "An object that represents a special condition that interrupts the normal flow of program execution. "
  "see https://developer.apple.com/documentation/foundation/nsexception?language=objc")

(define-condition objc-exception (objc-error)
  ((name      :initarg :name)
   (reason    :initarg :reason)
   (exception :initarg :exception))
  (:documentation "Throw `ns-exception' as lisp condition. ")
  (:report (lambda (condition stream)
             (with-slots (name reason) condition
               (format stream "[~A] ~A" name reason)))))

(cffi:defcallback objc-uncaught-exception-handler :void ((ns-exception :pointer))
  "Throw `ns-exception' as `objc-exception'. "
  (let ((exception (coca.objc::coerce-to-objc-object ns-exception)))
    (error 'objc-exception :exception exception
                           :name      (invoke-into-string exception "name")
                           :reason    (invoke-into-string exception "reason"))))

;; Changes the top-level error handler.
(cffi:foreign-funcall "NSSetUncaughtExceptionHandler"
                      :pointer (cffi:get-callback 'objc-uncaught-exception-handler)
                      :void)

;;; Diagonstics and Debugging

(defun ns-log (msg &rest args)
  "Logs an error message to the Apple System Log facility.
Return the formatted message string.

Parameters:
+ MSG: `ns-string' or `string' as format control string

  the control string should be like lisp `format' control string
+ ARGS: arguments used to format the log infomations

Dev Note:
if returned format string contains `%', which is used as ObjC formatting
string, it would be escaped as `%%'. "
  (declare (type (or ns-string string) msg))
  (let* ((res      (apply #'format
                          nil
                          (etypecase msg
                            (ns-string (ns-string-to-string msg))
                            (string    msg))
                          args))
         (ns-string (string-to-ns-string (str:replace-all "%" "%%" res))))
    (cffi:foreign-funcall "NSLog" :pointer (objc-object-pointer ns-string) :void)
    res))


;;;; Scripting Support
;; Allow users to control your app with AppleScript and other automation technologies, or run scripts from within your app.
;; see https://developer.apple.com/documentation/foundation/scripting-support?language=objc


;;;; File System
;; Create, read, write, and examine files and folders in the file system.
;; see https://developer.apple.com/documentation/foundation/file-system?language=objc


;;;; Archives and Serialization
;; Convert objects and values to and from property list, JSON, and other flat binary representations.
;; see https://developer.apple.com/documentation/foundation/archives-and-serialization?language=objc


;;;; Settings
;; Configure your app using data you store persistently on the local disk or in iCloud.
;; see https://developer.apple.com/documentation/foundation/settings?language=objc


;;;; Spotlight
;; Search for files and other items on the local device, and index your app’s content for searching.
;; see https://developer.apple.com/documentation/foundation/spotlight?language=objc


;;;; iCloud
;; Manage files and key-value data that automatically synchronize among a user’s iCloud devices.
;; see https://developer.apple.com/documentation/foundation/icloud?language=objc


;;;; Optimizing Your App’s Data for iCloud Backup
;; Minimize the space and time that backups take to create by excluding purgeable and nonpurgeable data from backups.
;; see https://developer.apple.com/documentation/foundation/optimizing-your-app-s-data-for-icloud-backup?language=objc


;;;; URL Loading System
;; Interact with URLs and communicate with servers using standard Internet protocols.
;; see https://developer.apple.com/documentation/foundation/url-loading-system?language=objc


;;;; Bonjour
;; Advertise services for easy discovery on local networks, or discover services advertised by others.
;; see https://developer.apple.com/documentation/foundation/bonjour?language=objc


;;;; XPC
;; Manage secure interprocess communication.
;; see https://developer.apple.com/documentation/foundation/xpc?language=objc


;;;; Object Runtime
;; Get low-level support for basic Objective-C features, Cocoa design patterns, and Swift integration.
;; see https://developer.apple.com/documentation/foundation/object-runtime?language=objc


;;;; Processes and Threads
;; Manage your app’s interaction with the host operating system and other processes, and implement low-level concurrency features.
;; see https://developer.apple.com/documentation/foundation/processes-and-threads?language=objc


;;;; Streams, Sockets, and Ports
;; Use low-level Unix features to manage input and output among files, processes, and the network.
;; see https://developer.apple.com/documentation/foundation/streams-sockets-and-ports?language=objc


;;;; foundation.lisp ends here
