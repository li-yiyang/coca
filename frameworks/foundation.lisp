;;;; foundation.lisp --- ObjC bindings for Foundation Framework

(uiop:define-package #:coca.foundation
  (:use :cl :coca.objc)
  (:documentation
   "Access essential data types, collections, and operating-system
services to define the base layer of functionality for your app.

The Foundation framework provides a base layer of functionality for
apps and frameworks, including data storage and persistence, text
processing, date and time calculations, sorting and filtering, and
networking. The classes, protocols, and data types defined by
Foundation are used throughout the macOS, iOS, watchOS, and tvOS
SDKs.

https://developer.apple.com/documentation/foundation?language=objc")
  (:export
   ;; Object Runtime
   #:ns-object
   #:copy
   #:init
   #:description
   #:ns-uinteger
   #:ns-integer
   #:ns-number
   #:ns-value
   #:ns-value-transformer
   #:ns-invocation
   #:ns-method-signature
   #:ns-proxy
   #:ns-autorelease-pool
   #:release
   #:drain
   #:retain
   #:retain-count
   #:autorelease
   #:alloc-init
   #:alloc
   #:make-autorelease-pool
   #:with-autorelease-pool
   #:+ns-foundation-version-number+

   ;; Numbers, Data, and Basic Values
   #:ns-decimal-number
   #:ns-number
   #:ns-number-formatter
   #:ns-data
   #:ns-mutable-data
   #:ns-url
   #:ns-url-components
   #:ns-url-query-item
   #:ns-uuid
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
   #:ns-affine-transform

   ;; Strings and Text
   #:ns-string
   #:ns-mutable-string
   #:string-to-ns-string
   #:ns-string-to-string
   #:invoke-into-string

   ;; Collections
   #:ns-array
   #:ns-mutable-array
   #:ns-dictionary
   #:ns-mutable-dictionary
   #:ns-set
   #:ns-mutable-set
   #:ns-index-path
   #:ns-index-set
   #:ns-mutable-index-set
   #:ns-counted-set
   #:ns-ordered-set
   #:ns-mutable-ordered-set
   #:ns-cache
   #:ns-purgeable-data
   #:ns-pointer-array
   #:ns-map-table
   #:ns-hash-table
   #:ns-enumerator
   #:ns-null

   ;; Dates and Times
   #:ns-date
   #:ns-time-interval
   #:ns-date-since
   #:ns-date-now
   #:ns-date-distant-future
   #:ns-date-distant-past
   #:ns-date-components
   #:ns-calendar
   #:ns-time-zone
   #:ns-date-formatter
   #:ns-date-components-formatter
   #:ns-date-interval-formatter
   #:ns-locale
   #:ns-calendar-date

   ;; Units and Measurement
   #:ns-measurement
   #:ns-unit
   #:ns-dimension
   #:ns-unit-converter
   #:ns-unit-converter-linear
   #:ns-unit-area
   #:ns-unit-length
   #:ns-unit-volume
   #:ns-unit-angle
   #:ns-unit-mass
   #:ns-unit-pressure
   #:ns-unit-acceleration
   #:ns-unit-duration
   #:ns-unit-frequency
   #:ns-unit-speed
   #:ns-unit-energy
   #:ns-unit-power
   #:ns-unit-temperature
   #:ns-unit-illuminance
   #:ns-unit-electric-charge
   #:ns-unit-electric-current
   #:ns-unit-electric-potential-difference
   #:ns-unit-electric-resistance
   #:ns-unit-concentration-mass
   #:ns-unit-dispersion
   #:ns-unit-fuel-efficiency
   #:ns-unit-information-storage

   ;; Data Formatting
   #:ns-number-formatter
   #:ns-person-name-components
   #:ns-date-formatter
   #:ns-date-components-formatter
   #:ns-relative-date-time-formatter
   #:ns-date-interval-formatter
   #:ns-byte-count-formatter
   #:ns-measurement-formatter
   #:ns-list-formatter
   #:ns-locale
   #:ns-formatter
   #:ns-inflection-rule
   #:ns-inflection-rule-explicit
   #:ns-morphology
   #:ns-term-of-address
   #:ns-morphology-pronoun
   #:ns-length-formatter
   #:ns-mass-formatter
   #:ns-energy-formatter

   ;; Filters and Sorting
   #:ns-predicate
   #:ns-expression
   #:ns-comparison-predicate
   #:ns-compound-predicate
   #:ns-sort-descriptor
   #:ns-ordered
   #:ns-ordered-p
   #:decode-ns-ordered

   ;; Task Management
   #:ns-undo-manager
   #:ns-progress
   #:ns-operation
   #:ns-operation-queue
   #:ns-block-operation
   #:ns-invocation-operation
   #:ns-timer
   #:coca
   #:ns-process-info
   #:ns-background-activity-scheduler
   #:ns-user-notification
   #:ns-user-notification-action
   #:ns-user-notification-center

   ;; Resources
   #:ns-bundle
   #:ns-bundle-resource-request

   ;; Notifications
   #:ns-notification
   #:ns-notification-center
   #:ns-notification-queue
   #:ns-distributed-notification-center

   ;; App Extension Support
   #:ns-extension-context
   #:ns-item-provider
   #:ns-extension-item
   #:coca

   ;; Errors and Exceptions
   #:ns-error
   #:ns-assertion-handler
   #:ns-exception
   #:objc-exception
   #:ns-log

   ;; Scripting Support
   #:ns-apple-script
   #:ns-apple-event-descriptor
   #:ns-apple-event-manager
   #:ns-script-command
   #:ns-quit-command
   #:ns-set-command
   #:ns-move-command
   #:ns-create-command
   #:ns-delete-command
   #:ns-exists-command
   #:ns-get-command
   #:ns-clone-command
   #:ns-count-command
   #:ns-close-command
   #:ns-script-object-specifier
   #:ns-property-specifier
   #:ns-positional-specifier
   #:ns-random-specifier
   #:ns-range-specifier
   #:ns-unique-id-specifier
   #:ns-whose-specifier
   #:ns-name-specifier
   #:ns-middle-specifier
   #:ns-index-specifier
   #:ns-relative-specifier
   #:ns-script-suite-registry
   #:ns-script-class-description
   #:ns-class-description
   #:ns-script-command-description
   #:ns-script-whose-test
   #:ns-specifier-test
   #:ns-logical-test
   #:ns-script-coercion-handler
   #:ns-script-execution-context

   ;; File System
   #:ns-file-manager
   #:ns-file-access-intent
   #:ns-file-coordinator
   #:ns-file-handle
   #:ns-file-security
   #:ns-file-version
   #:ns-file-wrapper

   ;; Archives and Serialization
   #:ns-json-serialization
   #:ns-property-list-serialization
   #:ns-keyed-archiver
   #:ns-keyed-unarchiver
   #:ns-coder
   #:ns-secure-unarchive-from-data-transformer
   #:ns-archiver
   #:ns-unarchiver

   ;; Settings
   #:ns-user-defaults
   #:ns-ubiquitous-key-value-store

   ;; Spotlight
   #:ns-metadata-query
   #:ns-metadata-item

   ;; iCloud
   #:ns-file-manager
   #:ns-ubiquitous-key-value-store
   #:ns-metadata-query
   #:ns-metadata-item

   ;; URL Loading System
   #:ns-url-session
   #:ns-url-session-task
   #:ns-url-request
   #:ns-mutable-url-request
   #:ns-url-response
   #:ns-httpurl-response
   #:ns-cached-url-response
   #:ns-url-cache
   #:ns-url-authentication-challenge
   #:ns-url-credential
   #:ns-url-credential-storage
   #:ns-url-protection-space
   #:ns-http-cookie
   #:ns-http-cookie-storage

   ;; Bonjour
   #:ns-net-service
   #:ns-net-service-browser

   ;; XPC
   #:ns-xpc-connection
   #:ns-xpc-interface
   #:ns-xpc-coder
   #:ns-xpc-listener
   #:ns-xpc-listener-endpoint

   ;; Processes and Threads
   #:ns-run-loop
   #:ns-run-loop-mode
   #:+ns-run-loop-common-modes+
   #:+ns-default-run-loop-mode+
   #:ns-timer
   #:ns-process-info
   #:ns-thread
   #:ns-lock
   #:ns-recursive-lock
   #:ns-distributed-lock
   #:ns-condition-lock
   #:ns-condition
   #:ns-operation-queue
   #:ns-operation
   #:ns-block-operation
   #:ns-invocation-operation
   #:ns-task
   #:ns-user-script-task
   #:ns-user-apple-script-task
   #:ns-user-automator-task
   #:ns-user-unix-task

   ;; Streams, Sockets, and Ports
   #:ns-stream
   #:ns-input-stream
   #:ns-output-stream
   #:ns-task
   #:ns-pipe
   #:ns-host
   #:ns-port
   #:ns-socket-port
   ))

(in-package :coca.foundation)


;;;; Object Runtime
;; Get low-level support for basic Objective-C features, Cocoa design patterns, and Swift integration.
;; see https://developer.apple.com/documentation/foundation/object-runtime?language=objc

;;; Object Basics

(define-objc-class "NSObject" ()              ; ns-object
  (("description"
    :reader description
    :after  ns-string-to-string
    :documentation
    "Returns a string that represents the contents of the receiving class.

Dev Note:
Internally this is equal to calling (ns-string-to-string (invoke ns-object \"description\")).
See https://developer.apple.com/documentation/objectivec/nsobject-swift.class/description()?language=objc"))
  (:documentation
   "The root class of most Objective-C class hierarchies,
from which subclasses inherit a basic interface to the runtime system and
the ability to behave as Objective-C objects.

Use `description' to print readable inspection string for `ns-object'.
See also `print-object' implementation for `ns-object'.

See https://developer.apple.com/documentation/ObjectiveC/NSObject-swift.class?language=objc. "))

(defun copy (ns-object)
  "Returns the object returned by copy(with:).

see https://developer.apple.com/documentation/objectivec/nsobject-swift.class/copy()?language=objc"
  (declare (type ns-object ns-object))
  (invoke ns-object "copy"))

(defmethod print-object ((obj ns-object) stream)
  "When printing `ns-object', print `description' of OBJ. "
  (write-string (description obj) stream))

(defmethod initialize-instance :after ((object ns-object) &key)
  "Allocate OBJECT pointer if OBJC-OBJECT-POINTER is unbound. "
  (unless (slot-boundp object 'objc-object-pointer)
    (let* ((tmp (invoke (class-of object) "alloc"))
           (ptr (objc-object-pointer tmp)))
      (setf (slot-value object 'objc-object-pointer)                       ptr
            (gethash (cffi:pointer-address ptr) coca.objc::*objc-objects*) object))))

(defgeneric init (object &key &allow-other-keys)
  (:documentation
   "Initialize OBJECT in ObjC environment.
Return OBJECT itself. ")
  (:method :around ((object ns-object) &key)
    (call-next-method)
    object)
  (:method ((object ns-object) &key)
    "By default, initialize ObjC by invoke ObjC method init. "
    (invoke object "init")))

;;; Copying

;;; Value Wrappers and Transformations

(define-objc-typedef ns-integer :long
  "Describes an integer.
see https://developer.apple.com/documentation/ObjectiveC/NSInteger?language=objc")

(define-objc-typedef ns-uinteger :unsigned-long
  "Describes an unsigned integer.
see https://developer.apple.com/documentation/objectivec/nsuinteger?language=objc")

(define-objc-struct (ca-transform-3d "CATransform3D")
  (m11 :double)
  (m12 :double)
  (m13 :double)
  (m14 :double)
  (m21 :double)
  (m22 :double)
  (m23 :double)
  (m24 :double)
  (m31 :double)
  (m32 :double)
  (m33 :double)
  (m34 :double)
  (m41 :double)
  (m42 :double)
  (m43 :double)
  (m44 :double))

(define-objc-struct (cg-affine-transform "CGAffineTransform")
  (a  :double)
  (b  :double)
  (c  :double)
  (d  :double)
  (tx :double)
  (ty :double))

(define-objc-struct (ns-range "_NSRange")
  (location :unsigned-long)
  (length   :unsigned-long))

(define-objc-class "NSValue" ()               ; ns-value
  ()
  (:documentation
   "A simple container for a single C or Objective-C data item.

see https://developer.apple.com/documentation/foundation/nsvalue?language=objc"))

(define-objc-class "NSValueTransformer" ()    ; ns-value-transformer
  ()
  (:documentation
   "An abstract class used to transform values from one representation to another.

see https://developer.apple.com/documentation/foundation/valuetransformer?language=objc"))

;;; Swift Support

;;; Invocations

(define-objc-class "NSInvocation" ()
  ()
  (:documentation
   "An Objective-C message rendered as an object.
see https://developer.apple.com/documentation/foundation/nsinvocation?language=objc"))

(define-objc-class "NSMethodSignature" ()
  ()
  (:documentation
   "A record of the type information for the return value and parameters of a method.
see https://developer.apple.com/documentation/foundation/nsmethodsignature?language=objc"))

;;; Remote Objects

(define-objc-class "NSProxy" ()
  ()
  (:documentation
   "An abstract superclass defining an API for objects that act as stand-ins for
other objects or for objects that don’t exist yet.
see https://developer.apple.com/documentation/foundation/nsproxy?language=objc"))

;;; Memory Management

(define-objc-class "NSAutoreleasePool" ()
  ()
  (:documentation
   "An object that supports Cocoa’s reference-counted memory management system.
An autorelease pool stores objects that are sent a release message when
the pool itself is drained.

Important:
If you use Automatic Reference Counting (ARC), you cannot use
autorelease pools directly. Instead, you use @autoreleasepool
blocks. For example, in place of:

    (let ((pool (make-autorelease-pool)))
      (unwind-protect (progn ...)
        (release pool)))

you would write:


    (with-autorelease-pool ()
      )

`with-autorelease-pool' blocks are more efficient than using an
instance of `ns-autorelease-pool' directly; you can also use them
even if you do not use ARC.

In a reference-counted environment (as opposed to one which uses
garbage collection), an NSAutoreleasePool object contains objects that
have received an autorelease message and when drained it sends a
release message to each of those objects. Thus, sending autorelease
instead of release to an object extends the lifetime of that object at
least until the pool itself is drained (it may be longer if the object
is subsequently retained). An object can be put into the same pool
several times, in which case it receives a release message for each
time it was put into the pool.

In a reference counted environment, Cocoa expects there to be an
autorelease pool always available. If a pool is not available,
autoreleased objects do not get released and you leak memory. In this
situation, your program will typically log suitable warning messages.

The Application Kit creates an autorelease pool on the main thread at
the beginning of every cycle of the event loop, and drains it at the
end, thereby releasing any autoreleased objects generated while
processing an event. If you use the Application Kit, you therefore
typically don’t have to create your own pools. If your application
creates a lot of temporary autoreleased objects within the event loop,
however, it may be beneficial to create “local” autorelease pools to
help to minimize the peak memory footprint.

You create an NSAutoreleasePool object with the usual alloc and init
messages and dispose of it with drain (or release—to understand the
difference, see Garbage Collection). Since you cannot retain an
autorelease pool (or autorelease it—see retain and autorelease),
draining a pool ultimately has the effect of deallocating it. You
should always drain an autorelease pool in the same context
(invocation of a method or function, or body of a loop) that it was
created. See Using Autorelease Pool Blocks for more details.

Each thread (including the main thread) maintains its own stack of
NSAutoreleasePool objects (see Threads). As new pools are created,
they get added to the top of the stack. When pools are deallocated,
they are removed from the stack. Autoreleased objects are placed into
the top autorelease pool for the current thread. When a thread
terminates, it automatically drains all of the autorelease pools
associated with itself.
Threads
=================
If you are making Cocoa calls outside of the Application Kit’s main
thread—for example if you create a Foundation-only application or if
you detach a thread—you need to create your own autorelease pool.  If
your application or thread is long-lived and potentially generates a
lot of autoreleased objects, you should periodically drain and create
autorelease pools (like the Application Kit does on the main thread);
otherwise, autoreleased objects accumulate and your memory footprint
grows. If, however, your detached thread does not make Cocoa calls,
you do not need to create an autorelease pool.

Note:
=================
If you are creating secondary threads using the POSIX thread APIs
instead of NSThread objects, you cannot use Cocoa, including
NSAutoreleasePool, unless Cocoa is in multithreading mode. Cocoa
enters multithreading mode only after detaching its first NSThread
object. To use Cocoa on secondary POSIX threads, your application must
first detach at least one NSThread object, which can immediately
exit. You can test whether Cocoa is in multithreading mode with the
NSThread class method isMultiThreaded.
Garbage Collection
====================
In a garbage-collected environment, there is no need for autorelease
pools. You may, however, write a framework that is designed to work in
both a garbage-collected and reference-counted environment. In this
case, you can use autorelease pools to hint to the collector that
collection may be appropriate. In a garbage-collected environment,
sending a drain message to a pool triggers garbage collection if
necessary; release, however, is a no-op. In a reference-counted
environment, drain has the same effect as release. Typically,
therefore, you should use drain instead of release.

see https://developer.apple.com/documentation/foundation/nsautoreleasepool?language=objc"))

(defun release (object)
  "Decrements the OBJECT reference count.

The receiver is sent a dealloc message when its reference count reaches 0.

see https://developer.apple.com/documentation/objectivec/nsobject-c.protocol/retain?language=objc"
  (declare (type ns-object object))
  (invoke object "release"))

(defun drain (object)
  "In a reference-counted environment, releases and pops the receiver;
in a garbage-collected environment, triggers garbage collection if the
memory allocated since the last collection is greater than the current
threshold.

In a reference-counted environment, this method behaves the same as
`release'. Since an autorelease pool cannot be retained (see
`ns-autorelease-pool'), this therefore causes the receiver to be
deallocated. When an autorelease pool is deallocated, it sends a
release message to all its autoreleased objects. If an object is added
several times to the same pool, when the pool is deallocated it
receives a release message for each time it was added.

Special Considerations
=========================
In a garbage-collected environment, `release' is a no-op, so unless
you do not want to give the collector a hint it is important to use
drain in any code that may be compiled for a garbage-collected
environment.

see https://developer.apple.com/documentation/foundation/nsautoreleasepool/drain?language=objc"
  (declare (type ns-object object))
  (invoke object "drain"))

(defun retain (object)
  "Increments the OBJECT reference count
Return OBJECT self.

You send an object a retain message when you want to prevent it from being
deallocated until you have finished using it.

An object is deallocated automatically when its reference count
reaches 0. retain messages increment the reference count, and release
messages decrement it. For more information on this mechanism, see
Advanced Memory Management Programming Guide.
see https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/MemoryMgmt/Articles/MemoryMgmt.html#//apple_ref/doc/uid/10000011i

As a convenience, retain returns self because it may be used in nested
expressions.

You would implement this method only if you were defining your own
reference-counting scheme. Such implementations must return self and
should not invoke the inherited method by sending a retain message to
super.

see https://developer.apple.com/documentation/objectivec/nsobject-c.protocol/retain?language=objc"
  (declare (type ns-object object))
  (invoke object "retain"))

(defun retain-count (object)
  "Invokes the Objective-C retainCount method.
Return reference count of OBJECT. "
  (invoke object "retainCount"))

(defun autorelease (object)
  "Decrements the OBJECT retain count at the end of the current autorelease pool block.
Return OBJECT self. "
  (declare (type ns-object object))
  (invoke object "autorelease"))

(defun alloc (class)
  "Returns a new instance of the receiving CLASS.

Dev Note:
Use `alloc-init' if do not need custom initialization process.

see https://developer.apple.com/documentation/objectivec/nsobject-swift.class/alloc?language=objc"
  (declare (type (or symbol string objc-class) class))
  (the ns-object (invoke class "alloc")))

(defun alloc-init (class &rest keys &key &allow-other-keys)
  "Invoke [[CLASS alloc] init];

This is equal to calling:

    (init (invoke CLASS \"alloc\") . KEYS)

see also `alloc'. "
  (declare (type (or symbol string objc-class) class))
  (the ns-object (apply #'init (invoke class "alloc") keys)))

(defun make-autorelease-pool ()
  "Makes an autorelease pool for the current thread.
Return `ns-autorelease-pool' instance.

Example usage:

    (let ((pool (make-autorelease-pool)))
      (unwind-protect (progn ...)
        (release pool)))

See also `with-autorelease-pool'. "
  (alloc-init 'ns-autorelease-pool))

(defmacro with-autorelease-pool (() &body body)
  "Just like `@autoreleasepool' blocks in ObjC. "
  (let ((pool (gensym "POOL")))
    `(let ((,pool (coca.objc::objc_autoreleasePoolPush)))
       (unwind-protect (progn ,@body)
         (coca.objc::objc_autoreleasePoolPop ,pool)))))

;;; Objective-C Runtime

;;; Versions and API Availability

(define-objc-const +ns-foundation-version-number+
    ("NSFoundationVersionNumber" :double coca.objc::foundation)
  "The version of the Foundation framework in the current environment. ")


;;;; Numbers, Data, and Basic Values
;; see https://developer.apple.com/documentation/foundation/numbers-data-and-basic-values?language=objc

;;; Numbers

;; A structure representing a base-10 number.
;; see https://developer.apple.com/documentation/foundation/decimal?language=objc
;; (define-objc-struct (ns-decimal "NSDecimal")
;;   ;;; ...
;;   )

(define-objc-class "NSDecimalNumber" ()
  ()
  (:documentation
   "An object for representing and performing arithmetic on base-10 numbers.
see https://developer.apple.com/documentation/foundation/nsdecimalnumber?language=objc"))

(define-objc-class "NSNumber" ()
  ()
  (:documentation
   "An object wrapper for primitive scalar numeric values.
see https://developer.apple.com/documentation/foundation/nsnumber?language=objc"))

(define-objc-class "NSNumberFormatter" ()
  ()
  (:documentation
   "A formatter that converts between numeric values and their textual representations.
see https://developer.apple.com/documentation/foundation/numberformatter?language=objc"))

;;; Binary Data

(define-objc-class "NSData" ()
  ()
  (:documentation
   "A static byte buffer in memory.
see https://developer.apple.com/documentation/foundation/nsdata?language=objc"))

(define-objc-class "NSMutableData" ()
  ()
  (:documentation
   "An object representing a dynamic byte buffer in memory.
see https://developer.apple.com/documentation/foundation/nsmutabledata?language=objc"))

;;; URLs

(define-objc-class "NSURL" ()
  ()
  (:documentation
   "An object that represents the location of a resource, such as an item on a
remote server or the path to a local file.
see https://developer.apple.com/documentation/foundation/nsurl?language=objc"))

(define-objc-class "NSURLComponents" ()
  ()
  (:documentation
   "An object that parses URLs into and constructs URLs from their constituent parts.
see https://developer.apple.com/documentation/foundation/nsurlcomponents?language=objc"))

(define-objc-class "NSURLQueryItem" ()
  ()
  (:documentation
   "An object representing a single name/value pair for an item in the query portion of a URL.
see https://developer.apple.com/documentation/foundation/nsurlqueryitem?language=objc"))

;;; Unique Identifiers

(define-objc-class "NSUUID" ()
  ()
  (:documentation
   "A universally unique value that can be used to identify types, interfaces, and other items.
UUIDs (Universally Unique Identifiers), also known as GUIDs
(Globally Unique Identifiers) or IIDs (Interface Identifiers), are
128-bit values. UUIDs created by NSUUID conform to RFC 4122 version
4 and are created with random bytes.

The standard format for UUIDs represented in ASCII is a string
punctuated by hyphens, for example
68753A44-4D6F-1226-9C60-0050E4C00067. The hex representation looks, as
you might expect, like a list of numerical values preceded by 0x. For
example, 0xD7, 0x36, 0x95, 0x0A, 0x4D, 0x6E, 0x12, 0x26, 0x80, 0x3A,
0x00, 0x50, 0xE4, 0xC0, 0x00, 0x67. Because a UUID is expressed simply
as an array of bytes, there are no endianness considerations for
different platforms.

The NSUUID class is not toll-free bridged with CoreFoundation’s
CFUUIDRef. Use UUID strings to convert between CFUUIDRef and NSUUID,
if needed. Two NSUUID objects are not guaranteed to be comparable by
pointer value (as CFUUIDRef is); use isEqual: to compare two NSUUID
instances.

see https://developer.apple.com/documentation/foundation/nsuuid?language=objc"))

;;; Geometry

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

(define-objc-struct (cps-process-serial-number "CPSProcessSerNum")
  (hi :unsigned-int)
  (lo :unsigned-int))


(define-objc-class "NSAffineTransform" ()
  ()
  (:documentation
   "A graphics coordinate transformation.
see https://developer.apple.com/documentation/foundation/nsaffinetransform?language=objc"))

;;; Ranges

;; A structure used to describe a portion of a series,
;; such as characters in a string or objects in an array.
;; see https://developer.apple.com/documentation/foundation/nsrange-c.struct?language=objc


;;;; Strings and Text
;; see https://developer.apple.com/documentation/foundation/strings-and-text?language=objc

(define-objc-class "NSString" ()
  ()
  (:documentation
   "A static, plain-text Unicode string object.
Use `ns-string-to-string' and `string-to-ns-string' to convert between
lisp string and `ns-string' objects.
See: https://developer.apple.com/documentation/foundation/nsstring?language=objc"))

(define-objc-class "NSMutableString" ()
  ()
  (:documentation
   "A dynamic plain-text Unicode string object.
See: https://developer.apple.com/documentation/foundation/nsmutablestring?language=objc"))

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

(defmethod objc-object-pointer ((string string))
  "Convert STRING to `ns-string' first and then pass the pointer. "
  (objc-object-pointer (string-to-ns-string string)))

(defmacro invoke-into-string (object method &rest args)
  "Call `invoke' and return string.

Dev Note:
The original METHOD should return `ns-string' and it would be turned into lisp string. "
  `(ns-string-to-string (invoke ,object ,method ,@args)))


;;;; Collections
;; Use arrays, dictionaries, sets, and specialized collections to store and iterate groups of objects or values.
;; https://developer.apple.com/documentation/foundation/collections?language=objc

;;; Basic Collections

(define-objc-class "NSArray" ()
  ()
  (:documentation
   "A static ordered collection of objects.
see https://developer.apple.com/documentation/foundation/nsarray?language=objc"))

(define-objc-class "NSMutableArray" ()
  ()
  (:documentation
   "A dynamic ordered collection of objects.
see https://developer.apple.com/documentation/foundation/nsmutablearray?language=objc"))

(define-objc-class "NSDictionary" ()
  ()
  (:documentation
   "A static collection of objects associated with unique keys.
see https://developer.apple.com/documentation/foundation/nsdictionary?language=objc"))

(define-objc-class "NSMutableDictionary" ()
  ()
  (:documentation
   "A dynamic collection of objects associated with unique keys.
see https://developer.apple.com/documentation/foundation/nsmutabledictionary?language=objc"))

(define-objc-class "NSSet" ()
  ()
  (:documentation
   "A static, unordered collection of unique objects.
see https://developer.apple.com/documentation/foundation/nsset?language=objc"))

(define-objc-class "NSMutableSet" ()
  ()
  (:documentation
   "A dynamic unordered collection of unique objects.
see https://developer.apple.com/documentation/foundation/nsmutableset?language=objc"))

;;; Indexes

(define-objc-class "NSIndexPath" ()
  ()
  (:documentation
   "A list of indexes that together represent the path to a specific location in a tree of nested arrays.
see https://developer.apple.com/documentation/foundation/nsindexpath?language=objc"))

(define-objc-class "NSIndexSet" ()
  ()
  (:documentation
   "An immutable collection of unique integer values that represent indexes in another collection.
see https://developer.apple.com/documentation/foundation/nsindexset?language=objc"))

(define-objc-class "NSMutableIndexSet" ()
  ()
  (:documentation
   "A mutable collection of unique integer values that represent indexes in another collection.
see https://developer.apple.com/documentation/foundation/nsmutableindexset?language=objc"))

;;; Specialized Sets

(define-objc-class "NSCountedSet" ()
  ()
  (:documentation
   "A mutable, unordered collection of distinct objects that may appear more than once in the collection.
see https://developer.apple.com/documentation/foundation/nscountedset?language=objc"))

(define-objc-class "NSOrderedSet" ()
  ()
  (:documentation
   "A static, ordered collection of unique objects.
see https://developer.apple.com/documentation/foundation/nsorderedset?language=objc"))

(define-objc-class "NSMutableOrderedSet" ()
  ()
  (:documentation
   "A dynamic, ordered collection of unique objects.
see https://developer.apple.com/documentation/foundation/nsmutableorderedset?language=objc"))

;;; Purgeable Collections

(define-objc-class "NSCache" ()
  ()
  (:documentation
   "A mutable collection you use to temporarily store transient key-value pairs
that are subject to eviction when resources are low.
see https://developer.apple.com/documentation/foundation/nscache?language=objc"))

(define-objc-class "NSPurgeableData" ()
  ()
  (:documentation
   "A mutable data object containing bytes that can be discarded when they’re no longer needed.
see https://developer.apple.com/documentation/foundation/nspurgeabledata?language=objc"))

;;; Pointer Collections

(define-objc-class "NSPointerArray" ()
  ()
  (:documentation
   "A collection similar to an array, but with a broader range of available memory semantics.
see https://developer.apple.com/documentation/foundation/nspointerarray?language=objc"))

(define-objc-class "NSMapTable" ()
  ()
  (:documentation
   "A collection similar to a dictionary, but with a broader range of available memory semantics.
see https://developer.apple.com/documentation/foundation/nsmaptable?language=objc"))

(define-objc-class "NSHashTable" ()
  ()
  (:documentation
   "A collection similar to a set, but with broader range of available memory semantics.
see https://developer.apple.com/documentation/foundation/nshashtable?language=objc"))

;;; Iteration

(define-objc-class "NSEnumerator" ()
  ()
  (:documentation
   "An abstract class whose subclasses enumerate collections of objects, such as arrays and dictionaries.
see https://developer.apple.com/documentation/foundation/nsenumerator?language=objc"))

;;; Special Semantic Values

(define-objc-class "NSNull" ()
  ()
  (:documentation
   "A singleton object used to represent null values in collection objects that don’t allow nil values.
see https://developer.apple.com/documentation/foundation/nsnull?language=objc"))

;; (define-objc-const +ns-not-found+
;;     ("NSNotFound" :long coca.objc::foundation)
;;   "A value indicating that a requested item couldn’t be found or doesn’t exist.
;; see https://developer.apple.com/documentation/foundation/nsnotfound-4qp9h?language=objc")



;;;; Dates and Times
;; Compare dates and times, and perform calendar and time zone calculations.
;; https://developer.apple.com/documentation/foundation/dates-and-times?language=objc


;;; Date Representations

(define-objc-class "NSDate" ()
  ()
  (:documentation
   "A representation of a specific point in time, independent of any calendar or time zone.
To Create `ns-date'
==========================
+ `ns-time-interval'
+ `ns-date-since'
+ `ns-date-distant-past'
+ `ns-date-distant-future'
see https://developer.apple.com/documentation/foundation/nsdate?language=objc"))

;; Creating Date

(deftype ns-time-interval ()
  "A number of seconds.

A NSTimeInterval value is always specified in seconds; it yields
sub-millisecond precision over a range of 10,000 years.

On its own, a time interval does not specify a unique point in time,
or even a span between specific times. Combining a time interval with
one or more known reference points yields a Date or DateInterval
value.

see https://developer.apple.com/documentation/foundation/timeinterval?language=objc"
  'double-float)

(defun ns-date-since (interval &optional (since :now))
  "Creates and returns a date since INTERVAL seconds from SINCE.
Return a `ns-date' object.

Parameters:
+ INTERVAL:
+ SINCE:
  + :now       dateWithTimeIntervalSinceNow:
  + :1970      dateWithTimeIntervalSince1970:
  + :2001      dateWithTimeIntervalSinceReferenceDate:
  + `ns-date'  dateWithTimeInterval:sinceDate:
"
  (declare (type real interval)
           (type (or (member :now :1970 :2001) ns-date) since))
  (let ((interval (coerce interval 'ns-time-interval)))
    (case since
      (:now  (invoke 'ns-date "dateWithTimeIntervalSinceNow:"           interval))
      (:1970 (invoke 'ns-date "dateWithTimeIntervalSince1970:"          interval))
      (:2001 (invoke 'ns-date "dateWithTimeIntervalSinceReferenceDate:" interval))
      (otherwise
       (invoke 'ns-date "dateWithTimeInterval:sinceDate:" interval (the ns-date since))))))

(defun ns-date-now ()
  "The current date and time, as of the time of access. "
  (alloc-init 'ns-date))

;; Getting Temporal Boundaries

(defun ns-date-distant-past ()
  "A date object representing a date in the distant past.
Return an `ns-date' object representing a date in the distant past
(in terms of centuries).

You can use this object as a control date, a guaranteed temporal boundary.

see https://developer.apple.com/documentation/foundation/nsdate/distantpast?language=objc"
  (invoke 'ns-date "distantPast"))

(defun ns-date-distant-future ()
  "A date object representing a date in the distant future.
Return an `ns-date' object representing a date in the distant future
(in terms of centuries).

You can pass this value when an NSDate object is required to have the
date argument essentially ignored. For example, the NSWindow method
nextEventMatchingMask:untilDate:inMode:dequeue: returns nil if an
event specified in the event mask does not happen before the specified
date. You can use the object returned by distantFuture as the date
argument to wait indefinitely for the event to occur.

see https://developer.apple.com/documentation/foundation/nsdate/distantfuture?language=objc"
  (invoke 'ns-date "distantFuture"))

;;; Calendrical Calculations

(define-objc-class "NSDateComponents" ()
  ()
  (:documentation
   "An object that specifies a date or time in terms of units
(such as year, month, day, hour, and minute) to be evaluated in a calendar system and time zone.
see https://developer.apple.com/documentation/foundation/nsdatecomponents?language=objc"))

(define-objc-class "NSCalendar" ()
  ()
  (:documentation
   "A definition of the relationships between calendar units and absolute points in time,
providing features for calculation and comparison of dates.
see https://developer.apple.com/documentation/foundation/nscalendar?language=objc"))

(define-objc-class "NSTimeZone" ()
  ()
  (:documentation
   "Information about standard time conventions associated with a specific geopolitical region.
see https://developer.apple.com/documentation/foundation/nstimezone?language=objc"))

;;; Date Formatting

(define-objc-class "NSDateFormatter" ()
  ()
  (:documentation
   "A formatter that converts between dates and their textual representations.
see https://developer.apple.com/documentation/foundation/dateformatter?language=objc"))

(define-objc-class "NSDateComponentsFormatter" ()
  ()
  (:documentation
   "A formatter that creates string representations of quantities of time.
see https://developer.apple.com/documentation/foundation/datecomponentsformatter?language=objc"))

(define-objc-class "NSDateIntervalFormatter" ()
  ()
  (:documentation
   "A formatter that creates string representations of time intervals.
see https://developer.apple.com/documentation/foundation/dateintervalformatter?language=objc"))

(define-objc-class "NSISO8601DateFormatter" ()
  ()
  (:documentation
   "A formatter that converts between dates and their ISO 8601 string representations.
see https://developer.apple.com/documentation/foundation/iso8601dateformatter?language=objc"))

;;; Internationalization

(define-objc-class "NSLocale" ()
  ()
  (:documentation
   "Information about linguistic, cultural, and technological conventions for use in
formatting data for presentation.
see https://developer.apple.com/documentation/foundation/nslocale?language=objc"))

;;; Deprecated

(define-objc-class "NSCalendarDate" ()
  ()
  (:documentation
   "A specialized date object with embedded calendar information.
see https://developer.apple.com/documentation/foundation/nscalendardate?language=objc"))

;;; Fundamentals


;;;; Units and Measurement
;; Label numeric quantities with physical dimensions to allow locale-aware formatting
;; and conversion between related units.
;; https://developer.apple.com/documentation/foundation/units-and-measurement?language=objc

;;; Essentials

(define-objc-class "NSMeasurement" ()
  ()
  (:documentation
   "A numeric quantity labeled with a unit of measure, with support for unit conversion
and unit-aware calculations.
see https://developer.apple.com/documentation/foundation/nsmeasurement?language=objc"))

(define-objc-class "NSUnit" ()
  ()
  (:documentation
   "An abstract class representing a unit of measure.
see https://developer.apple.com/documentation/foundation/unit?language=objc"))

(define-objc-class "NSDimension" ()
  ()
  (:documentation
   "An abstract class representing a dimensional unit of measure.
see https://developer.apple.com/documentation/foundation/dimension?language=objc"))

;;; Conversion

(define-objc-class "NSUnitConverter" ()
  ()
  (:documentation
   "An abstract class that provides a description of how to convert a unit to and from
the base unit of its dimension.
see https://developer.apple.com/documentation/foundation/unitconverter?language=objc"))

(define-objc-class "NSUnitConverterLinear" ()
  ()
  (:documentation
   "A description of how to convert between units using a linear equation.
see https://developer.apple.com/documentation/foundation/unitconverterlinear?language=objc"))

;;; Physical Dimension

(define-objc-class "NSUnitArea" ()
  ()
  (:documentation
   "A unit of measure for area.
see https://developer.apple.com/documentation/foundation/unitarea?language=objc"))

(define-objc-class "NSUnitLength" ()
  ()
  (:documentation
   "A unit of measure for length.
see https://developer.apple.com/documentation/foundation/unitlength?language=objc"))

(define-objc-class "NSUnitVolume" ()
  ()
  (:documentation
   "A unit of measure for volume.
see https://developer.apple.com/documentation/foundation/unitvolume?language=objc"))

(define-objc-class "NSUnitAngle" ()
  ()
  (:documentation
   "A unit of measure for planar angle and rotation.
see https://developer.apple.com/documentation/foundation/unitangle?language=objc"))

;;; Mass, Weight, and Force

(define-objc-class "NSUnitMass" ()
  ()
  (:documentation
   "A unit of measure for mass.
see https://developer.apple.com/documentation/foundation/unitmass?language=objc"))

(define-objc-class "NSUnitPressure" ()
  ()
  (:documentation
   "A unit of measure for pressure.
see https://developer.apple.com/documentation/foundation/unitpressure?language=objc"))

;;; Time and Motion

(define-objc-class "NSUnitAcceleration" ()
  ()
  (:documentation
   "A unit of measure for acceleration.
see https://developer.apple.com/documentation/foundation/unitacceleration?language=objc"))

(define-objc-class "NSUnitDuration" ()
  ()
  (:documentation
   "A unit of measure for a duration of time.
see https://developer.apple.com/documentation/foundation/unitduration?language=objc"))

(define-objc-class "NSUnitFrequency" ()
  ()
  (:documentation
   "A unit of measure for frequency.
see https://developer.apple.com/documentation/foundation/unitfrequency?language=objc"))

(define-objc-class "NSUnitSpeed" ()
  ()
  (:documentation
   "A unit of measure for speed.
see https://developer.apple.com/documentation/foundation/unitspeed?language=objc"))

;;; Energy, Heat, and Light

(define-objc-class "NSUnitEnergy" ()
  ()
  (:documentation
   "A unit of measure for energy.
see https://developer.apple.com/documentation/foundation/unitenergy?language=objc"))

(define-objc-class "NSUnitPower" ()
  ()
  (:documentation
   "A unit of measure for power.
see https://developer.apple.com/documentation/foundation/unitpower?language=objc"))

(define-objc-class "NSUnitTemperature" ()
  ()
  (:documentation
   "A unit of measure for temperature.
see https://developer.apple.com/documentation/foundation/unittemperature?language=objc"))

(define-objc-class "NSUnitIlluminance" ()
  ()
  (:documentation
   "A unit of measure for illuminance.
see https://developer.apple.com/documentation/foundation/unitilluminance?language=objc"))

;;; Electricity

(define-objc-class "NSUnitElectricCharge" ()
  ()
  (:documentation
   "A unit of measure for electric charge.
see https://developer.apple.com/documentation/foundation/unitelectriccharge?language=objc"))

(define-objc-class "NSUnitElectricCurrent" ()
  ()
  (:documentation
   "A unit of measure for electric current.
see https://developer.apple.com/documentation/foundation/unitelectriccurrent?language=objc"))

(define-objc-class "NSUnitElectricPotentialDifference" ()
  ()
  (:documentation
   "A unit of measure for electric potential difference.
see https://developer.apple.com/documentation/foundation/unitelectricpotentialdifference?language=objc"))

(define-objc-class "NSUnitElectricResistance" ()
  ()
  (:documentation
   "A unit of measure for electric resistance.
see https://developer.apple.com/documentation/foundation/unitelectricresistance?language=objc"))

;;; Concentration and Dispersion

(define-objc-class "NSUnitConcentrationMass" ()
  ()
  (:documentation
   "A unit of measure for concentration of mass.
see https://developer.apple.com/documentation/foundation/unitconcentrationmass?language=objc"))

(define-objc-class "NSUnitDispersion" ()
  ()
  (:documentation
   "A unit of measure for specific quantities of dispersion.
see https://developer.apple.com/documentation/foundation/unitdispersion?language=objc"))

;;; Fuel Efficiency

(define-objc-class "NSUnitFuelEfficiency" ()
  ()
  (:documentation
   "A unit of measure for fuel efficiency.
see https://developer.apple.com/documentation/foundation/unitfuelefficiency?language=objc"))

;;; Data Storage

(define-objc-class "NSUnitInformationStorage" ()
  ()
  (:documentation
   "A unit of measure for quantities of information.
see https://developer.apple.com/documentation/foundation/unitinformationstorage?language=objc"))


;;;; Data Formatting
;; Convert numbers, dates, measurements, and other values to and from locale-aware string representations.
;; https://developer.apple.com/documentation/foundation/data-formatting?language=objc

;;; Numbers and currency

(define-objc-class "NSNumberFormatter" ()
  ()
  (:documentation
   "A formatter that converts between numeric values and their textual representations.
see https://developer.apple.com/documentation/foundation/numberformatter?language=objc"))

;;; Names

(define-objc-class "NSPersonNameComponents" ()
  ()
  (:documentation
   "An object that manages the separate parts of a person’s name to allow locale-aware formatting.
see https://developer.apple.com/documentation/foundation/nspersonnamecomponents?language=objc"))

;;; Dates and times

(define-objc-class "NSDateFormatter" ()
  ()
  (:documentation
   "A formatter that converts between dates and their textual representations.
see https://developer.apple.com/documentation/foundation/dateformatter?language=objc"))

(define-objc-class "NSDateComponentsFormatter" ()
  ()
  (:documentation
   "A formatter that creates string representations of quantities of time.
see https://developer.apple.com/documentation/foundation/datecomponentsformatter?language=objc"))

(define-objc-class "NSRelativeDateTimeFormatter" ()
  ()
  (:documentation
   "A formatter that creates locale-aware string representations of a relative date or time.
see https://developer.apple.com/documentation/foundation/relativedatetimeformatter?language=objc"))

(define-objc-class "NSDateIntervalFormatter" ()
  ()
  (:documentation
   "A formatter that creates string representations of time intervals.
see https://developer.apple.com/documentation/foundation/dateintervalformatter?language=objc"))

(define-objc-class "NSISO8601DateFormatter" ()
  ()
  (:documentation
   "A formatter that converts between dates and their ISO 8601 string representations.
see https://developer.apple.com/documentation/foundation/iso8601dateformatter?language=objc"))

;;; Data sizes

(define-objc-class "NSByteCountFormatter" ()
  ()
  (:documentation
   "A formatter that converts a byte count value into a localized description that is formatted with the appropriate byte modifier (KB, MB, GB and so on).
see https://developer.apple.com/documentation/foundation/bytecountformatter?language=objc"))

;;; Measurements

(define-objc-class "NSMeasurementFormatter" ()
  ()
  (:documentation
   "A formatter that provides localized representations of units and measurements.
see https://developer.apple.com/documentation/foundation/measurementformatter?language=objc"))

;;; Lists

(define-objc-class "NSListFormatter" ()
  ()
  (:documentation
   "An object that provides locale-correct formatting of a list of items using the appropriate separator and conjunction.
see https://developer.apple.com/documentation/foundation/listformatter?language=objc"))

;;; Internationalization

(define-objc-class "NSLocale" ()
  ()
  (:documentation
   "Information about linguistic, cultural, and technological conventions for use in formatting data for presentation.
see https://developer.apple.com/documentation/foundation/nslocale?language=objc"))

;;; Custom formatters

(define-objc-class "NSFormatter" ()
  ()
  (:documentation
   "An abstract class that declares an interface for objects that create, interpret, and validate the textual representation of values.
see https://developer.apple.com/documentation/foundation/formatter?language=objc"))

;;; Automatic grammar agreement

(define-objc-class "NSInflectionRule" ()
  ()
  (:documentation
   "A rule that affects how an attributed string performs automatic grammatical agreement.
see https://developer.apple.com/documentation/foundation/nsinflectionrule?language=objc"))

(define-objc-class "NSInflectionRuleExplicit" ()
  ()
  (:documentation
   "An inflection rule that uses a morphology instance to determine how to inflect attribued strings.
see https://developer.apple.com/documentation/foundation/nsinflectionruleexplicit?language=objc"))

(define-objc-class "NSMorphology" ()
  ()
  (:documentation
   "A description of the grammatical properties of a string.
see https://developer.apple.com/documentation/foundation/nsmorphology?language=objc"))

(define-objc-class "NSTermOfAddress" ()
  ()
  (:documentation
   "The type for representing grammatical gender in localized text.
see https://developer.apple.com/documentation/foundation/nstermofaddress?language=objc"))

(define-objc-class "NSMorphologyPronoun" ()
  ()
  (:documentation
   "A custom pronoun for referring to a third person.
see https://developer.apple.com/documentation/foundation/nsmorphologypronoun?language=objc"))

;;; Deprecated

(define-objc-class "NSLengthFormatter" ()
  ()
  (:documentation
   "A formatter that provides localized descriptions of linear distances, such as length and height measurements.
see https://developer.apple.com/documentation/foundation/lengthformatter?language=objc"))

(define-objc-class "NSMassFormatter" ()
  ()
  (:documentation
   "A formatter that provides localized descriptions of mass and weight values.
see https://developer.apple.com/documentation/foundation/massformatter?language=objc"))

(define-objc-class "NSEnergyFormatter" ()
  ()
  (:documentation
   "A formatter that provides localized descriptions of energy values.
see https://developer.apple.com/documentation/foundation/energyformatter?language=objc"))


;;;; Filters and Sorting
;; Use predicates, expressions, and sort descriptors to examine elements in collections and other services.
;; https://developer.apple.com/documentation/foundation/filters-and-sorting?language=objc

;;; Filltering

(define-objc-class "NSPredicate" ()
  ()
  (:documentation
   "A definition of logical conditions for constraining a search for a fetch or for in-memory filtering.
see https://developer.apple.com/documentation/foundation/nspredicate?language=objc"))

(define-objc-class "NSExpression" ()
  ()
  (:documentation
   "An expression for use in a comparison predicate.
see https://developer.apple.com/documentation/foundation/nsexpression?language=objc"))

(define-objc-class "NSComparisonPredicate" ()
  ()
  (:documentation
   "A specialized predicate for comparing expressions.
see https://developer.apple.com/documentation/foundation/nscomparisonpredicate?language=objc"))

(define-objc-class "NSCompoundPredicate" ()
  ()
  (:documentation
   "A specialized predicate that evaluates logical combinations of other predicates.
see https://developer.apple.com/documentation/foundation/nscompoundpredicate?language=objc"))

;;; Sorting

(define-objc-class "NSSortDescriptor" ()
  ()
  (:documentation
   "An immutable description of how to order a collection of objects according to a property common to all the objects.
see https://developer.apple.com/documentation/foundation/nssortdescriptor?language=objc"))

(define-objc-enum ns-ordered
  "Constants that indicate sort order.

These constants are used to indicate how items in a request are
ordered, from the first one given in a method invocation or function
call to the last (that is, left to right in code). "
  (:ascending  #xFFFFFFFFFFFFFFFF "The left operand is smaller than the right operand.")
  (:same       0                  "The two operands are equal.")
  (:descending 1                  "The left operand is greater than the right operand."))


;;;; Task Management
;; Manage your app’s work and how it interacts with system services like Handoff and Shortcuts.
;; https://developer.apple.com/documentation/foundation/task-management?language=objc

;;; Undo

(define-objc-class "NSUndoManager" ()
  ()
  (:documentation
   "A general-purpose recorder of operations that enables undo and redo.
see https://developer.apple.com/documentation/foundation/undomanager?language=objc"))

;;; Progress

(define-objc-class "NSProgress" ()
  ()
  (:documentation
   "An object that conveys ongoing progress to the user for a specified task.
see https://developer.apple.com/documentation/foundation/progress?language=objc"))

;;; Operations

(define-objc-class "NSOperation" ()
  ()
  (:documentation
   "An abstract class that represents the code and data associated with a single task.
see https://developer.apple.com/documentation/foundation/operation?language=objc"))

(define-objc-class "NSOperationQueue" ()
  ()
  (:documentation
   "A queue that regulates the execution of operations.
see https://developer.apple.com/documentation/foundation/operationqueue?language=objc"))

(define-objc-class "NSBlockOperation" ()
  ()
  (:documentation
   "An operation that manages the concurrent execution of one or more blocks.
see https://developer.apple.com/documentation/foundation/blockoperation?language=objc"))

(define-objc-class "NSInvocationOperation" ()
  ()
  (:documentation
   "An operation that manages the execution of a single encapsulated task specified as an invocation.
see https://developer.apple.com/documentation/foundation/nsinvocationoperation?language=objc"))

;;; Scheduling

(define-objc-class "NSTimer" ()
  ()
  (:documentation
   "A timer that fires after a certain time interval has elapsed,
sending a specified message to a target object.
see https://developer.apple.com/documentation/foundation/timer?language=objc"))

;;; Activity Sharing

(define-objc-class "NSUserActivity" ()
  ()
  (:documentation
   "A representation of the state of your app at a moment in time.
see https://developer.apple.com/documentation/foundation/nsuseractivity?language=objc"))

;;; System Interaction

(define-objc-class "NSProcessInfo" ()
  ()
  (:documentation
   "A collection of information about the current process.
see https://developer.apple.com/documentation/foundation/processinfo?language=objc"))

(define-objc-class "NSBackgroundActivityScheduler" ()
  ()
  (:documentation
   "A task scheduler suitable for low priority operations that can run in the background.
see https://developer.apple.com/documentation/foundation/nsbackgroundactivityscheduler?language=objc"))

;;; User Notifications

(define-objc-class "NSUserNotification" ()
  ()
  (:documentation
   "A notification that can be scheduled for display in the notification center.
see https://developer.apple.com/documentation/foundation/nsusernotification?language=objc"))

(define-objc-class "NSUserNotificationAction" ()
  ()
  (:documentation
   "An action that the user can take in response to receiving a notification.
see https://developer.apple.com/documentation/foundation/nsusernotificationaction?language=objc"))

(define-objc-class "NSUserNotificationCenter" ()
  ()
  (:documentation
   "An object that delivers notifications from apps to the user.
see https://developer.apple.com/documentation/foundation/nsusernotificationcenter?language=objc"))


;;;; Resources
;; Access assets and other data bundled with your app.
;; https://developer.apple.com/documentation/foundation/resources?language=objc

;;; Bundle Resources

(define-objc-class "NSBundle" ()
  ()
  (:documentation
   "A representation of the code and resources stored in a bundle directory on disk.
see https://developer.apple.com/documentation/foundation/bundle?language=objc"))

;;; On-Demand Resources

(define-objc-class "NSBundleResourceRequest" ()
  ()
  (:documentation
   "A resource manager you use to download content hosted on the App Store at the time your app needs it.
see https://developer.apple.com/documentation/foundation/nsbundleresourcerequest?language=objc"))


;;;; Notifications
;; Design patterns for broadcasting information and for subscribing to broadcasts.
;; see https://developer.apple.com/documentation/foundation/notifications?language=objc

;;; Key-Value Observing

;;; Notifications

(define-objc-class "NSNotification" ()
  ()
  (:documentation
   "A container for information broadcast through a notification center to all registered observers.
see https://developer.apple.com/documentation/foundation/nsnotification?language=objc"))

(define-objc-class "NSNotificationCenter" ()
  ()
  (:documentation
   "A notification dispatch mechanism that enables the broadcast of information to registered observers.
see https://developer.apple.com/documentation/foundation/notificationcenter?language=objc"))

(define-objc-class "NSNotificationQueue" ()
  ()
  (:documentation
   "A notification center buffer.
see https://developer.apple.com/documentation/foundation/notificationqueue?language=objc"))

;;; Cross-Process Notifications

(define-objc-class "NSDistributedNotificationCenter" ()
  ()
  (:documentation
   "A notification dispatch mechanism that enables the broadcast of notifications across task boundaries.
see https://developer.apple.com/documentation/foundation/distributednotificationcenter?language=objc"))


;;;; App Extension Support
;; Manage the interaction between an app extension and its hosting app.
;; see https://developer.apple.com/documentation/foundation/app-extension-support?language=objc

;;; Extension Support

(define-objc-class "NSExtensionContext" ()
  ()
  (:documentation
   "The host app context from which an app extension is invoked.
see https://developer.apple.com/documentation/foundation/nsextensioncontext?language=objc"))

;;; Share Extensions

;;; Attachments

(define-objc-class "NSItemProvider" ()
  ()
  (:documentation
   "An item provider for conveying data or a file between processes during
drag-and-drop or copy-and-paste activities, or from a host app to an app extension.
see https://developer.apple.com/documentation/foundation/nsitemprovider?language=objc"))

(define-objc-class "NSExtensionItem" ()
  ()
  (:documentation
   "An immutable collection of values representing different aspects of an item
for an extension to act upon.
see https://developer.apple.com/documentation/foundation/nsextensionitem?language=objc"))

;;; Host App Interaction

(define-objc-class "NSUserActivity" ()
  ()
  (:documentation
   "A representation of the state of your app at a moment in time.
see https://developer.apple.com/documentation/foundation/nsuseractivity?language=objc"))


;;;; Errors and Exceptions
;; Respond to problem situations in your interactions with APIs, and fine-tune your app for better debugging.
;; see https://developer.apple.com/documentation/foundation/errors-and-exceptions?language=objc

;;; User-Relevant Errors

(define-objc-class "NSError" ()
  ()
  (:documentation
   "Information about an error condition including a domain, a domain-specific error code,
and application-specific information.
see https://developer.apple.com/documentation/foundation/errors-and-exceptions?language=objc"))

;;; Assertions

(define-objc-class "NSAssertionHandler" ()
  ()
  (:documentation
   "An object that logs an assertion to the console.
see https://developer.apple.com/documentation/foundation/nsassertionhandler?language=objc"))

;;; Exceptions

(define-objc-class "NSException" ()
  ()
  (:documentation
   "An object that represents a special condition that interrupts the
normal flow of program execution.
see https://developer.apple.com/documentation/foundation/nsexception?language=objc"))

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

;;; Script Execution

(define-objc-class "NSAppleScript" ()
  ()
  (:documentation
   "An object that provides the ability to load, compile, and execute scripts.
see https://developer.apple.com/documentation/foundation/nsapplescript?language=objc"))

;;; Apple Event Handling

(define-objc-class "NSAppleEventDescriptor" ()
  ()
  (:documentation
   "A wrapper for the Apple event descriptor data type.
see https://developer.apple.com/documentation/foundation/nsappleeventdescriptor?language=objc"))

(define-objc-class "NSAppleEventManager" ()
  ()
  (:documentation
   "A mechanism for registering handler routines for specific types of Apple events
and dispatching events to those handlers.
see https://developer.apple.com/documentation/foundation/nsappleeventmanager?language=objc"))

;;; Script Commands

(define-objc-class "NSScriptCommand" ()
  ()
  (:documentation
   "A self-contained scripting statement.
see https://developer.apple.com/documentation/foundation/nsscriptcommand?language=objc"))

(define-objc-class "NSQuitCommand" ()
  ()
  (:documentation
   "A command that quits the specified app.
see https://developer.apple.com/documentation/foundation/nsquitcommand?language=objc"))

(define-objc-class "NSSetCommand" ()
  ()
  (:documentation
   "A command that sets one or more attributes or relationships to one or more values.
see https://developer.apple.com/documentation/foundation/nssetcommand?language=objc"))

(define-objc-class "NSMoveCommand" ()
  ()
  (:documentation
   "A command that moves one or more scriptable objects.
see https://developer.apple.com/documentation/foundation/nsmovecommand?language=objc"))

(define-objc-class "NSCreateCommand" ()
  ()
  (:documentation
   "A command that creates a scriptable object.
see https://developer.apple.com/documentation/foundation/nscreatecommand?language=objc"))

(define-objc-class "NSDeleteCommand" ()
  ()
  (:documentation
   "A command that deletes a scriptable object.
see https://developer.apple.com/documentation/foundation/nsdeletecommand?language=objc"))

(define-objc-class "NSExistsCommand" ()
  ()
  (:documentation
   "A command that determines whether a scriptable object exists.
see https://developer.apple.com/documentation/foundation/nsexistscommand?language=objc"))

(define-objc-class "NSGetCommand" ()
  ()
  (:documentation
   "A command that retrieves a value or object from a scriptable object.
see https://developer.apple.com/documentation/foundation/nsgetcommand?language=objc"))

(define-objc-class "NSCloneCommand" ()
  ()
  (:documentation
   "A command that clones one or more scriptable objects.
see https://developer.apple.com/documentation/foundation/nsclonecommand?language=objc"))

(define-objc-class "NSCountCommand" ()
  ()
  (:documentation
   "A command that counts the number of objects of a specified class in the specified object container.
see https://developer.apple.com/documentation/foundation/nscountcommand?language=objc"))

(define-objc-class "NSCloseCommand" ()
  ()
  (:documentation
   "A command that closes one or more scriptable objects.
see https://developer.apple.com/documentation/foundation/nsclosecommand?language=objc"))

;;; Object Specifiers

(define-objc-class "NSScriptObjectSpecifier" ()
  ()
  (:documentation
   "An abstract class used to represent natural language expressions.
see https://developer.apple.com/documentation/foundation/nsscriptobjectspecifier?language=objc"))

(define-objc-class "NSPropertySpecifier" ()
  ()
  (:documentation
   "A specifier for a simple attribute value, a one-to-one relationship, or all elements of a to-many relationship.
see https://developer.apple.com/documentation/foundation/nspropertyspecifier?language=objc"))

(define-objc-class "NSPositionalSpecifier" ()
  ()
  (:documentation
   "A specifier for an insertion point in a container relative to another object in the container.
see https://developer.apple.com/documentation/foundation/nspositionalspecifier?language=objc"))

(define-objc-class "NSRandomSpecifier" ()
  ()
  (:documentation
   "A specifier for an arbitrary object in a collection or,
if not a one-to-many relationship, the sole object.
see https://developer.apple.com/documentation/foundation/nsrandomspecifier?language=objc"))

(define-objc-class "NSRangeSpecifier" ()
  ()
  (:documentation
   "A specifier for a range of objects in a container.
see https://developer.apple.com/documentation/foundation/nsrangespecifier?language=objc"))

(define-objc-class "NSUniqueIDSpecifier" ()
  ()
  (:documentation
   "A specifier for an object in a collection (or container) by unique ID.
see https://developer.apple.com/documentation/foundation/nsuniqueidspecifier?language=objc"))

(define-objc-class "NSWhoseSpecifier" ()
  ()
  (:documentation
   "A specifier that indicates every object in a collection matching a condition.
see https://developer.apple.com/documentation/foundation/nswhosespecifier?language=objc"))

(define-objc-class "NSNameSpecifier" ()
  ()
  (:documentation
   "A specifier for an object in a collection (or container) by name.
see https://developer.apple.com/documentation/foundation/nsnamespecifier?language=objc"))

(define-objc-class "NSMiddleSpecifier" ()
  ()
  (:documentation
   "A specifier indicating the middle object in a collection or,
if not a one-to-many relationship, the sole object.
see https://developer.apple.com/documentation/foundation/nsmiddlespecifier?language=objc"))

(define-objc-class "NSIndexSpecifier" ()
  ()
  (:documentation
   "A specifier representing an object in a collection (or container) with an index number.
see https://developer.apple.com/documentation/foundation/nsindexspecifier?language=objc"))

(define-objc-class "NSRelativeSpecifier" ()
  ()
  (:documentation
   "A specifier that indicates an object in a collection by its position relative to another object.
see https://developer.apple.com/documentation/foundation/nsrelativespecifier?language=objc"))

;;; Script Dictionary Description

(define-objc-class "NSScriptSuiteRegistry" ()
  ()
  (:documentation
   "The top-level repository of scriptability information for an app at runtime.
see https://developer.apple.com/documentation/foundation/nsscriptsuiteregistry?language=objc"))

(define-objc-class "NSScriptClassDescription" ()
  ()
  (:documentation
   "A scriptable class that a macOS app supports.
see https://developer.apple.com/documentation/foundation/nsscriptclassdescription?language=objc"))

(define-objc-class "NSClassDescription" ()
  ()
  (:documentation
   "An abstract class that provides the interface for querying the relationships and properties of a class.
see https://developer.apple.com/documentation/foundation/nsclassdescription?language=objc"))

(define-objc-class "NSScriptCommandDescription" ()
  ()
  (:documentation
   "A script command that a macOS app supports.
see https://developer.apple.com/documentation/foundation/nsscriptcommanddescription?language=objc"))

;;; Object Matching Tests

(define-objc-class "NSScriptWhoseTest" ()
  ()
  (:documentation
   "An abstract class that provides the basis for testing specifiers one at a time or in groups.
see https://developer.apple.com/documentation/foundation/nsscriptwhosetest?language=objc"))

(define-objc-class "NSSpecifierTest" ()
  ()
  (:documentation
   "A comparison between an object specifier and a test object.
see https://developer.apple.com/documentation/foundation/nsspecifiertest?language=objc"))

(define-objc-class "NSLogicalTest" ()
  ()
  (:documentation
   "The logical combination of one or more specifier tests.
see https://developer.apple.com/documentation/foundation/nslogicaltest?language=objc"))

;;; NSObject Script Support

(define-objc-class "NSScriptCoercionHandler" ()
  ()
  (:documentation
   "A mechanism for converting one kind of scripting data to another.
see https://developer.apple.com/documentation/foundation/nsscriptcoercionhandler?language=objc"))

(define-objc-class "NSScriptExecutionContext" ()
  ()
  (:documentation
   "The context in which the current script command is executed.
see https://developer.apple.com/documentation/foundation/nsscriptexecutioncontext?language=objc"))


;;;; File System
;; Create, read, write, and examine files and folders in the file system.
;; see https://developer.apple.com/documentation/foundation/file-system?language=objc

;;; File system operations

(define-objc-class "NSFileManager" ()
  ()
  (:documentation
   "A convenient interface to the contents of the file system, and the primary means of interacting with it.
see https://developer.apple.com/documentation/foundation/filemanager?language=objc"))

;;; Coordinated file access

(define-objc-class "NSFileAccessIntent" ()
  ()
  (:documentation
   "The details of a coordinated-read or coordinated-write operation.
see https://developer.apple.com/documentation/foundation/nsfileaccessintent?language=objc"))

(define-objc-class "NSFileCoordinator" ()
  ()
  (:documentation
   "An object that coordinates the reading and writing of files and directories among file presenters.
see https://developer.apple.com/documentation/foundation/nsfilecoordinator?language=objc"))

;;; Managed file access

(define-objc-class "NSFileHandle" ()
  ()
  (:documentation
   "An object-oriented wrapper for a file descriptor.
see https://developer.apple.com/documentation/foundation/filehandle?language=objc"))

(define-objc-class "NSFileSecurity" ()
  ()
  (:documentation
   "A stub class that encapsulates security information about a file.
see https://developer.apple.com/documentation/foundation/nsfilesecurity?language=objc"))

(define-objc-class "NSFileVersion" ()
  ()
  (:documentation
   "A snapshot of a file at a specific point in time.
see https://developer.apple.com/documentation/foundation/nsfileversion?language=objc"))

(define-objc-class "NSFileWrapper" ()
  ()
  (:documentation
   "A representation of a node (a file, directory, or symbolic link) in the file system.
see https://developer.apple.com/documentation/foundation/filewrapper?language=objc"))

;;; Errors



;;;; Archives and Serialization
;; Convert objects and values to and from property list, JSON, and other flat binary representations.
;; see https://developer.apple.com/documentation/foundation/archives-and-serialization?language=objc

;;; Adopting Codability

;;; JSON

(define-objc-class "NSJSONSerialization" ()
  ()
  (:documentation
   "An object that converts between JSON and the equivalent Foundation objects.
see https://developer.apple.com/documentation/foundation/jsonserialization?language=objc"))

;;; Property Lists

(define-objc-class "NSPropertyListSerialization" ()
  ()
  (:documentation
   "An object that converts between a property list and one of several serialized representations.
see https://developer.apple.com/documentation/foundation/propertylistserialization?language=objc"))

;;; XML
;; https://developer.apple.com/documentation/foundation/xml-processing-and-modeling?language=objc

;;; Keyed Archivers

(define-objc-class "NSKeyedArchiver" ()
  ()
  (:documentation
   "An encoder that stores an object’s data to an archive referenced by keys.
see https://developer.apple.com/documentation/foundation/nskeyedarchiver?language=objc"))

(define-objc-class "NSKeyedUnarchiver" ()
  ()
  (:documentation
   "A decoder that restores data from an archive referenced by keys.
see https://developer.apple.com/documentation/foundation/nskeyedunarchiver?language=objc"))

(define-objc-class "NSCoder" ()
  ()
  (:documentation
   "An abstract class that serves as the basis for objects that enable archiving and distribution of other objects.
see https://developer.apple.com/documentation/foundation/nscoder?language=objc"))

(define-objc-class "NSSecureUnarchiveFromDataTransformer" ()
  ()
  (:documentation
   "A value transformer that converts data to and from classes that support secure coding.
see https://developer.apple.com/documentation/foundation/nssecureunarchivefromdatatransformer?language=objc"))

;;; Deprecated

(define-objc-class "NSArchiver" ()
  ()
  (:documentation
   "A coder that stores an object’s data to an archive.
see https://developer.apple.com/documentation/foundation/nsarchiver?language=objc"))

(define-objc-class "NSUnarchiver" ()
  ()
  (:documentation
   "A decoder that restores data from an archive.
see https://developer.apple.com/documentation/foundation/nsunarchiver?language=objc"))

;; (defcfun ... "NXReadNSObjectFromCoder"
;;   "Returns the next object from the coder."
;;   "see https://developer.apple.com/documentation/foundation/nxreadnsobjectfromcoder?language=objc")

;;; Files and Data Persistence


;;;; Settings
;; Configure your app using data you store persistently on the local disk or in iCloud.
;; see https://developer.apple.com/documentation/foundation/settings?language=objc


;;; App-specific settings

(define-objc-class "NSUserDefaults" ()
  ()
  (:documentation
   "An interface to the user’s defaults database, which stores system-wide and app-specific settings.
see https://developer.apple.com/documentation/foundation/userdefaults?language=objc"))

;;; Settings interfaces

;;; iCloud key and value storage

(define-objc-class "NSUbiquitousKeyValueStore" ()
  ()
  (:documentation
   "An iCloud-based container of key-value pairs you share among instances of your app running on a person’s devices.
see https://developer.apple.com/documentation/foundation/nsubiquitouskeyvaluestore?language=objc"))


;;;; Spotlight
;; Search for files and other items on the local device, and index your app’s content for searching.
;; see https://developer.apple.com/documentation/foundation/spotlight?language=objc

;;; Queries

(define-objc-class "NSMetadataQuery" ()
  ()
  (:documentation
   "A query that you perform against Spotlight metadata.
see https://developer.apple.com/documentation/foundation/nsmetadataquery?language=objc"))

;;; Items

(define-objc-class "NSMetadataItem" ()
  ()
  (:documentation
   "The metadata associated with a file.
see https://developer.apple.com/documentation/foundation/nsmetadataitem?language=objc"))


;;;; iCloud
;; Manage files and key-value data that automatically synchronize among a user’s iCloud devices.
;; see https://developer.apple.com/documentation/foundation/icloud?language=objc


;;; iCloud Storage

(define-objc-class "NSFileManager" ()
  ()
  (:documentation
   "A convenient interface to the contents of the file system,
and the primary means of interacting with it.
see https://developer.apple.com/documentation/foundation/filemanager?language=objc"))

;;; App Preferences

(define-objc-class "NSUbiquitousKeyValueStore" ()
  ()
  (:documentation
   "An iCloud-based container of key-value pairs you share among instances
of your app running on a person’s devices.
see https://developer.apple.com/documentation/foundation/nsubiquitouskeyvaluestore?language=objc"))

;;; File Search

(define-objc-class "NSMetadataQuery" ()
  ()
  (:documentation
   "A query that you perform against Spotlight metadata.
see https://developer.apple.com/documentation/foundation/nsmetadataquery?language=objc"))

(define-objc-class "NSMetadataItem" ()
  ()
  (:documentation
   "The metadata associated with a file.
see https://developer.apple.com/documentation/foundation/nsmetadataitem?language=objc"))

;;; Entitlements

;;; Errors


;;;; Optimizing Your App’s Data for iCloud Backup
;; Minimize the space and time that backups take to create by excluding purgeable and nonpurgeable data from backups.
;; see https://developer.apple.com/documentation/foundation/optimizing-your-app-s-data-for-icloud-backup?language=objc


;;;; URL Loading System
;; Interact with URLs and communicate with servers using standard Internet protocols.
;; see https://developer.apple.com/documentation/foundation/url-loading-system?language=objc

;;; Essentials

(define-objc-class "NSURLSession" ()
  ()
  (:documentation
   "An object that coordinates a group of related, network data transfer tasks.
see https://developer.apple.com/documentation/foundation/urlsession?language=objc"))

(define-objc-class "NSURLSessionTask" ()
  ()
  (:documentation
   "A task, like downloading a specific resource, performed in a URL session.
see https://developer.apple.com/documentation/foundation/urlsessiontask?language=objc"))

;;; Requests and responses

(define-objc-class "NSURLRequest" ()
  ()
  (:documentation
   "A URL load request that is independent of protocol or URL scheme.
see https://developer.apple.com/documentation/foundation/nsurlrequest?language=objc"))

(define-objc-class "NSMutableURLRequest" ()
  ()
  (:documentation
   "A mutable URL load request that is independent of protocol or URL scheme.
see https://developer.apple.com/documentation/foundation/nsmutableurlrequest?language=objc"))

(define-objc-class "NSURLResponse" ()
  ()
  (:documentation
   "The metadata associated with the response to a URL load request, independent of protocol and URL scheme.
see https://developer.apple.com/documentation/foundation/urlresponse?language=objc"))

(define-objc-class "NSHTTPURLResponse" ()
  ()
  (:documentation
   "The metadata associated with the response to an HTTP protocol URL load request.
see https://developer.apple.com/documentation/foundation/httpurlresponse?language=objc"))

;;; Uploading

;;; Downloading

;;; Cache behavior

(define-objc-class "NSCachedURLResponse" ()
  ()
  (:documentation
   "A cached response to a URL request.
see https://developer.apple.com/documentation/foundation/cachedurlresponse?language=objc"))

(define-objc-class "NSURLCache" ()
  ()
  (:documentation
   "An object that maps URL requests to cached response objects.
see https://developer.apple.com/documentation/foundation/urlcache?language=objc"))

;;; Authentication and credentials

(define-objc-class "NSURLAuthenticationChallenge" ()
  ()
  (:documentation
   "A challenge from a server requiring authentication from the client.
see https://developer.apple.com/documentation/foundation/urlauthenticationchallenge?language=objc"))

(define-objc-class "NSURLCredential" ()
  ()
  (:documentation
   "An authentication credential consisting of information specific to the type of credential and the type of persistent storage to use, if any.
see https://developer.apple.com/documentation/foundation/urlcredential?language=objc"))

(define-objc-class "NSURLCredentialStorage" ()
  ()
  (:documentation
   "The manager of a shared credentials cache.
see https://developer.apple.com/documentation/foundation/urlcredentialstorage?language=objc"))

(define-objc-class "NSURLProtectionSpace" ()
  ()
  (:documentation
   "A server or an area on a server, commonly referred to as a realm, that requires authentication.
see https://developer.apple.com/documentation/foundation/urlprotectionspace?language=objc"))

;;; Network activity attribution

;;; Cookies

(define-objc-class "NSHTTPCookie" ()
  ()
  (:documentation
   "A representation of an HTTP cookie.
see https://developer.apple.com/documentation/foundation/httpcookie?language=objc"))

(define-objc-class "NSHTTPCookieStorage" ()
  ()
  (:documentation
   "A container that manages the storage of cookies.
see https://developer.apple.com/documentation/foundation/httpcookiestorage?language=objc"))

;;; Errors

;;; Legacy

;;; Networking


;;;; Bonjour
;; Advertise services for easy discovery on local networks, or discover services advertised by others.
;; see https://developer.apple.com/documentation/foundation/bonjour?language=objc

;;; Local Network Services

(define-objc-class "NSNetService" ()
  ()
  (:documentation
   "A network service that broadcasts its availability using multicast DNS.
see https://developer.apple.com/documentation/foundation/netservice?language=objc"))

;;; Service Discovery

(define-objc-class "NSNetServiceBrowser" ()
  ()
  (:documentation
   "A network service browser that finds published services on a network using multicast DNS.
see https://developer.apple.com/documentation/foundation/netservicebrowser?language=objc"))

;;; Networking


;;;; XPC
;; Manage secure interprocess communication.
;; see https://developer.apple.com/documentation/foundation/xpc?language=objc

;;; XPC Client

(define-objc-class "NSXPCConnection" ()
  ()
  (:documentation
   "A bidirectional communication channel between two processes.
see https://developer.apple.com/documentation/foundation/nsxpcconnection?language=objc"))

(define-objc-class "NSXPCInterface" ()
  ()
  (:documentation
   "An interface that may be sent to an exported object or remote object proxy.
see https://developer.apple.com/documentation/foundation/nsxpcinterface?language=objc"))

(define-objc-class "NSXPCCoder" ()
  ()
  (:documentation
   "A coder that encodes and decodes objects that your app sends over an XPC connection.
see https://developer.apple.com/documentation/foundation/nsxpccoder?language=objc"))

;;; XPC Services

(define-objc-class "NSXPCListener" ()
  ()
  (:documentation
   "A listener that waits for new incoming connections, configures them, and accepts or rejects them.
see https://developer.apple.com/documentation/foundation/nsxpclistener?language=objc"))

(define-objc-class "NSXPCListenerEndpoint" ()
  ()
  (:documentation
   "An object that names a specific XPC listener.
see https://developer.apple.com/documentation/foundation/nsxpclistenerendpoint?language=objc"))

;;; Low-Level Utilities


;;;; Processes and Threads
;; Manage your app’s interaction with the host operating system and other processes, and implement low-level concurrency features.
;; see https://developer.apple.com/documentation/foundation/processes-and-threads?language=objc

;;; Run Loop Scheduling

(define-objc-class "NSRunLoop" ()
  ()
  (:documentation
   "The programmatic interface to objects that manage input sources.
see https://developer.apple.com/documentation/foundation/runloop?language=objc"))

(deftype ns-run-loop-mode ()
  "Modes that a run loop operates in.

System Run Loop Modes:
+ `+ns-run-loop-common-modes+'
+ `+ns-default-run-loop-mode+'

`coca.appkit:ns-application' defines additional run loop modes,
including the following:
+ `coca.appkit:+ns-event-tracking-run-loop-mode+'
+ `coca.appkit:+ns-modal-panel-run-loop-mode+'

"
  'ns-string)

(define-objc-const +ns-run-loop-common-modes+
    ("NSRunLoopCommonModes" :object coca.objc::foundation)
  "A pseudo-mode that includes one or more other run loop modes.
see https://developer.apple.com/documentation/foundation/runloop/mode/common?language=objc")

(define-objc-const +ns-default-run-loop-mode+
    ("NSDefaultRunLoopMode" :object coca.objc::foundation)
  "The mode set to handle input sources other than connection objects.
see https://developer.apple.com/documentation/foundation/runloop/mode/default?language=objc")

(define-objc-class "NSTimer" ()
  ()
  (:documentation
   "A timer that fires after a certain time interval has elapsed,
sending a specified message to a target object.
see https://developer.apple.com/documentation/foundation/timer?language=objc"))

;;; Process Info

(define-objc-class "NSProcessInfo" ()
  ()
  (:documentation
   "A collection of information about the current process.
see https://developer.apple.com/documentation/foundation/processinfo?language=objc"))

;;; Threads and Locking

(define-objc-class "NSThread" ()
  ()
  (:documentation
   "A thread of execution.
see https://developer.apple.com/documentation/foundation/thread?language=objc"))

(define-objc-class "NSLock" ()
  ()
  (:documentation
   "An object that coordinates the operation of multiple threads of execution within the same application.
see https://developer.apple.com/documentation/foundation/nslock?language=objc"))

(define-objc-class "NSRecursiveLock" ()
  ()
  (:documentation
   "A lock that may be acquired multiple times by the same thread without causing a deadlock.
see https://developer.apple.com/documentation/foundation/nsrecursivelock?language=objc"))

(define-objc-class "NSDistributedLock" ()
  ()
  (:documentation
   "A lock that multiple applications on multiple hosts can use to restrict
access to some shared resource, such as a file.
see https://developer.apple.com/documentation/foundation/nsdistributedlock?language=objc"))

(define-objc-class "NSConditionLock" ()
  ()
  (:documentation
   "A lock that can be associated with specific, user-defined conditions.
see https://developer.apple.com/documentation/foundation/nsconditionlock?language=objc"))

(define-objc-class "NSCondition" ()
  ()
  (:documentation
   "A condition variable whose semantics follow those used for POSIX-style conditions.
see https://developer.apple.com/documentation/foundation/nscondition?language=objc"))

;;; Operations

(define-objc-class "NSOperationQueue" ()
  ()
  (:documentation
   "A queue that regulates the execution of operations.
see https://developer.apple.com/documentation/foundation/operationqueue?language=objc"))

(define-objc-class "NSOperation" ()
  ()
  (:documentation
   "An abstract class that represents the code and data associated with a single task.
see https://developer.apple.com/documentation/foundation/operation?language=objc"))

(define-objc-class "NSBlockOperation" ()
  ()
  (:documentation
   "An operation that manages the concurrent execution of one or more blocks.
see https://developer.apple.com/documentation/foundation/blockoperation?language=objc"))

(define-objc-class "NSInvocationOperation" ()
  ()
  (:documentation
   "An operation that manages the execution of a single encapsulated task specified as an invocation.
see https://developer.apple.com/documentation/foundation/nsinvocationoperation?language=objc"))

;;; Scripts and External Tasks

(define-objc-class "NSTask" ()
  ()
  (:documentation
   "An object that represents a subprocess of the current process.
see https://developer.apple.com/documentation/foundation/process?language=objc"))

(define-objc-class "NSUserScriptTask" ()
  ()
  (:documentation
   "An object that executes scripts.
see https://developer.apple.com/documentation/foundation/nsuserscripttask?language=objc"))

(define-objc-class "NSUserAppleScriptTask" ()
  ()
  (:documentation
   "An object that executes AppleScript scripts.
see https://developer.apple.com/documentation/foundation/nsuserapplescripttask?language=objc"))

(define-objc-class "NSUserAutomatorTask" ()
  ()
  (:documentation
   "An object that executes Automator workflows.
see https://developer.apple.com/documentation/foundation/nsuserautomatortask?language=objc"))

(define-objc-class "NSUserUnixTask" ()
  ()
  (:documentation
   "An object that executes unix applications.
see https://developer.apple.com/documentation/foundation/nsuserunixtask?language=objc"))


;;;; Streams, Sockets, and Ports
;; Use low-level Unix features to manage input and output among files, processes, and the network.
;; see https://developer.apple.com/documentation/foundation/streams-sockets-and-ports?language=objc

;;; Streams

(define-objc-class "NSStream" ()
  ()
  (:documentation
   "An abstract class representing a stream.
see https://developer.apple.com/documentation/foundation/stream?language=objc"))

(define-objc-class "NSInputStream" ()
  ()
  (:documentation
   "A stream that provides read-only stream functionality.
see https://developer.apple.com/documentation/foundation/inputstream?language=objc"))

(define-objc-class "NSOutputStream" ()
  ()
  (:documentation
   "A stream that provides write-only stream functionality.
see https://developer.apple.com/documentation/foundation/outputstream?language=objc"))

;;; Tasks and Pipes

(define-objc-class "NSTask" ()
  ()
  (:documentation
   "An object that represents a subprocess of the current process.
see https://developer.apple.com/documentation/foundation/process?language=objc"))

(define-objc-class "NSPipe" ()
  ()
  (:documentation
   "A one-way communications channel between related processes.
see https://developer.apple.com/documentation/foundation/pipe?language=objc"))

;;; Sockets

(define-objc-class "NSHost" ()
  ()
  (:documentation
   "A representation of an individual host on the network.
see https://developer.apple.com/documentation/foundation/host?language=objc"))

(define-objc-class "NSPort" ()
  ()
  (:documentation
   "An abstract class that represents a communication channel.
see https://developer.apple.com/documentation/foundation/port?language=objc"))

(define-objc-class "NSSocketPort" ()
  ()
  (:documentation
   "A port that represents a BSD socket.
see https://developer.apple.com/documentation/foundation/socketport?language=objc"))

;;; Byte Ordering

;;; Low-Level Utilities

;;;; foundation.lisp ends here
