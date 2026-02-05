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
   #:as-ns-url
   #:ns-url-to-pathname
   #:pathname-to-ns-url
   #:scheme
   #:user
   #:password
   #:host
   #:port
   #:path
   #:path-extension
   #:query
   #:fragment
   #:ns-url-components
   #:ns-url-query-item
   #:ns-uuid
   #:ns-rect
   #:ns-rect*
   #:ns-rect-p
   #:make-ns-rect
   #:ns-rect-x
   #:ns-rect-y
   #:ns-rect-w
   #:ns-rect-h
   #:ns-point
   #:ns-point*
   #:ns-point-p
   #:make-ns-point
   #:ns-point-x
   #:ns-point-y
   #:ns-size
   #:ns-size*
   #:ns-size-p
   #:make-ns-size
   #:ns-size-h
   #:ns-size-w
   #:ns-affine-transform
   #:ns-edge-insets
   #:ns-edge-insets*
   #:make-ns-edge-insets
   #:ns-edge-insets-bottom
   #:ns-edge-insets-left
   #:ns-edge-insets-right
   #:ns-edge-insets-top
   #:ns-edge-insets-p

   ;; Strings and Text
   #:ns-string
   #:ns-mutable-string
   #:as-ns-string
   #:string-to-ns-string
   #:ns-string-to-string
   #:invoke-into-string

   ;; Collections
   #:len
   #:ns-array
   #:as-ns-array
   #:ns-array-to-list
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
   #:as-ns-ordered
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
   #:ns-operating-system-version
   #:ns-operating-system-version-major
   #:ns-operating-system-version-minor
   #:ns-operating-system-version-patch
   #:ns-process-info
   #:process-name
   #:operating-system-version
   #:ns-background-activity-scheduler
   #:ns-user-notification
   #:ns-user-notification-action
   #:ns-user-notification-center

   ;; Resources
   #:ns-bundle
   #:ns-main-bundle
   #:ns-bundle-all-frameworks
   #:ns-bundle-all-bundles
   #:as-ns-bundle
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
   #:ns-main-run-loop
   #:ns-current-run-loop
   #:add-timer
   #:ns-timer
   #:validp
   #:fire-date
   #:time-interval
   #:user-info
   #:tolerance
   #:fire
   #:invalidate
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

#+slynk
(defmethod slynk:arglist-dispatch ((operator (eql 'alloc-init)) arguments)
  (flet ((find-arglist (class)
           (handler-case
               (let* ((class  (coerce-to-objc-class class))
                      (method (find-method #'init () (list class) nil)))
                 (slynk::decode-arglist (c2mop:method-lambda-list method)))
             (error () (call-next-method)))))
    (let ((name (first arguments)))
      (typecase name
        ;; 'OBJC-CLASS '"OBJC-CLASS"
        ((cons (eql quote) (cons (or symbol string) null))
         (find-arglist (second name)))
        ;; "OBJC-CLASS"
        (string (find-arglist name))
        (t      (call-next-method))))))

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
  (;;; Accessing the Parts of the URL
   ("scheme"
    :reader scheme
    :after  ns-string-to-string
    :documentation
    "The scheme. (read-only)

This property contains the scheme.
Any percent-encoded characters are not unescaped.
For example, in the URL http://www.example.com/index.html,
the scheme is http.

The full URL is the concatenation of the scheme, a colon (:), and the
value of resourceSpecifier.

Note

The term “protocol” is also sometimes used when talking about
network-based URL schemes. However, not all URL schemes are networking
protocols—data:// URLs, for example.

see https://developer.apple.com/documentation/foundation/nsurl/scheme?language=objc")
   ("user"
    :reader user
    :after  ns-string-to-string
    :documentation
    "The user name, conforming to RFC 1808.

This property contains the user name, unescaped using the
stringByReplacingPercentEscapesUsingEncoding: method. If the
receiver’s URL does not conform to RFC 1808, this property returns
nil. For example, in the URL ftp://username@www.example.com/, the user
name is username.

see https://developer.apple.com/documentation/foundation/nsurl/user?language=objc")
   ("password"
    :reader password
    :after  ns-string-to-string
    :documentation
    "The password conforming to RFC 1808. (read-only)

This property contains the password. Any percent-encoded characters
are not unescaped. If the receiver does not conform to RFC 1808, it
contains nil. For example, in the URL
http://username:password@www.example.com/index.html, the password is
password.

see https://developer.apple.com/documentation/foundation/nsurl/password?language=objc")
   ("host"
    :reader host
    :after  ns-string-to-string
    :documentation
    "The host, conforming to RFC 1808. (read-only)

This property contains the host, unescaped using the
stringByReplacingPercentEscapesUsingEncoding: method. If the receiver
does not conform to RFC 1808, this property contains nil. For example,
in the URL http://www.example.com/index.html, the host is
www.example.com.

see https://developer.apple.com/documentation/foundation/nsurl/host?language=objc")
   ("port"
    :reader port
    :documentation
    "The port, conforming to RFC 1808.

This property contains the port number. If the receiver does not
conform to RFC 1808, this property contains nil. For example, in the
URL http://www.example.com:8080/index.php, the port number is 8080.

see https://developer.apple.com/documentation/foundation/nsurl/port?language=objc")
   ("path"
    :reader path
    :after  ns-string-to-string
    :documentation
    "The path, conforming to RFC 1808. (read-only)

This property contains the path, unescaped using the
stringByReplacingPercentEscapesUsingEncoding: method. If the receiver
does not conform to RFC 1808, this property contains nil.

If the receiver contains a file or file reference URL (as determined
with fileURL), this property’s value is suitable for input into
methods of NSFileManager or NSPathUtilities. If the path has a
trailing slash, it is stripped.

If the receiver contains a file reference URL, this property’s value
provides the current path for the referenced resource, which may be
nil if the resource no longer exists.

If the parameterString property contains a non-nil value, the path may
be incomplete. If the receiver contains an unencoded semicolon, the
path property ends at the character before the semicolon. The
remainder of the URL is provided in the parameterString property.

To obtain the complete path, if parameterString contains a non-nil
value, append a semicolon, followed by the parameter string.

Per RFC 3986, the leading slash after the authority (host name and
port) portion is treated as part of the path. For example, in the URL
http://www.example.com/index.html, the path is /index.html.

see https://developer.apple.com/documentation/foundation/nsurl/path?language=objc")
   ("pathExtension"
    :reader path-extension
    :documentation
    "The path extension. (read-only)
Return the path extension of the receiver, or nil if path is nil.

This property contains the path extension, unescaped using the
stringByReplacingPercentEscapesUsingEncoding: method. For example, in
the URL file:///path/to/file.txt, the path extension is txt.

see https://developer.apple.com/documentation/foundation/nsurl/pathextension?language=objc")
   ("query"
    :reader query
    :after  ns-string-to-string
    :documentation
    "The query string, conforming to RFC 1808.

This property contains the query string. Any percent-encoded
characters are not unescaped. If the receiver does not conform to RFC
1808, this property contains nil. For example, in the URL
http://www.example.com/index.php?key1=value1&key2=value2, the query
string is key1=value1&key2=value2.

see https://developer.apple.com/documentation/foundation/nsurl/query?language=objc")
   ("fragment"
    :reader fragment
    :after  ns-string-to-string
    :documentation
    "The fragment identifier, conforming to RFC 1808. (read-only)

This property contains the URL’s fragment. Any percent-encoded
characters are not unescaped. If the receiver does not conform to RFC
1808, this property contains nil. For example, in the URL
http://www.example.com/index.html#jumpLocation, the fragment
identifier is jumpLocation.

see https://developer.apple.com/documentation/foundation/nsurl/fragment?language=objc"))
  (:documentation
   "An object that represents the location of a resource, such as an item on a
remote server or the path to a local file.

In Swift, this object bridges to URL; use NSURL when you need
reference semantics or other Foundation-specific behavior.

You can use URL objects to construct URLs and access their parts. For
URLs that represent local files, you can also manipulate properties of
those files directly, such as changing the file’s last modification
date. Finally, you can pass URL objects to other APIs to retrieve the
contents of those URLs. For example, you can use the NSURLSession,
NSURLConnection, and NSURLDownload classes to access the contents of
remote resources, as described in URL Loading System.

URL objects are the preferred way to refer to local files. Most
objects that read data from or write data to a file have methods that
accept an NSURL object instead of a pathname as the file
reference. For example, you can get the contents of a local file URL
as an NSString object using the initWithContentsOfURL:encoding:error:
initializer, or as an NSData object using the
initWithContentsOfURL:options:error: initializer.

You can also use URLs for interapplication communication. In macOS,
the NSWorkspace class provides the openURL: method to open a location
specified by a URL. Similarly, in iOS, the UIApplication class
provides the openURL:options:completionHandler: method.

Additionally, you can use URLs when working with pasteboards, as
described in NSURL Additions Reference (part of the AppKit framework).

The NSURL class is “toll-free bridged” with its Core Foundation
counterpart, CFURLRef. See Toll-Free Bridging for more information on
toll-free bridging.

Structure of a URL
An NSURL object is composed of two parts—a potentially nil base URL
and a string that is resolved relative to the base URL. An NSURL
object is considered absolute if its string part is fully resolved
without a base; all other URLs are considered relative.

For example, when constructing an NSURL object, you might specify
file:///path/to/user/ as the base URL and folder/file.html as the
string part, as follows:

NSURL *baseURL = [NSURL fileURLWithPath:@\"file:///path/to/user/\"];
NSURL *URL = [NSURL URLWithString:@\"folder/file.html\"
                    relativeToURL:baseURL];
NSLog(@\"absoluteURL = %@\", [URL absoluteURL]);

When fully resolved, the absolute URL is file:///path/to/user/folder/file.html.

A URL can be also be divided into pieces based on its structure.
For example, the URL
https://johnny:p4ssw0rd@www.example.com:443/script.ext;param=value?query=value#ref
contains the following URL components:

    Component        Value
    scheme           https
    user             johnny
    password         p4ssw0rd
    host             www.example.com
    port             443
    path             /script.ext
    pathExtension    ext
    pathComponents   [\"/\", \"script.ext\"]
    parameterString  param=value
    query            query=value
    fragment         ref

The NSURL class provides properties that let you examine each of these
components.

Important
For apps linked on or after iOS 17 and aligned OS versions, NSURL
parsing has updated from the obsolete RFC 1738/1808 parsing to the
same RFC 3986 parsing as NSURLComponents. This unifies the parsing
behaviors of the NSURL and NSURLComponents APIs. Now, NSURL
automatically percent- and IDNA-encodes invalid characters to help
create a valid URL.

To check if a URLString is strictly valid according to the RFC, use
the new [NSURL URLWithString:URLString encodingInvalidCharacters:NO]
method. This method leaves all characters as they are and returns nil
if URLString is explicitly invalid.

For apps linked before iOS 17, the NSURL class parses URLs according
to RFC 1808, RFC 1738, and RFC 2732.  Bookmarks and Security Scope

Starting with OS X v10.6 and iOS 4.0, the NSURL class provides a
facility for creating and using bookmark objects. A bookmark provides
a persistent reference to a file-system resource. When you resolve a
bookmark, you obtain a URL to the resource’s current location. A
bookmark’s association with a file-system resource (typically a file
or folder) usually continues to work if the user moves or renames the
resource, or if the user relaunches your app or restarts the system.

For a general introduction to using bookmarks, read Locating Files
Using Bookmarks in File System Programming Guide.

In a macOS app that adopts App Sandbox, you can use security-scoped
bookmarks to gain access to file-system resources outside your app’s
sandbox. These bookmarks preserve the user’s intent to give your app
access to a resource across app launches. For details on how this
works, including information on the entitlements you need in your
Xcode project, read Security-Scoped Bookmarks and Persistent Resource
Access in App Sandbox Design Guide. The methods for using
security-scoped bookmarks are described in this document in Working
with Bookmark Data.

When you resolve a security-scoped bookmark, you get a security-scoped
URL.

Security-Scoped URLs
Security-scoped URLs provide access to resources outside an app’s
sandbox. In macOS, you get access to security-scoped URLs when you
resolve a security-scoped bookmark. In iOS, apps that open or move
documents using a UIDocumentPickerViewController also receive
security-scoped URLs.

To gain access to a security-scoped URL, you must call the
startAccessingSecurityScopedResource method (or its Core Foundation
equivalent, the CFURLStartAccessingSecurityScopedResource
function). For iOS apps, if you use a UIDocument to access the URL, it
automatically manages the security-scoped URL for you.

If startAccessingSecurityScopedResource (or
CFUrLStartAccessingSecurityScopedResource) returns true, you must
relinquish your access by calling the
stopAccessingSecurityScopedResource method (or its Core Foundation
equivalent, the CFURLStopAccessingSecurityScopedResource
function). You should relinquish your access as soon as you have
finished using the file. After you call these methods, you immediately
lose access to the resource in question.

Warning
If you fail to relinquish your access when you no longer need a
file-system resource, your app leaks kernel resources. If sufficient
kernel resources are leaked, your app loses its ability to add
file-system locations to its sandbox, using Powerbox, security-scoped
bookmarks, or similar APIs, until relaunched.  Security-Scoped URLs
and String Paths

In a macOS app, when you copy a security-scoped URL, the copy has the
security scope of the original. You gain access to the file-system
resource (that the URL points to) just as you would with the original
URL: by calling the startAccessingSecurityScopedResource method (or
its Core Foundation equivalent).

If you need a security-scoped URL’s path as a string value (as
provided by the path method), such as to provide to an API that
requires a string value, obtain the path from the URL as needed. Note,
however, that a string-based path obtained from a security-scoped URL
does not have security scope and you cannot use that string to obtain
access to a security-scoped resource.  iCloud Document Thumbnails

With OS X v10.10 and iOS 8.0, the NSURL class includes the ability to
get and set document thumbnails as a resource property for iCloud
documents. You can get a dictionary of NSImage objects in macOS or
UIImage objects in iOS using the getResourceValue:forKey:error: or
getPromisedItemResourceValue:forKey:error: methods.

    ...

In macOS, you can set a dictionary of thumbnails using the
setResourceValue:forKey:error: method. You can also get or set all the
thumbnails as an NSImage object with multiple representations by using
the NSURLThumbnailKey.

    ...

Note
Do not set the NSURLThumbnailDictionaryKey key directly. Modifying
this key interferes with document tracking and can create duplicates
of your document, as well as other possible problems.

In iOS, use a UIDocument subclass to manage your file. Set the
thumbnail by overriding the document’s
fileAttributesToWriteToURL:forSaveOperation:error: method and
returning a dictionary that contains the proper thumbnail keys (along
with any other file attributes).

In macOS, follow the instructions for creating thumbnails given in
Quick Look Programming Guide.

Note
Although the thumbnail API is designed to support multiple image
resolutions, currently it only supports 1024 x 1024 pixel thumbnails.

see https://developer.apple.com/documentation/foundation/nsurl?language=objc"))

(defun ns-url-to-pathname (ns-url)
  "Convert NS-URL to lisp `pathname'. "
  (declare (type ns-url ns-url))
  (the pathname (pathname (path ns-url))))

(defun pathname-to-ns-url (pathname)
  "Convert PATHNAME to `ns-url'. "
  (declare (type (or string pathname) pathname))
  (invoke 'ns-url "fileURLWithPath:"
          (string-to-ns-string (uiop:native-namestring pathname))))

(defgeneric as-ns-url (url &key &allow-other-keys)
  (:documentation "Create `ns-url' from URL. ")
  (:method ((url pathname) &key)
    (pathname-to-ns-url url))
  (:method ((url string) &key)
    (invoke 'ns-url "URLWithString:"   (string-to-ns-string url))))

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
  "A two-dimensional size.

Normally, the values of `ns-size-w' and `ns-size-h' are non-negative.
The functions that create an `ns-size' structure do not prevent you from
setting a negative value for these attributes. If the value of width
or height is negative, however, the behavior of some methods may be
undefined.

see https://developer.apple.com/documentation/foundation/nssize?language=objc"
  (w :double :type real)
  (h :double :type real))

(define-objc-struct (ns-point "CGPoint")
  "A point in a Cartesian coordinate system.
see https://developer.apple.com/documentation/foundation/nspoint?language=objc"
  (x :double :type real)
  (y :double :type real))

(define-objc-struct (ns-rect "CGRect")
  "A rectangle.

Dev Note:
Unlike NSRect, `ns-rect' is flattened as x, y, w, h slots.

see https://developer.apple.com/documentation/foundation/nsrect?language=objc"
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

(define-objc-struct (ns-edge-insets "NSEdgeInsets")
  "A description of the distance between the edges of two rectangles.

Edge insets describe the distance between the edges of one rectangle
to a related rectangle that can be described by measuring a constant
but edge-specific distance from each edge.

A common use for this structure is to describe the relationship
between a view’s frame and its alignment rectangle.

see https://developer.apple.com/documentation/Foundation/NSEdgeInsets?language=objc"
  (bottom
   :double
   :type real
   :documentation
   "The distance from the bottom of the source rectangle to the bottom
of the result rectangle.")
  (left
   :double
   :type real
   :documentation
   "The distance from the left side of the source rectangle to the
left side of the result rectangle.")
  (right
   :double
   :type real
   :documentation
   "The distance from the right side of the source rectangle to the
right side of the result rectangle.")
  (top
   :double
   :type real
   :documentation
   "The distance from the top of the source rectangle to the top of
the result rectangle."))

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
  (declare (type (or null ns-string) ns-string))
  (the string (if ns-string (invoke ns-string "UTF8String") "")))

(defmethod objc-object-pointer ((string string))
  "Convert STRING to `ns-string' first and then pass the pointer. "
  (objc-object-pointer (string-to-ns-string string)))

(defmacro invoke-into-string (object method &rest args)
  "Call `invoke' and return string.

Dev Note:
The original METHOD should return `ns-string' and it would be turned into lisp string. "
  `(ns-string-to-string (invoke ,object ,method ,@args)))

(defgeneric as-ns-string (string)
  (:documentation "Convert STRING into NSString. ")
  (:method (default)               (string-to-ns-string (format nil "~A" default)))
  (:method ((ns-string ns-string)) ns-string)
  (:method ((string string))       (string-to-ns-string string)))


;;;; Collections
;; Use arrays, dictionaries, sets, and specialized collections to store and iterate groups of objects or values.
;; https://developer.apple.com/documentation/foundation/collections?language=objc

;;; Basic Collections

(define-objc-class "NSArray" ()
  (("count"
    :reader len
    :documentation
    "The number of objects in the array."))
  (:documentation
   "A static ordered collection of objects.
see https://developer.apple.com/documentation/foundation/nsarray?language=objc"))

(defgeneric as-ns-array (object)
  (:documentation
   "Convert OBJECT as NSArray. ")
  (:method ((list list))
    "Initializes a newly allocated array to include a given number of
objects from a given C array.

Dev Note:
This will copy LIST into foreign array, and then invoke ObjC method
initWithObjects:count: to create NSArray. "
    (let ((len (length list)))
      (cffi:with-foreign-pointer (arr (* len (cffi:foreign-type-size :pointer)))
        (loop :for i :from 0
              :for elem :in list
              :do (setf (cffi:mem-aref arr :pointer i)
                        (objc-object-pointer elem)))
        (invoke 'ns-array "initWithObjects:count:" arr len)))))

(defun ns-array-to-list (ns-array)
  "Convert an ObjC NSArray NS-ARRAY to Lisp list.
Return list.

Dev Note:
this collects objects using ObjC method objectAtIndex:"
  (declare (type ns-array ns-array))
  (loop :for i :below (len ns-array)
        :collect (invoke ns-array "objectAtIndex:" i)))

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
  (invoke 'ns-date "date"))

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

;; A structure that contains version information about the currently
;; executing operating system, including major, minor, and patch
;; version numbers.
(define-objc-struct (ns-operating-system-version "NSOperatingSystemVersion")
  (major :long)
  (minor :long)
  (patch :long))

(define-objc-class "NSProcessInfo" ()
  (("operatingSystemVersion"
    :reader operating-system-version
    :documentation "The version of the operating system on which the
process is executing.
see https://developer.apple.com/documentation/foundation/processinfo/operatingsystemversion?language=objc"))
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
   "A representation of the code and resources stored in a bundle
directory on disk.

Apple uses bundles to represent apps, frameworks, plug-ins, and many
other specific types of content. Bundles organize their contained
resources into well-defined subdirectories, and bundle structures vary
depending on the platform and the type of the bundle. By using a
bundle object, you can access a bundle’s resources without knowing the
structure of the bundle. The bundle object provides a single interface
for locating items, taking into account the bundle structure, user
preferences, available localizations, and other relevant factors.

Any executable can use a bundle object to locate resources, either
inside an app’s bundle or in a known bundle located elsewhere. You
don’t use a bundle object to locate files in a container directory or
in other parts of the file system.

The general pattern for using a bundle object is as follows:

1. Create a bundle object for the intended bundle directory.
2. Use the methods of the bundle object to locate or load the needed
   resource.
3. Use other system APIs to interact with the resource.

Some types of frequently used resources can be located and opened
without a bundle. For example, when loading images, you store images
in asset catalogs and load them using the imageNamed: methods of
UIImage or NSImage. Similarly, for string resources, you use
NSLocalizedString to load individual strings instead of loading the
entire .strings file yourself.

Note
Unlike some other Foundation classes with corresponding Core
Foundation names (such as NSString and CFStringRef), NSBundle objects
cannot be cast to CFBundleRef references. If you need functionality
provided by CFBundleRef, you can still create a CFBundleRef and use
the CFBundleRef API. See Toll-Free Bridging for more information.

Finding and Opening a Bundle
Before you can locate a resource, you must first specify which bundle
contains it. The NSBundle class has many constructors, but the one you
use most often is mainBundle. The main bundle represents the bundle
directory that contains the currently executing code. So for an app,
the main bundle object gives you access to the resources that shipped
with your app.

If your app interacts directly with plug-ins, frameworks, or other
bundled content, you can use other methods of this class to create
appropriate bundle objects. You can always create bundle objects from
a known URL or path, but other methods make it easier to access
bundles your app is already using. For example, if you link to a
framework, you can use the bundleForClass: method to locate the
framework bundle based on a class defined in that framework.

    ;; Get the app's main bundle
    (ns-main-bundle)
    ;; Get the bundle containing the specified private class
    (as-ns-bundle (coerce-to-objc-class 'my-private-class))

You use NSBundle objects to obtain the location of specific resources
inside the bundle. When looking for resources, you provide the name of
the resource and its type at a minimum. For resources in a specific
subdirectory, you can also specify that directory. After locating the
resource, the bundle routines return a path string or URL that you can
use to open the file.

Locating a single resource in a bundle

    (as-ns-bundle #P\"Seagull\" :type :jpg)

Bundle objects follow a specific search pattern when looking for
resources on disk. Global resources—that is, resources not in a
language-specific .lproj directory—are returned first, followed by
region- and language-specific resources. This search pattern means
that the bundle looks for resources in the following order:

1. Global (nonlocalized) resources
2. Region-specific localized resources (based on the user’s region
   preferences)
3. Language-specific localized resources (based on the user’s
   language preferences)
4. Development language resources (as specified by the
   CFBundleDevelopmentRegion key in the bundle’s Info.plist file)

Because global resources take precedence over language-specific
resources, you should never include both a global and localized
version of a given resource in your app. When a global version of a
resource exists, language-specific versions are never returned. The
reason for this precedence is performance. If localized resources were
searched first, the bundle object might waste time searching for a
nonexistent localized resource before returning the global resource.

Important
Bundle objects always consider case when searching for resource files,
even on file systems that support case-insensitive filenames. Always
make sure that you specify filenames with case sensitivity in mind.

When locating resource files, the bundle object automatically
considers many standard filename modifiers when determining which file
to return. Resources may be tagged for a specific device (~iphone,
~ipad) or for a specific screen resolution (@2x, @3x). Do not include
these modifiers when specifying the name of the resource you want. The
bundle object selects the file that is most appropriate for the
underlying device. For more information, see App Icons on iPhone, iPad
and Apple Watch.

Understanding Bundle Structures
Bundle structures vary depending on the target platform and the type
of bundle you are building. The NSBundle class hides this underlying
structure in most (but not all) cases. Many of the methods you use to
load resources from a bundle automatically locate the appropriate
starting directory and look for resources in known places. You can
also use the methods and properties of this class to get the location
of known bundle directories and to retrieve resources specifically
from those directories.

For information about the bundle structure of iOS and macOS apps, see
Bundle Programming Guide. For information about the structure of
framework bundles, see Framework Programming Guide. For information
about the structure of macOS plug-ins, see Code Loading Programming
Topics.

see https://developer.apple.com/documentation/foundation/bundle?language=objc"))

;; Getting Standard Bundle Objects

(declaim (type (or null ns-bundle) *ns-main-bundle*))
(defparameter *ns-main-bundle* nil
  "The bundle object contains the current executable. ")

(defun ns-main-bundle ()
  "Returns the bundle object that contains the current executable.
see https://developer.apple.com/documentation/foundation/bundle/main?language=objc"
  (or *ns-main-bundle*
      (setf *ns-main-bundle* (invoke 'ns-bundle "mainBundle"))))

(define-coca-init :pre (setf *ns-main-bundle* nil))

(defun ns-bundle-all-frameworks ()
  "Returns a list of all of the application’s bundles that represent frameworks.

Return a list of all of the application’s bundles that represent
frameworks. Only frameworks with one or more Objective-C classes in
them are included.

The returned array includes frameworks that are linked into an
application when the application is built and bundles for frameworks
that have been dynamically created.

see https://developer.apple.com/documentation/foundation/bundle/allframeworks?language=objc"
  (ns-array-to-list (invoke 'ns-bundle "allFrameworks")))

(defun ns-bundle-all-bundles ()
  "Returns a list of all the application’s non-framework bundles.

The returned array includes the main bundle and all bundles that have
been dynamically created but doesn’t contain any bundles that
represent frameworks.

see https://developer.apple.com/documentation/foundation/bundle/allbundles?language=objc"
  (ns-array-to-list (invoke 'ns-bundle "allBundles")))

;; Creating and Initializing a Bundle

;; TODO: more
(defgeneric as-ns-bundle (bundle &key &allow-other-keys)
  (:documentation "Convert BUNDLE into `ns-bundle'. ")
  (:method ((bundle ns-bundle) &key) bundle)
  (:method ((path pathname) &key)
    "Returns an NSBundle object that corresponds to the specified directory.

This method allocates and initializes the returned object if there is
no existing NSBundle associated with path, in which case it returns
the existing object.

Dev Note:
invoke bundleWithPath:

see https://developer.apple.com/documentation/foundation/nsbundle/bundlewithpath:?language=objc"
    (invoke 'ns-bundle
            "bundleWithPath:"
            (string-to-ns-string (uiop:native-namestring path)))))

;; Loading Nib Files

;; Finding Resource Files

;; Finding Image Resources

;; Finding Sound Resources

;; Fetching Localized Strings

;; Fetching Context Help Resources

;; Getting the Standard Bundle Directories

;; Getting Bundle Information

;; Getting Localization Information

;; Managing Preservation Priority for On Demand Resources

;; Getting Classes from a Bundle

;; Loading Code from a Bundle

;; Errors

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

(defun ns-main-run-loop ()
  "Returns the run loop of the main thread. "
  (invoke 'ns-run-loop "mainRunLoop"))

(defun ns-current-run-loop ()
  "Returns the run loop for the current thread. "
  (invoke 'ns-run-loop "currentRunLoop"))

(defmethod add-timer ((run-loop ns-run-loop) (timer ns-timer) (mode ns-string))
  "Registers a given timer with a given input mode.
Return TIMER, the `ns-timer'.

Parameters:
+ TIMER:
  The timer to register with the receiver
+ MODE:
  The mode in which to add aTimer.
  You may specify a custom mode or use one of the modes
  listed in Run Loop Modes.

  Possible Values:
  + `+ns-run-loop-common-modes+'
  + `+ns-default-run-loop-mode+'
"
  (invoke run-loop "addTimer:forMode:" timer mode)
  timer)

(defmethod add-timer (run-loop timer (mode (eql :default)))
  (add-timer run-loop timer +ns-default-run-loop-mode+))

(defmethod add-timer (run-loop timer (mode (eql :common)))
  (add-timer run-loop timer +ns-run-loop-common-modes+))

(define-objc-class "NSTimer" ()
  (("valid"
    :reader validp
    :documentation
    "A Boolean value that indicates whether the timer is currently valid.
see https://developer.apple.com/documentation/foundation/timer/isvalid?language=objc")
   ("fireDate"
    :reader fire-date
    :documentation
    "The date at which the timer will fire.
see https://developer.apple.com/documentation/foundation/timer/firedate?language=objc")
   ("timeInterval"
    :reader time-interval
    :documentation
    "The timer’s time interval, in seconds.
see https://developer.apple.com/documentation/foundation/timer/timeinterval?language=objc")
   ("userInfo"
    :reader user-info
    :documentation
    "The receiver’s userInfo object.
see https://developer.apple.com/documentation/foundation/timer/userinfo?language=objc")
   ("tolerance"
    :accessor tolerance
    :documentation
    "The amount of time after the scheduled fire date that the timer may fire.

The default value is zero, which means no additional tolerance is
applied.

Setting a tolerance for a timer allows it to fire later than the
scheduled fire date. Allowing the system flexibility in when a timer
fires increases the ability of the system to optimize for increased
power savings and responsiveness.

The timer may fire at any time between its scheduled fire date and the
scheduled fire date plus the tolerance. The timer will not fire before
the scheduled fire date. For repeating timers, the next fire date is
calculated from the original fire date regardless of tolerance applied
at individual fire times, to avoid drift. The system reserves the
right to apply a small amount of tolerance to certain timers
regardless of the value of this property.

see https://developer.apple.com/documentation/foundation/timer/tolerance?language=objc"))
  (:documentation
   "A timer that fires after a certain time interval has elapsed,
sending a specified message to a target object.

Timers work in conjunction with run loops. Run loops maintain strong
references to their timers, so you don’t have to maintain your own
strong reference to a timer after you have added it to a run loop.

To use a timer effectively, you should be aware of how run loops
operate. See Threading Programming Guide for more information.

A timer is not a real-time mechanism. If a timer’s firing time occurs
during a long run loop callout or while the run loop is in a mode that
isn’t monitoring the timer, the timer doesn’t fire until the next time
the run loop checks the timer. Therefore, the actual time at which a
timer fires can be significantly later. See also Timer Tolerance.

NSTimer is toll-free bridged with its Core Foundation counterpart,
CFRunLoopTimerRef. See Toll-Free Bridging for more information.
Comparing Repeating and Nonrepeating Timers

You specify whether a timer is repeating or nonrepeating at creation
time. A nonrepeating timer fires once and then invalidates itself
automatically, thereby preventing the timer from firing again. By
contrast, a repeating timer fires and then reschedules itself on the
same run loop. A repeating timer always schedules itself based on the
scheduled firing time, as opposed to the actual firing time. For
example, if a timer is scheduled to fire at a particular time and
every 5 seconds after that, the scheduled firing time will always fall
on the original 5-second time intervals, even if the actual firing
time gets delayed. If the firing time is delayed so far that it passes
one or more of the scheduled firing times, the timer is fired only
once for that time period; the timer is then rescheduled, after
firing, for the next scheduled firing time in the future.  Timer
Tolerance

In iOS 7 and later and macOS 10.9 and later, you can specify a
tolerance for a timer (tolerance). This flexibility in when a timer
fires improves the system’s ability to optimize for increased power
savings and responsiveness. The timer may fire at any time between its
scheduled fire date and the scheduled fire date plus the
tolerance. The timer doesn’t fire before the scheduled fire date. For
repeating timers, the next fire date is calculated from the original
fire date regardless of tolerance applied at individual fire times, to
avoid drift. The default value is zero, which means no additional
tolerance is applied. The system reserves the right to apply a small
amount of tolerance to certain timers regardless of the value of the
tolerance property.

As the user of the timer, you can determine the appropriate tolerance
for a timer. A general rule, set the tolerance to at least 10% of the
interval, for a repeating timer. Even a small amount of tolerance has
significant positive impact on the power usage of your
application. The system may enforce a maximum value for the tolerance.
Scheduling Timers in Run Loops

You can register a timer in only one run loop at a time, although it
can be added to multiple run loop modes within that run loop. There
are three ways to create a timer:

+ Use the scheduledTimerWithTimeInterval:invocation:repeats: or
  scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:
  class method to create the timer and schedule it on the current
  run loop in the default mode.
+ Use the timerWithTimeInterval:invocation:repeats: or
  timerWithTimeInterval:target:selector:userInfo:repeats: class
  method to create the timer object without scheduling it on a run
  loop. (After creating it, you must add the timer to a run loop
  manually by calling the addTimer:forMode: method of the
  corresponding NSRunLoop object.)
+ Allocate the timer and initialize it using the
  initWithFireDate:interval:target:selector:userInfo:repeats:
  method. (After creating it, you must add the timer to a run loop
  manually by calling the addTimer:forMode: method of the
  corresponding NSRunLoop object.)

Once scheduled on a run loop, the timer fires at the specified
interval until it is invalidated. A nonrepeating timer invalidates
itself immediately after it fires. However, for a repeating timer, you
must invalidate the timer object yourself by calling its invalidate
method. Calling this method requests the removal of the timer from the
current run loop; as a result, you should always call the invalidate
method from the same thread on which the timer was
installed. Invalidating the timer immediately disables it so that it
no longer affects the run loop. The run loop then removes the timer
(and the strong reference it had to the timer), either just before the
invalidate method returns or at some later point. Once invalidated,
timer objects cannot be reused.

After a repeating timer fires, it schedules the next firing for the
nearest future date that is an integer multiple of the timer interval
after the last scheduled fire date, within the specified tolerance. If
the time taken to call out to perform a selector or invocation is
longer than the specified interval, the timer schedules only the next
firing; that is, the timer doesn’t attempt to compensate for any
missed firings that would have occurred while calling the specified
selector or invocation.  Subclassing Notes

Do not subclass NSTimer.

see https://developer.apple.com/documentation/foundation/timer?language=objc"))

;;; TODO: FIXME
;; not working, not sure why
(defmethod init ((timer ns-timer)
                 &key
                   (fire-date     (ns-date-now))
                   (time-interval 1.0           interval?)
                   (repeats-p     interval?)
                   (callback      nil           callback?)
                   (target        :self)
                   action
                   user-info
                   run-loop
                   (run-loop-mode :default))
  "Initialize TIMER `ns-timer'.

Parameters:
+ FIRE-DATE (`ns-date')
  time at which the timer should first fire
  default as `ns-date-now'
+ TIME-INTERVAL:
  For a repeating timer, this parameter contains the number of
  seconds between firings of the timer.
  If interval is less than or equal to 0.0, this method chooses
  the nonnegative value of 0.0001 seconds instead.
+ REPEATS-P
  if TIMER repeats,
  if setting with TIME-INTERVAL, the REPEATS-P would be `t'
+ CALLBACK
  a callback cffi function pointer,
  if setting, will invoke initWithFireDate:interval:repeats:block:
+ TARGET (default `:self')
  default as TIMER self
+ ACTION
  The message to send to target when the timer fires.

  The selector should have the following signature: timerFireMethod:
  (including a colon to indicate that the method takes an
  argument). The timer passes itself as the argument, thus the method
  would adopt the following pattern:
+ USER-INFO
  Custom user info for the timer.
  The timer maintains a strong reference to this object until it
  (the timer) is invalidated. This parameter may be nil.
+ RUN-LOOP (`ns-run-loop')
  run loop to add TIMER
  if nil, ignore (not add) (default);

  Other possible values:
  + `:current' `ns-current-run-loop'
  + `:main'    `ns-main-run-loop'
+ RUN-LOOP-MODE
  run mode `ns-string'

  Possible values:
  + `:default', `+ns-default-run-loop-mode+'
  + `:common',  `+ns-run-loop-common-modes+'
"
  (declare (type ns-date fire-date)
           (type real    time-interval)
           (type (or symbol cffi:foreign-pointer) callback)
           (type (or null (eql :self) ns-object)  target)
           (type (or null ns-object)              user-info)
           (type (or nil ns-run-loop (member :current :main)) run-loop))
  (if callback?
      (invoke timer
              "initWithFireDate:interval:repeats:block:"
              fire-date
              (coerce time-interval 'double-float)
              (as-boolean repeats-p)
              (etypecase callback
                (symbol               (cffi:get-callback callback))
                (cffi:foreign-pointer callback)))
      (invoke timer
              "initWithFireDate:interval:target:selector:userInfo:repeats:"
              fire-date
              (coerce time-interval 'double-float)
              (if (eq target :self) timer target)
              (coerce-to-selector action)
              user-info
              (as-boolean repeats-p)))
  (etypecase run-loop
    (null           nil)
    (ns-run-loop    (add-timer run-loop              timer run-loop-mode))
    ((eql :current) (add-timer (ns-current-run-loop) timer run-loop-mode))
    ((eql :main)    (add-timer (ns-main-run-loop)    timer run-loop-mode))))

(defmethod fire ((timer ns-timer))
  "Causes the timer’s message to be sent to its target. "
  (invoke timer "fire"))

(defmethod invalidate ((timer ns-timer))
  "Stops the timer from ever firing again and requests its removal
from its run loop."
  (invoke timer "invalidate"))

;;; Process Info

(define-objc-class "NSProcessInfo" ()
  (("processName"
    :reader process-name
    :after  ns-string-to-string
    :documentation
    "The name of the process.

The process name is used to register application defaults and is used
in error messages. It does not uniquely identify the process.

Warning
User defaults and other aspects of the environment might depend on the
process name, so be very careful if you change it. Setting the process
name in this manner is not thread safe.

see https://developer.apple.com/documentation/foundation/processinfo/processname?language=objc"))
  (:documentation
   "A collection of information about the current process.

Each process has a single, shared NSProcessInfo object known as a
process information agent that can return information such as
arguments, environment variables, host name, and process name. The
processInfo class method returns the shared agent for the current
process. For example, the following line returns the NSProcessInfo
object, which then provides the name of the current process:

    (process-name (ns-process-info))

Note
NSProcessInfo is thread-safe in macOS 10.7 and later.

The NSProcessInfo class also includes the operatingSystemVersion
property, which returns an NSOperatingSystemVersion structure
identifying the operating system version on which the process is
executing.

NSProcessInfo objects attempt to interpret environment variables and
command-line arguments in the user’s default C string encoding if they
can’t convert to Unicode as UTF-8 strings. If neither the Unicode nor
C string conversion works, the NSProcessInfo object ignores these
values.

Manage Activities
The system has heuristics to improve battery life, performance, and
responsiveness of applications for the benefit of the user. You can
use the following methods to manage activities that give hints to the
system that your application has special requirements:

+ beginActivityWithOptions:reason:
+ endActivity:
+ performActivityWithOptions:reason:usingBlock:

In response to creating an activity, the system disables some or all
of the heuristics so your application can finish quickly while still
providing responsive behavior if the user needs it.

You use activities when your application performs a long-running
operation. If the activity can take different amounts of time (for
example, calculating the next move in a chess game), it should use
this API to ensure correct behavior when the amount of data or the
capabilities of the user’s computer varies. Activities fall into two
major categories:

+ User-initiated activities are explicitly started by the
  user. Examples include exporting or downloading a user-specified
  file.
+ Background activities perform the normal operations of your
  application and aren’t explicitly started by the user. Examples
  include autosaving, indexing, and automatic downloading of files.

In addition, if your application requires high priority input/output
(I/O), you can include the NSActivityLatencyCritical flag (using a
bitwise OR). You should only use this flag for activities like audio
or video recording that require high priority I/O.

If your activity takes place synchronously inside an event callback on
the main thread, you don’t need to use this API.

Be aware that failing to end these activities for an extended period
of time can have significant negative impacts on the performance of
your user’s computer, so be sure to use only the minimum amount of
time required. User preferences may override your application’s
request.

You can also use this API to control automatic termination or sudden
termination (see Support Sudden Termination). For example, the
following code brackets the work to protect it from sudden
termination:

    (let ((activity (begin-activity (ns-process-info)
                                    :options :automatic-termination-disabled
                                    :reason  \"Good Reason\")))
      (end-activity activity))

The above example is equivalent to the following code, which uses the
disableAutomaticTermination: method:

    (disable-automatic-termination (ns-process-info) \"Good Reason\")
    (enable-automatic-termination  (ns-process-info) \"Good Reason\")

Because this API returns an object, it may be easier to pair begins
and ends than when using the automatic termination API. If your app
deallocates the object before the endActivity: call, the activity ends
automatically.

This API also provides a mechanism to disable system-wide idle sleep
and display idle sleep. These can have a large impact on the user
experience, so be careful to end activities that disable sleep
(including NSActivityUserInitiated).

Support Sudden Termination
macOS 10.6 and later includes a mechanism that allows the system to
log out or shut down more quickly by, whenever possible, killing
applications instead of requesting that they quit themselves.

Your application can enable this capability on a global basis and then
manually override its availability during actions that could cause
data corruption or a poor user experience by allowing sudden
termination.

Alternatively, your application can manually enable and disable this
functionality. Creating a process assigns a counter that indicates if
the process is safe to terminate. You decrement and increment the
counter using the methods enableSuddenTermination and
disableSuddenTermination. A value of 0 enables the system to terminate
the process without first sending a notification or event.

Your application can support sudden termination upon launch by adding
a key to the application’s Info.plist file. If the
NSSupportsSuddenTermination key exists in the Info.plist file and has
a value of true, it’s the equivalent of calling
enableSuddenTermination during your application launch. This allows
the system to terminate the process immediately. You can still
override this behavior by invoking disableSuddenTermination.

Typically, you disable sudden termination whenever your app defers
work that the app must complete before it terminates. If, for example,
your app defers writing data to disk and enables sudden termination,
you should bracket the sensitive operations with a call to
disableSuddenTermination, perform the necessary operations, and then
send a balancing enableSuddenTermination message.

In agents or daemon executables that don’t depend on AppKit, you can
manually invoke enableSuddenTermination right away. You can then use
the enable and disable methods whenever the process has work it must
do before it terminates.

Some AppKit functionality automatically disables sudden termination on
a temporary basis to ensure data integrity.

+ NSUserDefaults temporarily disables sudden termination to prevent
  the process from terminating between the time at which it sets the
  default and the time at which it writes the preferences file —
  including that default — to disk.
+ NSDocument temporarily disables sudden termination to prevent the
  process from terminating between the time at which the user has
  made a change to a document and the time at which NSDocument
  writes the user’s change to disk.

Tip
You can determine the value of the sudden termination using the
following LLDB command.

    (invoke (ns-process-info) \"_suddenTerminationDisablingCount\")

Don’t attempt to invoke or override suddenTerminationDisablingCount (a
private method) in your application. It’s there for this debugging
purpose and may disappear at any time.  Monitor Thermal State to
Adjust App Performance

Thermal state indicates the level of heat generated by logic
components as they run apps. As the thermal state increases, the
system decreases heat by reducing the speed of the
processors. Optimize your app’s performance by monitoring the thermal
state and reducing system usage as the thermal state increases. Query
the current state with thermalState to determine if your app needs to
reduce system usage. You can register the
NSProcessInfoThermalStateDidChangeNotification for notifications of a
change in thermal state. For recommended actions, see
NSProcessInfoThermalState.

see https://developer.apple.com/documentation/foundation/processinfo?language=objc"))

(defparameter *ns-process-info* nil
  "The process information agent for the process.")

(defun ns-process-info ()
  "Returns the process information agent for the process.

An NSProcessInfo object is created the first time this method is
invoked, and that same object is returned on each subsequent
invocation.

see https://developer.apple.com/documentation/foundation/processinfo/processinfo?language=objc"
  (or *ns-process-info*
      (setf *ns-process-info* (invoke 'ns-process-info "processInfo"))))

(define-coca-init :pre (setf *ns-process-info* nil))

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
