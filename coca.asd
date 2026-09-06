;;;; coca.asd ---- Portable ObjC CFFI binding

(defsystem #:coca
  :author ("凉凉")
  :license "LGPL"
  :version "0.0.1"
  :description "Toolbox for programming ObjC Cocoa in Lisp"
  :depends-on (#:coca/objc
               #:coca/objc/block
               #:coca/app
               #:coca/appkit))

(defsystem #:coca/objc
  :author ("凉凉")
  :license "LGPL"
  :version "0.0.3"
  :description "Very very very thin ObjC wrapper. "
  :depends-on (#:cffi        ; ObjC runtime in CFFI binding
               #:trivia      ; pattern matching style
               #:alexandria)
  :pathname "objc"
  :components
  ((:file "package")
   (:file "typing"    :depends-on ("package"))
   (:file "resources" :depends-on ("typing"))
   (:file "invoke"    :depends-on ("typing"))))

(defsystem #:coca/objc/block
  :author ("凉凉")
  :license "LGPL"
  :version "0.0.2"
  :description "ObjC Block support"
  :depends-on (#:coca/objc
               #:bordeaux-threads)
  :pathname "objc"
  :components
  ((:file "block")))

(defsystem #:coca/app
  :author ("凉凉")
  :license "LGPL"
  :version "0.0.1"
  :description "Simple NSApp management"
  :depends-on (#:coca/objc
               ;; NSApp runs under main thread
               #:trivial-main-thread)
  :pathname "app"
  ;; TODO: build/release Coca.app
  ;; :defsystem-depends-on (#:deploy)
  ;; :build-operation "osx-app-deploy-op"
  ;; :build-pathname  "Coca"
  ;; :entry-point     "coca.app::entry"
  :components
  ((:file "package")
   (:file "app" :depends-on ("package"))))

(defsystem #:coca/core-graphics
  :author ("凉凉")
  :license "LGPL"
  :version "0.0.0"
  :description "Introduce CoreGraphics"
  :depends-on (#:coca/objc)
  :pathname "core-graphics"
  :components
  ((:file "package")))

(defsystem #:coca/appkit
  :author ("凉凉")
  :license "LGPL"
  :version "0.0.1"
  :description "Mixins to manipulate AppKit"
  :depends-on (#:coca/objc
               #:coca/app
               #:coca/core-graphics
               ;; return struct as values
               #:cffi-libffi
               ;; owned-mixin
               #:trivial-garbage)
  :pathname "appkit"
  :components
  ((:file "package")
   (:file "typing"    :depends-on ("package"))
   (:file "obj"       :depends-on ("package"))
   (:file "hierarchy" :depends-on ("package"))
   (:file "subview"   :depends-on ("hierarchy"))
   (:file "named"     :depends-on ("package"))
   (:file "visible"   :depends-on ("package"))
   (:file "titled"    :depends-on ("obj"))
   (:file "framed"    :depends-on ("obj" "typing"))
   (:file "screen"    :depends-on ("obj" "framed" "named"))
   (:file "window"    :depends-on ("screen" "visible" "subview"))
   (:file "views"     :depends-on ("subview"))))

(defsystem #:coca/metal
  :author ("凉凉")
  :license "LGPL"
  :version "0.0.1"
  :description "Metal API"
  :depends-on (#:cffi-libffi
               #:coca/objc
               ;; In macOS, in order for the system to provide
               ;; a default Metal device object, you need to link
               ;; to the Core Graphics framework.
               ;;
               ;; You usually need to do this explicitly if you’re
               ;; writing apps that don’t use graphics by default,
               ;; such as command line tools.
               #:coca/core-graphics
               #:trivial-garbage)
  :pathname "metal"
  :components
  ((:file "package")
   (:file "typing"    :depends-on ("package"))
   (:file "device"    :depends-on ("package"))
   (:file "resources" :depends-on ("device"))
   (:file "command"   :depends-on ("device"))
   (:file "pipeline"  :depends-on ("device" "command" "typing"))))

;;;; coca.asd ends here
