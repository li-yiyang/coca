;;;; coca.asd ---- Portable ObjC CFFI binding

(defsystem #:coca
  :author ("凉凉")
  :license "LGPL"
  :version "0.0.1"
  :description "Cocoa in Common Lisp"
  :depends-on (#:cffi                   ; ObjC runtime in CFFI binding
               #:cffi-libffi            ; pass/return struct as values
               #:trivial-main-thread    ; NSApp runs under main thread
               #:trivial-garbage        ; release when GCed in lisp
               #:trivial-gray-streams   ; simple-view <- output-stream
               #:trivia                 ; pattern matching style
               #:str)
  :components
  ((:module "objc"
    :description "Portable ObjC CFFI binding"
    :components
    ((:file "package")
     (:file "typing"    :depends-on ("package"))
     (:file "resources" :depends-on ("typing"))
     (:file "invoke"    :depends-on ("typing"))))
   (:module "cocoa"
    :depends-on ("objc")
    :description "Thin Cocoa wrapper on ObjC"
    :components
    ((:file "package")
     (:file "obj"     :depends-on ("package"))
     (:file "app"     :depends-on ("obj"))
     (:file "block"   :depends-on ("obj"))
     (:file "view"    :depends-on ("obj"))
     (:file "screen"  :depends-on ("obj"))
     (:file "window"  :depends-on ("view" "screen"))
     #+coca.todo (:file "color"       :depends-on ("obj"))
     #+coca.todo (:file "font"        :depends-on ("obj"))
     #+coca.todo (:file "menu"        :depends-on ("obj"))
     #+coca.todo (:file "windoid"     :depends-on ("view"))
     #+coca.todo (:file "dialog"      :depends-on ("windows"))
     #+coca.todo (:file "dialog-item" :depends-on ("view"))))))

#+coca.todo
(defsystem #:coca/app
  :author ("凉凉")
  :license "GPL"
  :version "0"
  :description "Coca, the Common Lisp IDE on macOS"
  :depends-on (#:coca)
  :defsystem-depends-on (#:deploy)
  :pathname "app"
  :build-operation "osx-app-deploy-op"
  :build-pathname  "Coca"
  :entry-point     "coca.app::entry"
  :components
  ((:file "package")
   (:file "application")
   (:file "entry")))

;;;; coca.asd ends here
