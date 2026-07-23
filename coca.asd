;;;; coca.asd ---- Portable ObjC CFFI binding

(defsystem #:coca
  :author ("凉凉")
  :license "LGPL"
  :version "0.0.1"
  :description "Toolbox for programming ObjC Cocoa in Lisp"
  :depends-on (#:coca/objc
               #:coca/app))

(defsystem #:coca/objc
  :author ("凉凉")
  :license "LGPL"
  :version "0.0.2"
  :description "Very very very thin ObjC wrapper. "
  :depends-on (#:cffi                   ; ObjC runtime in CFFI binding
               #:cffi-libffi            ; pass/return struct as values
               #:trivial-main-thread    ; NSApp runs under main thread
               #:trivial-garbage        ; release when GCed in lisp
               #:trivia                 ; pattern matching style
               #:str
               #:alexandria)
  :pathname "objc"
  :components
  ((:file "package")
   (:file "typing"    :depends-on ("package"))
   (:file "resources" :depends-on ("typing"))
   (:file "invoke"    :depends-on ("typing"))
   (:file "block"     :depends-on ("resources" "invoke"))))

(defsystem #:coca/app
  :author ("凉凉")
  :license "LGPL"
  :version "0.0.1"
  :description "Simple NSApp management"
  :depends-on (#:coca/objc)
  :pathname "app"
  ;; TODO: build/release Coca.app
  ;; :defsystem-depends-on (#:deploy)
  ;; :build-operation "osx-app-deploy-op"
  ;; :build-pathname  "Coca"
  ;; :entry-point     "coca.app::entry"
  :components
  ((:file "package")
   (:file "app" :depends-on ("package"))
   ;; TODO: menu, dock, ...
   ))

;;;; coca.asd ends here
