;;;; views.lisp --- Wrapper of NSViews-like widgets

(in-package :coca.appkit)

(defclass button (view)
  ()
  (:documentation
   "Wrapper of NSButton. ")
  (:default-initargs
   :objc-class "NSButton"))

;;;; views.lisp ends here
