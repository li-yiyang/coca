;;;; slider.lisp --- Wrapper of NSSlider

(in-package :coca.appkit)

(defclass slider (base-view
                  target-mixin
                  double-value-mixin)
  ()
  (:documentation
   "A slider. ")
  (:default-initargs
   :objc-class "NSSlider"))

;;;; slider.lisp ends here
