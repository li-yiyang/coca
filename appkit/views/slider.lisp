;;;; slider.lisp --- Wrapper of NSSlider

(in-package :coca.appkit)

(define-coca-base-view (slider "NSSlider"
                               target-mixin
                               double-value-mixin)
  (:objc-property
   (vertical-slider-p
    :bool ("isVertical" :read-only)
    "If or not SLIDER is vertical. "))
  (:documentation
   "A slider. "))

;;;; slider.lisp ends here
