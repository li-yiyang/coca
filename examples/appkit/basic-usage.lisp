;;;; basic-usage.lisp --- Basic examples using Coca.AppKit

(uiop:define-package :coca.example.appkit.basic-usage
  (:use :common-lisp :coca.appkit))

(in-package :coca.example.appkit.basic-usage)

(defvar win
  (make-instance 'window
                 :title "My Window"
                 :width 400 :height 400))

(make-instance 'window :size #(400 400))

(defvar but
  (make-instance 'button
                 :title  "Button"
                 :frame  #(20 20 350 30)
                 :parent win
                 :font   (make-font :family :fix)
                 :action (lambda (self sender)
                           (alert (format nil "Clicked ~A (sender=~A)"
                                          self sender)))))
