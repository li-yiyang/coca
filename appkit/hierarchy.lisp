;;;; hierarchy.lisp --- Manages hierarchy relationship

(in-package :coca.appkit)

(defgeneric parent (view)
  (:documentation
   "Return the parent `obj' of VIEW. "))

(defgeneric children (view)
  (:documentation
   "Return a list of children `obj' of VIEW. "))

;;;; hierarchy.lisp ends here
