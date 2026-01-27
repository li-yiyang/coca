;;;; utils.lisp --- Helper functions of coca

(in-package :coca.objc)

(define-condition objc-error (error) ()
  (:documentation "Base class of ObjC conditions. "))

(defmacro with-cached ((key cache) &body body)
  "Cached BODY return value in CACHE by KEY. "
  (let ((k (gensym "KEY")))
    `(let ((,k ,key))
       (or (gethash ,k ,cache)
           (setf (gethash ,k ,cache)
                 (progn ,@body))))))

(defun atomize (object &optional (part #'car))
  "If OBJECT is `list', return (funcall PART OBJECT), otherwise return OBJECT. "
  (declare (type function part))
  (if (listp object) (funcall part object) object))

(defun listfy (object &optional (wrap #'list))
  "If OBJECT is `atom', return (funcall WRAP OBJECT), otherwise return OBJECT. "
  (declare (type function wrap))
  (if (atom object) (funcall wrap object) object))

(defun select-plist (plist &rest keys)
  "Select sub-plist of PLIST by KEYS.
Return a new plist. "
  (loop :for (key . val-rest) :on plist :by #'cddr
        :if (endp val-rest)
          :do (error "Invalid plist, want even length but got:~%~S. " plist)
        :if (member key keys)
          :collect key
          :and :collect (first val-rest)))

(defun remove-plist (plist &rest keys)
  "Remove KEYS from PLIST.
Return a new plist. "
  (loop :for (key . val-rest) :on plist :by #'cddr
        :if (endp val-rest)
          :do (error "Invalid plist, want even length but got:~%~S. " plist)
        :if (not (member key keys))
          :collect key
          :and :collect (first val-rest)))

(defun select-alist (alist &rest keys)
  "Select sub-alist of ALIST by KEYS.
Return a new alist. "
  (loop :for (car . cdr) :in alist
        :if (member car keys)
          :collect (cons car cdr)))

(defun remove-alist (alist &rest keys)
  "Remove elements from ALIST by KEYS.
Return a new alist. "
  (loop :for (car . cdr) :in alist
        :if (not (member car keys))
          :collect (cons car cdr)))

(defun objc-intern (name &optional (package *package*))
  "Intern ObjC NAME as lisp symbol.
Return a lisp symbol in PACKAGE.

Parameters:
+ NAME: string of ObjC name
+ PACKAGE: package to intern the symbol

Rules:
1. if name is prefixed with `_', then `%' would be added to the result symbol
   for example: _InternalClassName would be `%internal-class-name';
2. name would be transformed using `str:param-case';
"
  (declare (type string name))
  (str:match name
    ;; Black list: not conflicts with CL symbols
    ;; should add more later
    (("count")    (intern "COUNTS" package))
    (("length")   (intern "LEN"    package))

    ;; Prefix escape
    (("NS" name)  (intern (str:concat "NS-" (str:upcase (str:param-case name))) package))
    (("CG" name)  (intern (str:concat "CG-" (str:upcase (str:param-case name))) package))
    ;; FIX: frameworks;webkit.lisp
    (("WK" name)  (intern (str:concat "WK-" (str:upcase (str:param-case name))) package))
    ;; FIX: frameworks;appkit.lisp
    (("URL" name) (intern (str:concat "URL-" (str:upcase (str:param-case name))) package))
    (("UT"  name) (intern (str:concat "UT-" (str:upcase (str:param-case name))) package))

    ;; internal/private
    (("__" name)  (intern (str:concat "%%"  (str:upcase (str:param-case name))) package))
    (("_"  name)  (intern (str:concat "%"   (str:upcase (str:param-case name))) package))

    (t            (intern (str:upcase (str:param-case name))                    package))))

(defun tree-find-if (tree pred)
  "Search by PRED in TREE.

Parameters:
+ TREE: list or atom
+ PRED: test function"
  (declare (type function pred))
  (if (funcall pred tree) t
      (if (listp tree)
          (find-if (lambda (elem) (tree-find-if elem pred)) tree)
          nil)))

(defun as-boolean (value)
  "Convert VALUE as boolean value. "
  (and value t))

;;;; utils.lisp ends here
