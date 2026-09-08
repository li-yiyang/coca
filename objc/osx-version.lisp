;;;; osx-version.lisp --- Utils for version check

(in-package :coca.objc)

;; Dev Note:
;; use `osx-version>=' in code to validate OSX version
;; like:
;;
;;    #+(osx-version>= 15) ;; read time
;;    (code ...)
;;
;; or
;;
;;    (if (osx-version>= ...) ;; run time
;;        (code ...)
;;        (code ...))
;;

(define-objc-global-variable %osx-version
    (invoke (invoke "NSProcessInfo" "processInfo" :object)
            "operatingSystemVersion"
            (:struct %ns-operating-system-version)))

(declaim (inline osx-version))
(defun osx-version ()
  "Return values as MAJOR MINOR PATCH version of current os version. "
  (values-list (%osx-version)))

(defvar +osx-version-names+
  '((:golden-gate . 27)
    (:tahoe       . 26)
    (:sequoia     . 15)
    (:sonoma      . 14)
    (:ventura     . 13)
    (:monterey    . 12)
    (:big-sur     . 11))
  "An alist of macOS name and major version map. ")

(defun osx-version>= (major &optional (minor 0 minor?) (patch 0 patch?))
  "Test if `osx-version' >= MAJOR.MINOR.PATCH
Return `t' if pass.

Parameters:
+ MAJOR: major version number
  or keyword like `:sequoia', `:tahoe', `:golden-gate'
+ MINOR: minor version number 
+ PATCH: patch version number
"
  (macrolet ((cmp (a b &optional (else t))
               `(cond ((> ,a ,b) t)
                      ((< ,a ,b) nil)
                      (t         ,else))))
    (flet ((major! (major)
             (etypecase major
               (keyword
                (or (car (assoc major +osx-version-names+))
                    (error "Unknown macOS version `~S'. " major)))
               (integer
                major))))
      (destructuring-bind (major* minor* patch*) (%osx-version)
        (cmp major* (major! major)
             (if minor?
                 (cmp minor* minor
                      (if patch? (cmp patch* patch) t))
                 t))))))

;;;; osx-version.lisp ends here
