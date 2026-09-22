;;;; url.lisp --- NSURL as `quri:uri'

(in-package :coca.objc)

(defun ns-url-to-pathname-or-url (ns-url)
  "Convert NS-URL to pathname or `quri:uri'.
Return a `pathname' or `quri:uri' instance. "
  (declare (type foreign-pointer ns-url))
  (if (invoke ns-url "isFileURL" :bool)
      (the pathname
        (pathname
         (invoke ns-url "fileSystemRepresentation" :string)))
      (quri:uri (invoke ns-url "absoluteString" :ns-string))))

(defun url-to-ns-url (url)
  "Convert URL into NSURL.
Return foreign-pointer of NSURL for URL.

Parameter:
+ URL:
  + pathname => fileURLWithPath
  + string   => literally NSURL
  + quri:uri => rendered NSURL
"
  (declare (type (or string pathname quri:uri) url))
  (flet ((convert (url)
           (let ((ns-url (invoke "NSURL" "URLWithString:"
                                 :ns-string url
                                 :object)))
             (if (null-pointer-p ns-url)
                 (error "~S is not in a valid NSURL form" url)
                 ns-url))))
    (the foreign-pointer
      (typecase url
        (pathname
         (invoke "NSURL" "fileURLWithPath:"
                 :ns-string (uiop:native-namestring url)
                 :object))
        (string
         (convert url))
        (quri:uri
         (convert (quri:render-uri url)))))))

(define-objc-typing :ns-url
  :result (:pointer ns-url-to-pathname-or-url)
  :arg    ((url `(:pointer (url-to-ns-url ,url)))))

;;;; url.lisp ends here
