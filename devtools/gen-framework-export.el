(defun coca-gen-framework-export ()
  "Parse buffer and search for:

    (doc-objc-class \"*\" ; <SYMBOL>

and output #:<SYMBOL>"
  (interactive)
  (let ((capture ()))
    (save-excursion
      (while (search-forward-regexp
              (rx (or (seq bol (group ";;;; ") (group (+ (any "a-zA-Z-, "))) eol)
                      (seq bol (group "(doc-objc-class \"") (+ (any "a-zA-Z")) "\"" (+ blank)
                           "; " (group (+ (any "a-z-"))))
                      (seq bol (group "(define-objc-enum") (+ blank) (group (+ (any "a-z-"))))))
              nil
              t)
        (cond ((match-string 1) (push (format "\n  ;; %s" (match-string 2)) capture))
              ((match-string 3) (push (format "  #:%s" (match-string 4)) capture))
              ((match-string 5) (push (format "  #:%s" (match-string 6)) capture)))))
    (insert (string-join (reverse capture) "\n"))))
