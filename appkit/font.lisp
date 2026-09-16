;;;; font.lisp --- Wraps NSFont

(in-package :coca.appkit)

(define-objc-global-variable ns-font-traits-attribute
    (mem-ref (foreign-symbol-pointer "NSFontTraitsAttribute")
             :pointer)
  "NSFontTraitsAttribute")

(define-objc-global-variable ns-font-weight-trait
    (mem-ref (foreign-symbol-pointer "NSFontWeightTrait")
             :pointer)
  "NSFontWeightTrait")

(defvar *fonts* (tg:make-weak-hash-table)
  "Cache of `font'.

Key: pointer address
Val: `font'")

(defstruct (font (:constructor make-ns-font))
  (ptr       (null-pointer) :type foreign-pointer :read-only t)
  (cg-ptr    (null-pointer) :type foreign-pointer :read-only t)
  (name      ""             :type string       :read-only t)
  (family    ""             :type string       :read-only t)
  (display-name ""          :type string       :read-only t)
  (size      0.0d0          :type double-float :read-only t)
  (weight    0.0d0          :type double-float :read-only t)
  (ascender  0.0d0          :type double-float :read-only t)
  (descender 0.0d0          :type double-float :read-only t))

(defmethod print-object ((font font) stream)
  (print-unreadable-object (font stream :type t)
    (format stream "~S ~F #x~X"
            (font-display-name font)
            (font-size         font)
            (pointer-address (font-ptr font)))))

(defun ns-font-to-font (ptr &optional weight)
  "Make `font' fron NSFont pointer PTR.
Return `font' instance. "
  (declare (type foreign-pointer ptr))
  (alx:ensure-gethash
   (pointer-address ptr)
   *fonts*
   (let ((cg-ptr    (foreign-funcall "CTFontCopyGraphicsFont"
                                     :pointer ptr
                                     :pointer (null-pointer)
                                     :pointer))
         (name      (invoke ptr "fontName"    :ns-string))
         (family    (invoke ptr "familyName"  :ns-string))
         (display   (invoke ptr "displayName" :ns-string))
         (size      (invoke ptr "pointSize"   :double))
         (ascender  (invoke ptr "ascender"    :double))
         (descender (invoke ptr "descender"   :double)))
     (if weight
         (setf weight (coerce weight 'double-float))
         (let* ((desc   (invoke ptr "fontDescriptor" :object))
                (traits (invoke desc "objectForKey:"
                                :pointer (ns-font-traits-attribute)
                                :object))
                (w*     (invoke traits "objectForKey:"
                                :pointer (ns-font-weight-trait)
                                :object)))
           (setf weight (invoke w* "doubleValue" :double))))
     (make-ns-font :ptr          ptr
                   :cg-ptr       cg-ptr
                   :name         name
                   :family       family
                   :display-name display
                   :size         size
                   :weight       weight
                   :ascender     ascender
                   :descender    descender))))

(define-objc-typing :ns-font
  :alias (:pointer ns-font-to-font)
  :arg   ((font `(:pointer (font-ptr ,font)))))


;;;; font-weight

(defvar *font-weight-map*
  (alx:alist-hash-table
   '((:ultra-light . -0.8d0)
     (:thin        . -0.6d0)
     (:light       . -0.4d0)
     (:regular     .  0.0d0)
     (:medium      .  0.23d0)
     (:semibold    .  0.3d0)
     (:bold        .  0.4d0)
     (:heavy       .  0.56d0)
     (:black       .  0.62d0))
   :test 'eq)
  "Named font weights.

Key: keyword of weight
Val: double-float of ns font weight
")

(defun ns-font-weight (weight)
  (declare (type keyword weight))
  (or (gethash weight *font-weight-map*)
      (error "Unknown font weight `~S'. " weight)))

(defun (setf ns-font-weight) (weight name)
  (declare (type keyword weight))
  (let ((weight! (as-ns-font-weight weight)))
    (setf (gethash name *font-weight-map*) weight!)))

(defun as-ns-font-weight (weight)
  "Convert WEIGHT into ns font weight.
Return `double-float' as ns font weight. "
  (declare (type (or (real -1 1) keyword) weight))
  (the double-float
    (etypecase weight
      (double-float
       (assert (<= -1d0 weight 1d0))
       weight)
      ((real -1 1)
       (coerce weight 'double-float))
      (keyword      (ns-font-weight weight)))))

(define-compiler-macro as-ns-font-weight (&whole form weight)
  (typecase weight
    (keyword (ns-font-weight weight))
    (number
     (assert (<= -1 weight 1))
     (coerce weight 'double-float))
    (t       form)))

(define-objc-typing :ns-font-weight
  :alias :double
  :arg   (weight `(:double (as-ns-font-weight ,weight))))

(define-objc-global-variable ns-font-manager
    (invoke "NSFontManager" "sharedFontManager" :object)
  "Return the ObjC NSFontManager instance. ")


;;;; font-size

(defvar *font-size-map*
  (alx:alist-hash-table
   '((:normal . 13d0))
   :test 'eq)
  "")

(defun ns-font-size (name)
  (declare (type keyword name))
  (or (gethash name *font-size-map*)
      (error "Unknown font size for ~S. " name)))

(defun (setf ns-font-size) (size name)
  (declare (type (real 0) size)
           (type keyword  name))
  (setf (gethash name *font-size-map*)
        (coerce size 'double-float)))

(defun as-ns-font-size (size)
  (declare (type (or keyword (real 0)) size))
  (etypecase size
    (double-float size)
    (real         (coerce size 'double-float))
    (keyword      (ns-font-size size))))

(define-objc-typing :ns-font-size
  :alias :double
  :arg   ((size `(:double (as-ns-font-size ,size)))))


;;;; font-family

(defun %font-family-list ()
  (mapcar #'ns-string-to-string
          (invoke (ns-font-manager)
                  "availableFontFamilies"
                  :ns-array)))

(define-objc-global-variable font-family-list (%font-family-list)
  "A list of strings as all avaliable font family names. ")

(define-objc-enum :ct-font-manager-scope
  "Constants that define the scope for font registration.

+ `:none'
  no scoep is defined
+ `:process'
  font is avaliable to the current process for the duration
  of the process unless directly unregistered
+ `:persistent'
  font is avaliable to all processes for the current user
  session and will be available in subsequent sessions
  unless unregistered
+ `:session'
  font is avaliable to the current user session but won't
  be available in subsequent sessions
+ `:user'
  font is available to all processes for the current user
  session and will be available in subsequent sessions
  unless unregistered
"
  (:none            0)
  ((:process :user) 1)
  (:persistent      2)
  (:session         3))

(defun load-font-file (font-file)
  "Load FONT-FILE into current runtime.
Return a list of new font family string.

Parameter:
+ FONT-FILE: pathname to the font name
"
  (declare (type (or string pathname) font-file))
  (with-autorelease-pool
    (let ((ns-url (pathname-to-ns-url font-file)))
      (with-foreign-object (err* :pointer)
        (let ((res (foreign-funcall
                    "CTFontManagerRegisterFontsForURL"
                    :pointer ns-url
                    :pointer (as-ct-font-manager-scope :process)
                    :pointer err*
                    :bool)))
          (when (and (not res)
                     (not (null-pointer-p (mem-ref err* :pointer))))
            (let ((err (mem-ref err* :pointer)))
              (unwind-protect
                   (error "Failed to load font ~A:~%~A"
                          font-file
                          (ns-string-to-string
                           (foreign-funcall
                            "CFErrorCopyDescription"
                            :pointer err
                            :pointer)))
                (foreign-funcall "CFRelease" :pointer err))))
          (setf *font-family-list* (%font-family-list)))))))

(defvar *font-family-name-map*
  (alx:alist-hash-table
   '((:system          . ".AppleSystemUIFont")
     (:san-francisco   . ".SF NS")
     (:helvetica       . "Helvetica")
     (:arial           . "Arial")
     (:times-new-roman . "Times New Roman")
     (:monaco          . "Monaco")
     (:ping-fang       . "PingFang SC")
     (:ping-fang-sc    . "PingFang SC")
     (:ping-fang-tc    . "PingFang TC")
     (:ping-fang-hk    . "PingFang HK"))
   :test 'eq)
  "Named font family names.

Key: keyword of fonts
Val: string of family name
")

(defun ns-font-family-name (name)
  (declare (type keyword name))
  (or (gethash name *font-family-name-map*)
      (error "Get the family name")))

(defun (setf ns-font-family-name) (family name)
  (declare (type string  family)
           (type keyword name))
  (unless (find family (font-family-list) :test #'string=)
    (error "Unknown font family name ~A. " family))
  (setf (gethash name *font-family-name-map*) family))

(defun as-ns-font-family-name (name)
  (declare (type (or string keyword) name))
  (etypecase name
    (string
     (unless (find name (font-family-list) :test #'string=)
       (error "Font ~A is not known font family name. " name))
     (string-to-ns-string name))
    (keyword
     (string-to-ns-string (ns-font-family-name name)))))

(define-objc-typing :ns-font-family-name
  :alias :pointer
  :arg   (((and (type keyword) name)
           `(:pointer (string-to-ns-string
                       ,(ns-font-family-name name))))
          ((and (type string) name)
           (unless (find name (font-family-list) :test #'string=)
             (error "Font ~A is not known font family name. " name))
           `(:pointer (string-to-ns-string ,name)))
          (name
           `(:pointer (as-ns-font-family-name ,name)))))


;;;; make-font

(defun make-font-of-style (style size)
  (declare (type keyword style))
  (macrolet ((font* (&rest bindings)
               `(ecase style
                  ,@(loop :for (key sel*) :in bindings
                          :for sel := (format nil "~AFontOfSize:" sel*)
                          :collect `(,key (invoke "NSFont" ,sel
                                                  :ns-font-size size
                                                  :ns-font))))))
    (font*
     (:label           "label")
     (:message         "message")
     (:menubar         "menuBar")
     (:menu            "menu")
     (:control-content "controlContent")
     (:titlebar        "titleBar")
     (:palette         "palette")
     (:tooltips        "toolTips"))))

;; (defun make-font (&rest font-description
;;                   &key style family size weight slant
;;                   &allow-other-keys)
;;   "Make `font' instance.
;;
;; Parameters:
;; + STYLE:
;; + FAMILY:
;; + SIZE
;;   + font size in points
;;   + `:normal'
;;   + `
;; + WEIGHT
;;   + `:ultra-light'
;;   + `:thin'
;;   + `:light'
;;   + `:normal', `:regular' (default)
;;   + `:medium'
;;   + `:semibold'
;;   + `:bold'
;;   + `:heavy'
;;   + `:black'
;;   + number between [-1, 1]
;; + SLANT

;; "
;;   (macrolet ((givenp (required excluded)
;;                `(and ,@required (not (or ,@excluded)))))
;;     (cond ((givenp (style) (family weight slant))
;;            (case style
;;              (:system
;;               (invoke "NSFont"
;;                       "systemFontOfSize:"
;;                       :double (as-ns-font-size size)
;;                       :ns-font))
;;              (:label
;;               (invoke "NSFont"
;;                       "labelFontOfSize:"
;;                       :double (as-ns-font-size size)
;;                       :ns-font))
;;              (:menubar
;;               (invoke "NSFont"
;;                       "menuBarFontOfSize:"
;;                       :double (as-ns-font-size size)
;;                       :ns-font))
;;              (:menu
;;               (invoke "NSFont"
;;                       "menuFontOfSize:"
;;                       :double (as-ns-font-size size)
;;                       :ns-font))
;;              (:control-content
;;               (invoke "NSFont"
;;                       "controlContentFontOfSize:"
;;                       :double (as-ns-font-size size)
;;                       :object))
;;              (:title-bar
;;               (invoke "NSFont"
;;                       "titleBarFontOfSize:"
;;                       :double (as-ns-font-size size)
;;                       :object))
;;              (:palette
;;               (invoke "NSFont"
;;                       "paletteFontOfSize:"
;;                       :double (as-ns-font-size size)
;;                       :object))))
;;           ((givenp (style weight) (family slant))
;;            ))))

;;;; font.lisp ends here
