;;;; font.lisp --- Wraps NSFont

(in-package :coca.appkit)

(defvar *fonts* (tg:make-weak-hash-table :weakness :value)
  "Cache of `font'.

Key: pointer address
Val: `font'")

(defstruct (font (:constructor make-ns-font))
  "Wrap of NSFont.

Slot Variables:
+ PTR: foreign-pointer to NSFont
+ CG-PTR: foreign-pointer to CGFont
+ NAME: font name
+ FAMILY: font family name
+ DISPLAY-NAME: font display name
+ SIZE: font point size
+ WEIGHT: font weight [-1, 1]
+ SLANT: font slant [-1, 1]
+ PROPERTIES: an alist of font properties
  use `font-property' to get property of `font'
"
  (ptr          (null-pointer) :type foreign-pointer)
  (cg-ptr       (null-pointer) :type foreign-pointer)
  (name         ""             :type string :read-only t)
  (family       ""             :type string :read-only t)
  (display-name ""             :type string :read-only t)
  (size         0.0d0          :type (double-float 0d0)      :read-only t)
  (weight       0.0d0          :type (double-float -1d0 1d0) :read-only t)
  (slant        0.0d0          :type (double-float -1d0 1d0) :read-only t)
  (properties   ()))

(defun font-property (font property)
  "Get PROPERTY of FONT.
Return font property or nil if not found.

Parameters:
+ FONT: `font'
+ PROPERTY: keyword of font property name
  + `:size'
  + `:weight'
  + `:slant'
  + `:ascender'
  + `:descender'
"
  (declare (type font    font)
           (type keyword property))
  (case property
    (:size     (font-size   font))
    (:weight   (font-weight font))
    (:slant    (font-slant  font))
    (otherwise (getf (font-properties font) property))))

(define-compiler-macro font-property (&whole form font property)
  (typecase property
    (keyword
     (case property
       (:size     `(font-size   ,font))
       (:weight   `(font-weight ,font))
       (:slant    `(font-slant  ,font))
       (otherwise `(getf (font-properties ,font) ,property))))
    (t form)))

(defmethod print-object ((font font) stream)
  (print-unreadable-object (font stream :type t)
    (format stream "~S ~F #x~X"
            (font-display-name font)
            (font-size         font)
            (pointer-address (font-ptr font)))))

(defun ct-font-copy-graphics-font (ptr)
  (declare (type foreign-pointer ptr))
  (foreign-funcall "CTFontCopyGraphicsFont"
                   :pointer ptr
                   :pointer (null-pointer)
                   :pointer))

(define-on-coca-app-finish-run renew-font-ptr-cg-ptr
  (let ((fonts (alx:hash-table-values *fonts*)))
    (clrhash *fonts*)
    (dolist (font fonts)
      (let ((ptr (make-ns-font-ptr (font-family font)
                                   (font-size   font)
                                   (font-weight font)
                                   (font-slant  font))))
        (setf (font-ptr    font) ptr
              (font-cg-ptr font) (ct-font-copy-graphics-font ptr)
              (gethash (pointer-address ptr) *fonts*) font)))))

(defun ns-font-to-font (ptr &key weight slant)
  "Make `font' fron NSFont pointer PTR.
Return `font' instance. "
  (declare (type foreign-pointer ptr)
           (type (or null (double-float -1.0d0 1.0d0)) weight slant))
  (alx:ensure-gethash
   (pointer-address ptr)
   *fonts*
   (let ((cg-ptr    (ct-font-copy-graphics-font ptr))
         (name      (invoke ptr "fontName"    :ns-string))
         (family    (invoke ptr "familyName"  :ns-string))
         (display   (invoke ptr "displayName" :ns-string))
         (size      (invoke ptr "pointSize"   :double))
         (ascender  (invoke ptr "ascender"    :double))
         (descender (invoke ptr "descender"   :double)))
     (unless (and weight slant)
       (let* ((desc   (invoke ptr "fontDescriptor" :object))
              (traits (get-ns-dictionary desc   "NSFontTraitsAttribute")))
         (unless weight
           (setf weight (get-ns-dictionary traits "NSFontWeightTrait" :double)))
         (unless slant
           (setf slant  (get-ns-dictionary traits "NSFontSlantTrait"  :double)))))
     (make-ns-font :ptr          ptr
                   :cg-ptr       cg-ptr
                   :name         name
                   :family       family
                   :display-name display
                   :size         size
                   :weight       weight
                   :slant        slant
                   :properties   (list
                                  :ascender  ascender
                                  :descender descender)))))

(define-objc-typing :ns-font
  :alias (:pointer ns-font-to-font)
  :arg   ((font `(:pointer (font-ptr ,font)))))

(define-objc-global-variable ns-font-manager
    (invoke "NSFontManager" "sharedFontManager" :object)
  "Return the ObjC NSFontManager instance. ")

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
Return updated `font-family-list'.

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
                    :uint32  (as-ct-font-manager-scope :process)
                    :pointer err*
                    :bool)))
          (when (and (not res)
                     (not (null-pointer-p (mem-ref err* :pointer))))
            (let* ((err  (mem-ref err* :pointer))
                   (desc (foreign-funcall
                          "CFErrorCopyDescription"
                          :pointer err
                          :pointer)))
              (unwind-protect
                   (error "Failed to load font ~A:~%~A"
                          font-file
                          (ns-string-to-string desc))
                (release desc)
                (foreign-funcall "CFRelease" :pointer err))))
          (setf *font-family-list* (%font-family-list)))))))

(defvar *font-family-name-map*
  (alx:alist-hash-table
   '((:system          . ".AppleSystemUIFont")
     (:fix             . "Courier New")
     (:serif           . "Times New Roman")
     (:sans-serif      . "Verdana")
     (:helvetica       . "Helvetica")
     (:arial           . "Arial")
     (:times-new-roman . "Times New Roman")
     (:monaco          . "Monaco")
     (:ping-fang       . "PingFang SC")
     (:ping-fang-sc    . "PingFang SC")
     (:ping-fang-tc    . "PingFang TC")
     (:ping-fang-hk    . "PingFang HK"))
   :test 'eq))

(defun ns-font-family-name (face &optional (errorp t))
  (declare (type keyword face))
  (or (gethash face *font-family-name-map*)
      (when errorp
        (error "Unknown font family name for `~S' face. " face))))

(defun (setf ns-font-family-name) (family name &optional errorp)
  (declare (type keyword name)
           (ignore errorp))
  (setf (gethash name *font-family-name-map*)
        (as-ns-font-family-name family)))

(defun as-ns-font-family-name (face)
  (declare (type (or string keyword) face))
  (etypecase face
    (string
     (unless (find face (font-family-list) :test #'string=)
       (error "Font ~A is not known font family name. " face))
     face)
    (keyword (ns-font-family-name face))))

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

(macrolet ((define-font-attr-map ((name low high) binding)
             (let ((table   (objc::symbol-concat "*FONT-" name "-MAP*"))
                   (accessf (objc::symbol-concat "NS-FONT-" name))
                   (as-f    (objc::symbol-concat "AS-NS-FONT-" name)))
               `(progn
                  (defvar ,table
                    (alx:alist-hash-table ',binding :test 'eq))
                  (defun ,accessf (face &optional (errorp t))
                    (declare (type keyword face))
                    (or (gethash face ,table)
                        (when errorp
                          (error ,(format nil
                                          "Unknown font ~(~A~) for `~~S' face. "
                                          name)
                                 face))))
                  (defun ,as-f (face)
                    (declare (type (or keyword
                                       (real ,low ,high)
                                       (cons keyword list))
                                   face))
                    (etypecase face
                      ((double-float ,low ,high) face)
                      ((real ,low ,high) (coerce face 'double-float))
                      (keyword (,accessf face))))
                  (defun (setf ,accessf) (,name face &optional errorp)
                    (declare (type keyword face)
                             (ignore errorp))
                    (setf (gethash face ,table) (,as-f ,name)))
                  (define-objc-typing (intern (symbol-name accessf) :keyword)
                    :alias :double
                    :arg   (((and (type (double-float ,low ,high)) val)
                             (list :double val))
                            ((and (type (real ,low ,high)) val)
                             (list :double (coerce val 'double-float)))
                            (face
                             `(:double (,',as-f ,face)))))))))

  (define-font-attr-map (weight -1d0 1d0)
      ((:ultra-light . -0.8d0)
       (:thin        . -0.6d0)
       (:light       . -0.4d0)
       (:regular     .  0.0d0)
       (:medium      .  0.23d0)
       (:semibold    .  0.3d0)
       (:bold        .  0.4d0)
       (:heavy       .  0.56d0)
       (:black       .  0.62d0)))

  (define-font-attr-map (size 0d0 *)
      ((:tiny       . 9d0)
       (:very-small . 10d0)
       (:small      . 11d0)
       (:normal     . 13d0)
       (:regular    . 13d0)
       (:large      . 18d0)
       (:very-large . 24d0)
       (:huge       . 32d0)))

  (define-font-attr-map (slant -1d0 1d0)
      ((:roman   . 0d0)
       (:regular . 0d0)
       (:normal  . 0d0)
       (:italic  . -0.2d0))))

(defun make-ns-font-ptr (family size weight slant)
  "Make and return foreign-pointer to NSFont of FAMILY, SIZE, WEIGHT, TRAIT. "
  (declare (type string family)
           (type (double-float 0d0) size)
           (type (double-float -1d0 1d0) weight slant))
  (let ((desc (invoke "NSFontDescriptor"
                      "fontDescriptorWithFontAttributes:"
                      :ns-dictionary (("NSFontFamilyAttribute" family)
                                      ("NSFontTraitsAttribute"
                                       (("NSFontWeightTrait" weight)
                                        ("NSFontSlantTrait"  slant))))
                      :object)))
    (invoke "NSFont"
            "fontWithDescriptor:size:"
            :object desc
            :double size
            :object)))

(defun make-font (&key
                    (family :system)
                    (size   :regular)
                    (weight :regular)
                    (slant  :roman))
  "Create `font' using FAMILY, SIZE, WEIGHT, SLANT.
Return a `font' object.

Parameters:
+ FAMILY: keyword or string for font family name
  + `:system'
  + `:san-francisco'
  + `:helvetica'
  + `:arial'
  + `:times-new-roman'
  + `:monaco'
  + `:ping-fang'
  + `:ping-fang-sc'
  + `:ping-fang-tc'
  + `:ping-fang-hk'
+ SIZE: keyword or point size of font
  + `:tiny'
  + `:very-small'
  + `:small'
  + `:normal'
  + `:regular'
  + `:large'
  + `:very-large'
  + `:huge'
+ WEIGHT: keyword or [-1, 1] for font weight
  + `:ultra-light'
  + `:thin'
  + `:light'
  + `:regular'
  + `:medium'
  + `:semibold'
  + `:bold'
  + `:heavy'
  + `:black'
+ SLANT: keyword or [-1, 1] for font slant
  + `:roman'
  + `:italic'
"
  (declare (type (or keyword string) family)
           (type (or keyword (real -1 1)) weight slant)
           (type (or keyword (real 0)) size))
  (let ((weight (as-ns-font-weight weight))
        (slant  (as-ns-font-slant  slant)))
    (ns-font-to-font
     (make-ns-font-ptr (as-ns-font-family-name family)
                       (as-ns-font-size        size)
                       weight
                       slant)
     :weight weight
     :slant  slant)))

;;;; font.lisp ends here
