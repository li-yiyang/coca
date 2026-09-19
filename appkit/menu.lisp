;;;; menu.lisp --- Wrapper of NSMenu

(in-package :coca.appkit)

(defclass menu-element (obj) ()
  (:documentation
   "Base class for `menu-item', `menu'. "))

(defclass base-menu-item (menu-element)
  ((menu
    :initform nil
    :type     (or null menu)
    :reader   parent))
  (:documentation
   "Base class for NSMenuItem wrapper. "))

(defclass menu (menu-element
                owned-mixin
                titled-mixin)
  ((supermenu
    :initform nil
    :type     (or null menu)
    :reader   parent)
   (menu-items
    :initform ()
    :reader   children
    :reader   item-list))
  (:documentation
   "Wrapper of NSMenu. ")
  (:default-initargs
   :objc-class     "NSMenu"
   :init-in-main-p t
   :title          "Menu"))

(defclass menu-item (base-menu-item
                     owned-mixin
                     find-obj-mixin
                     target-mixin
                     titled-mixin)
  ((submenu
    :initform nil
    :type     (or null menu)))
  (:documentation
   "Wrapper of NSMenuItem. ")
  (:default-initargs
   :objc-class     "NSMenuItem"
   :init-in-main-p t
   :title          (alx:required-argument :title)))

(defmethod add-item ((container menu-element) (item menu-element))
  (and (add-item container item) t))

(defmethod remove-item ((container menu-element) (item menu-element))
  (remove-item container item))

(defmethod add-child ((item menu-item) (child base-menu-item))
  (with-slots (submenu) item
    ;; ensure submenu is initialized as menu
    (unless submenu
      (let ((menu (make-instance 'menu :title (title item))))
        (with-ptr item item-ptr
          (with-ptr menu menu-ptr
            (dispatch-main ()
              (invoke item-ptr "setSubmenu:" :object menu-ptr))))
        (setf submenu menu)))
    (add-child submenu child)))

(defmethod remove-child ((item menu-item) (child base-menu-item))
  (with-slots (submenu) item
    (when submenu
      (remove-child submenu child))))

(defmethod add-child ((menu menu) (item base-menu-item))
  (with-ptr menu menu-ptr
    (with-ptr item item-ptr
      (dispatch-main ()
        (invoke menu-ptr "addItem:" :object item-ptr))))
  (setf (slot-value item 'menu) menu)
  (setf (slot-value menu 'menu-items)
        (append (slot-value menu 'menu-items) (list item)))
  item)

(defmethod add-child ((menu menu) (item menu-item))
  (call-next-method)
  (alx:when-let ((submenu (slot-value item 'submenu)))
    (setf (slot-value submenu 'supermenu) menu))
  item)

(defmethod remove-child ((menu menu) (item base-menu-item))
  (with-ptr menu menu-ptr
    (with-ptr item item-ptr
      (dispatch-main ()
        (invoke menu-ptr "removeItem:" :object item-ptr))))
  (setf (slot-value item 'menu) nil)
  (setf (slot-value menu 'menu-items)
        (delete item (slot-value menu 'menu-items) :test #'eq)))

(defmethod remove-child ((menu menu) (item menu-item))
  (let ((res (call-next-method)))
    (alx:when-let ((submenu (slot-value item 'submenu)))
      (setf (slot-value submenu 'supermenu) nil))
    res))


;;;; main menu

(flet ((submenu-item (name initf)
         (declare (type string   name)
                  (type function initf))
         (dispatch-main ()
           (let ((item (invoke (alloc "NSMenuItem")
                               "initWithTitle:action:keyEquivalent:"
                               :ns-string name
                               :pointer   (null-pointer)
                               :ns-string ""
                               :object))
                 (menu (invoke (alloc "NSMenu")
                               "initWithTitle:"
                               :ns-string name
                               :object)))
             (invoke item  "setSubmenu:" :object menu)
             (funcall initf menu)
             item))))

  (defclass help-menu-item (base-menu-item) ()
    (:documentation
     "Wrapper of [NSApp helpMenu].

Dev Note:
+ do not manually make-instance of help-menu-item
+ there should only be one help-menu-item
")
    (:default-initargs
     :ptr (flet ((set-help-menu (menu)
                   (invoke (app) "setHelpMenu:" :object menu)))
            (submenu-item "Help" #'set-help-menu))))

  (defclass services-menu-item (base-menu-item) ()
    (:documentation
     "Wrapper of [NSApp servicesMenu].

Dev Note:
+ do not manually make-instance of service-menu-item
+ there should only be one service-menu-item
")
    (:default-initargs
     :ptr (flet ((set-services-menu (menu)
                   (invoke (app) "setServicesMenu:" :object menu)))
            (submenu-item "Services" #'set-services-menu)))))

(define-objc-global-variable help-menu-item
  (make-instance 'help-menu-item)
  "[NSApp helpMenu]")

(define-objc-global-variable services-menu-item
    (make-instance 'services-menu-item)
  "[NSApp servicesMenu]")

;; Dev Note: should menu-separator use `owned-mixin'?
(defclass menu-separator (base-menu-item) ()
  (:default-initargs
   :ptr (invoke "NSMenuItem" "separatorItem" :object)))

(defclass process-menu-item (menu-item) ()
  (:documentation
   "Subclass for process menu item. "))

(define-objc-global-variable process-menu-item
    (let* ((process (invoke (invoke "NSProcessInfo" "processInfo" :object)
                            "processName" :ns-string))
           (item    (make-instance 'process-menu-item :title process)))
      (add-child item (make-instance
                      'menu-item
                      :title  "About"
                      :action "orderFrontStandardAboutPanel:"))
      (add-child item (make-instance 'menu-separator))
      (add-child item (services-menu-item))
      (add-child item (make-instance 'menu-separator))
      (add-child item (make-instance
                      'menu-item
                      :title  "Hide Other"
                      :action "hideOtherApplications:"))
      (add-child item (make-instance
                      'menu-item
                      :title  "Show All"
                      :action "unhideAllApplications:"))
      (add-child item (make-instance
                      'menu-item
                      :title  "Quit"
                      :action "terminate:"))
      item))

(declaim (type menu *main-menu* *default-main-menu*))
(defvar *main-menu*)
(defvar *default-main-menu*)

(app:define-on-coca-app-finish-run set-main-menu
  (set-main-menu (setf *default-main-menu* (make-instance 'menu))))

(defclass %main-menu (menu) ()
  (:documentation
   "Internal usage only.

This should be used to mark a `menu' is setted as main menu.

Dev Note:
+ [maybe] introduce some slot values to save more infomation
"))

(defun main-menu-p (menu)
  "Test if MENU is main menu.
Return `t' if MENU is setted as main menu. "
  (typep menu '%main-menu))

(defun main-menu ()
  "Get/Set the main menu of current app. "
  (unless (boundp '*main-menu*)
    (coca-app-run))
  *main-menu*)

(defun (setf main-menu) (menu)
  (etypecase menu
    (menu (set-main-menu menu))
    (null (set-main-menu *default-main-menu*))))

(defmethod add-child ((menu %main-menu) (item process-menu-item))
  "The `process-menu-item' ITEM should always be added as
the first `menu-item' of MENU. "
  (with-ptr menu menu-ptr
    (with-ptr item item-ptr
      (dispatch-main ()
        (invoke menu-ptr "insertItem:atIndex:"
                :object item-ptr
                :ns-int 0))))
  (setf (slot-value item 'menu) menu)
  (setf (slot-value menu 'menu-items)
        (cons item (slot-value menu 'menu-items)))
  item)

(defmethod add-child ((menu %main-menu) (item menu-item))
  "Normal `menu-item' should be inserted as last two item,
before the `help-menu-item'. "
  (with-ptr menu menu-ptr
    (with-ptr item item-ptr
      (let ((idx (1- (length (slot-value menu 'menu-items)))))
        (dispatch-main ()
          (invoke menu-ptr "insertItem:atIndex:"
                  :object item-ptr
                  :ns-int idx)))))
  (setf (slot-value item 'menu) menu)
  (setf (slot-value menu 'menu-items)
        (cons item (slot-value menu 'menu-items)))
  item)

(defclass main-menu-mixin ()
  ((menu
    :initform (main-menu)
    :type     menu))
  (:documentation
   "Mixin classes for instances having a `menu' as main menu. "))

(defgeneric set-main-menu (menu &rest main-menu-initargs)
  (:documentation
   "Set MENU as main menu.

Parameters:
+ MENU
  If MENU is `main-menu-mixin', the menu of `main-menu-mixin'
  would be setted as App's main menu.
+ MAIN-MENU-INITARGS
  additional initargs for `change-class'
")
  (:method ((menu menu) &rest args)
    (unless (main-menu-p menu)
      (let ((process (process-menu-item))
            (help    (help-menu-item)))
        (with-ptr menu ptr
          (dispatch-main ()
            (add-child menu process)
            (add-child menu help)
            (invoke (app) "setMainMenu:" :object ptr))))
      (apply #'change-class menu '%main-menu args)
      (when (boundp '*main-menu*)
        (change-class *main-menu* 'menu))
      (setf *main-menu* menu))
    menu)
  (:method ((obj main-menu-mixin) &rest args)
    (apply #'set-main-menu (slot-value obj 'menu) args)))

;;;; menu.lisp ends here
