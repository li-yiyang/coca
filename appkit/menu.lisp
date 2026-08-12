;;;; menu.lisp --- Wrapper of NSMenu

(in-package :coca.appkit)

(define-objc-global-variable nsapp-help-menu
    (dispatch-main ()
      (invoke (app) "helpMenu" :object))
  "[NSApp helpMenu]")

(define-objc-global-variable nsapp-services-menu
    (dispatch-main ()
      (invoke (app) "servicesMenu" :object))
  "[NSApp servicesMenu]")

(defun %nsapp-process-name ()
  "Return current Cocoa process name NSString pointer. "
  (invoke (invoke "NSProcessInfo" "processInfo" :object)
          "processName"
          :object))

(defun nsapp-process-name ()
  "Return current Cocoa process name string. "
  (ns-string-to-string (%nsapp-process-name)))

(declaim (type (or null string) *nsapp-show-settings-sel*))
(defvar *nsapp-show-settings-sel* nil
  "The selector name (string) or `nil' for no Settings. ")

(defun ns-menu-item (title &optional action (key "") &rest modifiers
                     &aux (title* (etypecase title
                                    (string (string-to-ns-string title))
                                    (foreign-pointer title))))
  (declare (type (or string foreign-pointer) title)
           (type (or null string foreign-pointer) action))
  (let ((item (if action
                  (invoke (alloc "NSMenuItem")
                          "initWithTitle:action:keyEquivalent:"
                          :object    title*
                          :sel       (coerce-to-selector action)
                          :ns-string key
                          :object)
                  (let ((item (init (alloc "NSMenuItem"))))
                    (invoke item "setTitle:" :object title*)
                    item))))
    (when (and action (string/= key ""))
      (invoke item "setKeyEquivalentModifierMask:"
              :unsigned-long (apply #'as-ns-event-modifier modifiers)))
    item))

(defun ns-menu-add-items (item &rest items)
  (let ((menu (invoke (alloc "NSMenu")
                      "initWithTitle:"
                      :object (invoke item "title" :object)
                      :object)))
    (dolist (item items)
      (when (typep item 'foreign-pointer)
        (invoke menu "addItem:" :object item)))
    (invoke item "setSubmenu:" :object menu)
    item))

(defun ns-menu-separator ()
  (invoke "NSMenuItem" "separatorItem" :object))

(defun make-nsapp-application-menu-item ()
  (let* ((proc (%nsapp-process-name))
         (name (ns-string-to-string proc)))
    (ns-menu-add-items
     (ns-menu-item proc)
     (ns-menu-item (format nil "About ~A" name)
                   "orderFrontStandardAboutPanel:")
     (ns-menu-separator)
     (ns-menu-item "Settings"
                   "showSettings:"
                   ","
                   :command)
     (ns-menu-separator)
     (let ((item (ns-menu-item "Services")))
       (invoke item "setSubmenu:" :object (nsapp-services-menu))
       item)
     (ns-menu-separator)
     (ns-menu-item (format nil "Hide ~A" name)
                   "hide:"
                   "h"
                   :command)
     (ns-menu-item "Hide Others"
                   "hideOtherApplications:"
                   "h"
                   :option :command)
     (ns-menu-item "Show All"
                   "unhideAllApplications:")
     (ns-menu-separator)
     (ns-menu-item (format nil "Quit ~A" name)
                   "terminate:"
                   "q"
                   :command))))

(define-objc-global-variable nsapp-application-menu
    (make-nsapp-application-menu-item)
  "A foreign-pointer to NSMenuItem for NSApp Application. ")

(defclass menu (obj
                owned-mixin
                find-obj-mixin
                titled-mixin)
  ()
  (:default-initargs
   :objc-class "NSMenu")
  (:documentation
   "Wrapper of NSMenu.

Initialize Parameter:
+ MENU-ITEMS: should be a list of `menu-item'. "))

(defmethod initialize-instance :after ((menu menu) &key menu-items)
  (declare (type list menu-items))
  (assert (every (alx:rcurry #'typep 'menu-item) menu-items))
  (dispatch-main ()
    (dolist (item menu-items)
      (add-child menu item))))

(defclass main-menu (menu) ()
  (:documentation
   "The `menu' set to be [NSApp mainMenu].

Use `set-main-menu' only to make a `menu' as `main-menu'.
"))

(defclass menu-item (obj
                     owned-mixin
                     find-obj-mixin
                     titled-mixin)
  ((menu
    :type     (or null menu)
    :initarg  :menu
    :initform nil
    :reader   menu
    :reader   parent)
   (submenu
    :type     (or null menu)
    :initform nil
    :reader   submenu))
  (:documentation
   "Wrapper of NSMenuItem. "))

(defun menu-item-ensure-submenu (item)
  (declare (type menu-item item))
  (let ((menu (submenu item)))
    (if menu
        menu
        (setf (slot-value item 'submenu)
              (make-instance 'menu :title (title item))))))

(defmethod initialize-instance :after ((menu-item menu-item) &key menu-items)
  (declare (type list menu-items))
  (assert (every (alx:rcurry #'typep 'menu-item) menu-items))
  (let ((menu (menu-item-ensure-submenu menu-item)))
    (dolist (item menu-items)
      (add-child menu item))))

(defclass menu-seperator (special-menu-item) ()
  (:documentation
   "NSMenuItem seperator. "))

(defun menu-seperator ()
  "Make `menu-seperator'. "
  (make-instance 'menu-seperator
                 :ptr (invoke "NSMenuItem"
                              "separatorItem"
                              :object)))

(defmethod children ((menu menu))
  (with-ptr menu ptr
    (loop :for item* :in (invoke ptr "itemArray" :ns-array)
          :if (pointer-eq item* (nsapp-help-menu))
            :collect :seperator
          :else :if (pointer-eq item* (nsapp-services-menu))
                  :collect :services
          :else :if (find-obj item*)
                  :collect (find-obj item*))))

(defmethod children ((item menu-item))
  (alx:when-let ((menu (submenu item)))
    (children menu)))

(defmethod add-child ((menu menu) (item menu-item))
  (with-ptr menu menu*
    (with-ptr item item*
      (dispatch-main ()
        (invoke menu* "addItem:" :object item*)))))

(defmethod add-child ((item menu-item) (child menu-item))
  (add-child (menu-item-ensure-submenu item) child))

(defmethod remove-child ((menu menu) (item menu-item))
  (with-ptr menu menu*
    (with-ptr item item*
      (dispatch-main ()
        (invoke menu* "removeItem:" :object item*)))))

(defmethod remove-child ((item menu-item) (child menu-item))
  (alx:when-let ((submenu (submenu item)))
    (remove-child submenu child)))

;;;; menu.lisp ends here
