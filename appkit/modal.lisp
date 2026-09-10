;;;; modal.lisp --- Opens some dialog window 

(in-package :coca.appkit)

(define-objc-block modal-response ((response :long)))

(defun %run-sheet-modal (window panel)
  "Display PANEL on WINDOW and return response code. "
  (declare (type foreign-pointer panel window))
  (let ((cvar (bt:make-condition-variable :name "MODAL-CVAR"))
        (lock (bt:make-lock "MODAL-LOCK"))
        (resp nil))
    (dispatch-main ()
      (invoke panel
              "beginSheetModalForWindow:completionHandler:"
              :object  window
              :block   (modal-response
                        (response)
                        (bt:with-lock-held (lock)
                          (setf resp response)
                          (bt:condition-notify cvar)))))
    (bt:with-lock-held (lock)
      (loop :while (null resp)
            :do (bt:condition-wait cvar lock)))
    resp))

(defun %run-modal (panel &optional window)
  "Run PANEL and return NSModalResponse. 

Parameters: 
+ PANEL: foreign pointer to panel 
+ WINDOW: `window' or `nil'
  + when given WINDOW and NOT in main thread, 
    `run-modal' run as sheet modal and wait for response 
  + otherwise, `run-modal' run as runModel:
"
  (declare (type foreign-pointer panel)
           (type (or null window) window))
  (if (or (tmt:main-thread-p)
          (null window))
      (dispatch-main () (invoke panel "runModal" :long))
      (%run-sheet-modal (obj-ptr window) panel)))


;;;; Alert

(deftype alert-stype ()
  "Style of popuped NSAlert. 

Definitions: 
+ `:inform'   Tell the user something
+ `:warning'  Warn the user
+ `:error'    Tell the user about an error
+ `:question' Question the user (e.g. Delete this file y-or-n?)

Dev Note: 
+ the `alert-style' design is taken from franz's
  `nofity-user' specification when designing the API
"
  '(member :inform :error :question :warning))

(defun alert (message
              &key
                (title "Coca")
                (style :inform)
                (choices '((t "OK")) choices?)
                icon
                window)
  "Popup NSAlert window with MESSAGE and TITLE. 
Return selected CHOICES as return value. 

Parameters: 
+ MESSAGE: a string for informativeText
+ TITLE:   a string for messageText (default as \"Coca\")
+ CHOICES: a list of element(s), which could be like:

    RETURN-VALUE
    (RETURN-VALUE &optional BUTTON-TEXT)

  + RETURN-VALUE: the value returned as result
  + BUTTON-TEXT:  string used to display as button title

  Note: if STYLE is `:question', the CHOICES will be ignored
+ STYLE:   see `alert-style'
+ WINDOW:  a `window' or `nil'
  + if given `window', the alert will run as window model 
    (sheet model)
  + if given `nil', the alert will run as global model 
+ ICON:    (to be implemented)
"
  (declare (type string message title)
           (type alert-stype style)
           (type (or null window) window)
           (type list choices)
           (ignore icon))
  (let* ((style   (ecase style
                    (:inform  1)
                    (:warning 0)
                    (:error   2)
                    (:question
                     (when choices?
                       (warn "CHOICES is ignored when STYLE is `:question'"))
                     (setf choices '((nil "No") (t "Yes")))
                     1)))
         (choices (loop :for choice* :in choices
                        :collect (destructuring-bind
                                     (ret &optional (txt (princ-to-string ret)))
                                     (alx:ensure-list choice*)
                                   (list ret txt))))
         (alert   (dispatch-main ()
                    (with-autorelease-pool
                      (let ((alert (init (alloc "NSAlert"))))
                        (invoke alert "setMessageText:"     :ns-string title)
                        (invoke alert "setInformativeText:" :ns-string message)
                        (invoke alert "setAlertStyle:"      :ns-uint   style)
                        (loop :for (return-value title) :in choices
                              :do (invoke alert
                                          "addButtonWithTitle:"
                                          :ns-string title))
                        alert)))))
    ;; TODO: better processing NSModalResponse
    (unwind-protect (car (nth (- (%run-modal alert window) 1000) choices))
      (release alert))))

;;;; modal.lisp ends here
