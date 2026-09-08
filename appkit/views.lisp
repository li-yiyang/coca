;;;; views.lisp --- Wrapper of NSViews-like widgets

(in-package :coca.appkit)


;;;; misc mixins

(define-objc-enum (:ns-control-state :alias :long)
  "Whether a control is on, off, or in a mixed state. "
  (:on    1)
  (:off   0)
  (:mixed -1))

(defclass state-mixin () ()
  (:documentation
   "Mixin class for obj support state method. "))

(defgeneric state (state-mixin)
  (:method ((obj state-mixin))
    (with-ptr obj ptr
      (invoke ptr "state" :ns-control-state))))

(defmethod (setf state) (state (obj state-mixin))
  (declare (type (member :on :off :mixed) state))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setState:" :ns-control-state state))))

(defclass bordered-mixin () ()
  (:documentation
   "Mixin class for obj support isBordered method. "))

(defgeneric borderedp (bordered-mixin)
  (:documentation
   "Get/Set if BORDERED-MIXIN is bordered or not. ")
  (:method ((obj bordered-mixin))
    (with-ptr obj ptr
      (invoke ptr "isBordered" :bool))))

(defmethod (setf borderedp) (bordered (obj bordered-mixin)
                             &aux (borderedp (and bordered t)))
  (with-ptr obj ptr
    (dispatch-main ()
      (invoke ptr "setBordered:" :bool borderedp))
    borderedp))


;;;; button

(define-objc-enum :ns-button-type
  "ObjC NSButton type.

See `button-type'.
"
  (:momentary-push-in       7)
  (:momentary-light         0)
  (:momentary-change        5)
  (:push-on-push-off        1)
  (:on-off                  6)
  (:toggle                  2)
  (:switch                  3)
  (:radio                   4)
  (:accelerator             8)
  (:multi-level-accelerator 9))

(deftype button-type ()
  "Type of `button'.

+ `:momentary-push-in'
  illuminates when the user clicks it.
+ `:momentary-light'
  displays a highlight when the user clicks it
  and returns to its normal state when the user releases it.
+ `:momentary-change'
  displays its alternate content when clicked
  and returns to its normal content when the user releases it.
+ `:push-on-push-off'
  switches between on and off states with each click.
+ `:on-off'
  switches between a normal and emphasized bezel on each click.
+ `:toggle'
  switches between its normal and alternate content on each click.
+ `:switch'
  checkbox button.
+ `:radio'
  displays a single selected value from group of possible choices.
+ `:accelerator'
  sends repeating actions as pressure changes occur.
+ `:multi-level-accelerator'
  allows for a configurable number of stepped pressure levels
  and provides tactile feedback as the user reaches each step."
  '(member :momentary-push-in :momentary-light :momentary-change
    :push-on-push-off :on-off :toggle :switch :radio
    :accelerator :multi-level-accelerator))

(defclass base-button (base-view
                       titled-mixin
                       alternate-titled-mixin
                       state-mixin
                       bordered-mixin)
  ((button-type
    :initarg  :button-type
    :initform (alx:required-argument :button-type)
    :reader   button-type
    :type     button-type
    :documentation
    "NSButton types. "))
  (:documentation
   "Wrapper of NSButton. ")
  (:default-initargs
   :objc-class "NSButton"))

(defclass button (base-button) ()
  (:documentation
   "Default push button.

Dev Note:
+ different than AppKit default push button,
  the button height can be modified
  (NOTE: maybe changed in the future)
")
  (:default-initargs
   :button-type :momentary-push-in
   :bezel-style :flexible-push))

(defclass checkbox (base-button) ()
  (:documentation
   "Default checkbox button. ")
  (:default-initargs
   :button-type :switch))

(defclass radio-button (base-button) ()
  (:documentation
   "Default radio button. ")
  (:default-initargs
   :button-type :radio))

(defclass help-button (base-button) ()
  (:documentation
   "Default help button.

A help button is round button with question mark within.
Typically used to open a help dialog. ")
  (:default-initargs
   :button-type :momentary-push-in
   :bezel-stype :help))

(define-objc-enum :ns-bezel-style
  "ObjC NSButton bezel style.

+ `:automatic'
  default button style based on the button's contents
  and position within the window
+ `:push'
  standard push style button
+ `:flexible-push'
  push button with a flexible height to accommodate
  longer text labels or an image
+ `:disclosure'
  used with a disclosure triangle
+ `:push-disclosure'
  push button with a disclosure triangle
+ `:toolbar'
  appropriate for a toolbar item
+ `:accessory-bar'
  typically used in the context of an accessory toolbar
  for buttons that narrow the focus of a search or other
  operation
+ `:accessory-bar-action'
  use for extra actions in an accessory toolbar
+ `:help-action'
  a round button with a question mark, providing the
  standard help button look
+ `:badge'
  a button style suitable for displaying additional infomation
+ `:circular'
  a round button that can contain either a single character
  or an icon
+ `:small-square'
  a simple square bezel style that can scale to any size
+ `:glass' (osx-version>= 26)
  with a glass effect
"
  ;; Automatic
  (:automatic            0)
  ;; Push
  (:push                 1)
  (:flexible-push        2)
  ;; Disclosure
  (:disclosure           5)
  (:push-disclosure      14)
  ;; Toolbar
  (:toolbar              11)
  (:accessory-bar        13)
  (:accessory-bar-action 12)
  ;; Help
  (:help-button          9)
  (:badge                15)
  (:circular             7)
  (:small-square         10)
  ;; Glass
  (:glass                16 (osx-version>= 26)))

(defmethod initialize-instance :after ((button base-button)
                                       &key (bezel-style :automatic))
  (with-slots (button-type) button
    (with-ptr button ptr
      (dispatch-main ()
        (invoke ptr "setButtonType:"
                :ns-button-type button-type)
        (invoke ptr "setBezelStyle:"
                :ns-bezel-style bezel-style)))))

(defmethod bezel-style ((button button))
  (with-ptr button ptr
    (invoke ptr "bezelStyle" :ns-bezel-style)))

(defmethod (setf bezel-style) (style (button button))
  (declare (type (member :automatic :push :flexible-push
                         :disclosure :push-disclosure
                         :toolbar :accessory-bar
                         :accessory-bar-action
                         :help-button :badge :circular
                         :small-square :glass)
                 style))
  (with-ptr button ptr
    (dispatch-main ()
      (invoke ptr "setBezelStyle:" :ns-bezel-style style))
    style))

;;;; views.lisp ends here
