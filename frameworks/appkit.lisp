;;;; appkit.lisp --- ObjC bindings for AppKit Framework

(uiop:define-package #:coca.appkit
  (:use :cl :coca.objc :coca.foundation :coca.uniform-type-identifiers)
  (:documentation
   "Construct and manage a graphical, event-driven user interface for your macOS app.

AppKit contains the objects you need to build the user interface for a
macOS app. In addition to drawing windows, buttons, panels, and text
fields, it handles all the event management and interaction between
your app, people, and macOS.

Aside from drawing and managing interactions, AppKit handles printing,
animating, as well as creating documents with large amounts of data
efficiently. The framework also contains built-in support for
localization and accessibility to ensure that your app reaches as many
people as possible.

AppKit also works with SwiftUI, so you can implement parts of your
AppKit app in SwiftUI or mix interface elements between the two
frameworks. For example, you can place AppKit views and view
controllers inside SwiftUI views, and vice versa.

Note:
For information about bringing your iPad app to Mac, see Mac
Catalyst. To build an iOS app, you can use SwiftUI to create an app
that works across all of Apple’s platforms, or use UIKit to create an
app for iOS only.

see https://developer.apple.com/documentation/appkit?language=objc")
  (:export
   ;; App and Environment
   #:ns-application
   #:current-event
   #:running-p
   #:activep
   #:dock-tile
   #:windows
   #:application-icon-image
   #:main-menu
   #:ns-application-activation-policy
   #:as-ns-application-activation-policy
   #:decode-ns-application-activation-policy
   #:ns-application-activation-policy-p
   #:activation-policy
   #:ns-app
   #:+ns-event-tracking-run-loop-mode+
   #:+ns-modal-panel-run-loop-mode+
   #:next-event
   #:finish-launching
   #:run
   #:stop
   #:terminate
   #:send-event
   #:ns-modal-response
   #:ns-modal-response-p
   #:as-ns-modal-response
   #:decode-ns-modal-response
   #:ns-running-application
   #:ns-workspace
   #:ns-workspace-open-configuration
   #:ns-user-activity
   #:ns-sharing-service
   #:ns-toolbar-item
   #:ns-sharing-service-picker-toolbar-item
   #:ns-help-manager
   #:update-windows

   ;; Documents, Data, and Pasteboard
   #:ns-document
   #:ns-document-controller
   #:ns-persistent-document
   #:ns-pasteboard
   #:ns-file-promise-provider
   #:ns-file-promise-receiver

   ;; Cocoa Bindings
   #:ns-object-controller
   #:ns-controller
   #:ns-tree-controller
   #:ns-tree-node
   #:ns-array-controller
   #:ns-dictionary-controller
   #:ns-dictionary-controller-key-value-pair

   ;; Resource Management
   #:ns-storyboard
   #:ns-storyboard-segue
   #:ns-data-asset
   #:ns-nib
   #:ns-nib-connector
   #:ns-nib-control-connector
   #:ns-nib-outlet-connector

   ;; App Extensions

   ;; Views and Controls
   #:ns-autoresizing-mask-options
   #:ns-autoresizing-mask-options-p
   #:as-ns-autoresizing-mask-options
   #:decode-ns-autoresizing-mask-options
   #:ns-view
   #:superview
   #:subviews
   #:window
   #:needs-display-p
   #:frame
   #:autoresizing-mask
   #:ns-text-alignment
   #:as-ns-text-alignment
   #:ns-text-alignment-p
   #:decode-ns-text-alignment
   #:ns-line-break-mode
   #:as-ns-line-break-mode
   #:ns-line-break-mode-p
   #:decode-ns-line-break-mode
   #:ns-window-ordering-mode
   #:add-subview
   #:remove-from-superview
   #:replace-subview
   #:ns-text-alignment
   #:ns-text-alignment-p
   #:as-ns-text-alignment
   #:decode-ns-text-alignment
   #:ns-line-break-mode
   #:ns-line-break-mode-p
   #:as-ns-line-break-mode
   #:decode-ns-line-break-mode
   #:ns-control
   #:enablep
   #:string-value
   #:alignment
   #:font
   #:line-break-mode
   #:action
   #:target
   #:continuousp
   #:ns-cell
   #:ns-action-cell
   #:ns-split-view
   #:ns-stack-view
   #:ns-tab-view
   #:ns-text-view
   #:ns-button-type
   #:ns-button-type-p
   #:as-ns-button-type
   #:decode-ns-button-type
   #:*ns-button-type*
   #:ns-cell-image-position
   #:ns-cell-image-position-p
   #:as-ns-cell-image-position
   #:decode-ns-cell-image-position
   #:ns-bezel-style
   #:ns-bezel-style-p
   #:as-ns-bezel-style
   #:decode-ns-bezel-style
   #:ns-control-state-value
   #:ns-control-state-value-p
   #:as-ns-control-state-value
   #:decode-ns-control-state-value
   #:ns-button
   #:button-type
   #:title
   #:alternate-title
   #:image
   #:alternate-image
   #:image-position
   #:borderedp
   #:transparentp
   #:bezel-style
   #:bezel-color
   #:shows-border-only-while-mouse-inside-p
   #:allows-mixed-state-p
   #:state
   #:ns-color-well
   #:ns-combo-button
   #:ns-combo-button-style
   #:ns-combo-button-style-p
   #:as-ns-combo-button-style
   #:decode-ns-combo-button-style
   #:ns-image-scaling
   #:ns-image-scaling-p
   #:as-ns-image-scaling
   #:decode-ns-image-scaling
   #:style
   #:title
   #:image
   #:image-scaling
   #:menu
   #:ns-combo-box
   #:has-vertical-scroller-p
   #:intercell-spacing
   #:borderedp
   #:item-height
   #:number-of-visible-items
   #:index-of-selected-item
   #:add-item
   #:select-item-at-index
   #:ns-image-view
   #:ns-level-indicator
   #:ns-pop-up-button
   #:ns-progress-indicator
   #:ns-rule-editor
   #:ns-predicate-editor
   #:ns-segmented-control
   #:ns-stepper
   #:ns-switch
   #:ns-matrix
   #:ns-glass-effect-view
   #:ns-glass-effect-container-view
   #:ns-background-extension-view
   #:ns-visual-effect-view
   #:ns-box-type
   #:ns-box-type-p
   #:as-ns-box-type
   #:decode-ns-box-type
   #:ns-title-position
   #:ns-title-position-p
   #:as-ns-title-position
   #:decode-ns-title-position
   #:ns-box
   #:border-rect
   #:box-type
   #:transparentp
   #:title
   #:title-font
   #:title-position
   #:border-color
   #:border-width
   #:fill-color
   #:corner-radius

   ;; View Management
   #:ns-window-controller
   #:ns-view-controller
   #:ns-titlebar-accessory-view-controller
   #:ns-split-view-controller
   #:ns-split-view
   #:ns-split-view-item
   #:ns-stack-view
   #:ns-tab-view-controller
   #:ns-tab-view
   #:ns-tab-view-item
   #:ns-page-controller
   #:ns-media-library-browser-controller

   ;; View Layout
   #:ns-stack-view
   #:ns-layout-constraint
   #:ns-layout-guide
   #:ns-layout-dimension
   #:ns-layout-anchor
   #:ns-layout-x-axis-anchor
   #:ns-layout-y-axis-anchor

   ;; Appearance Customization
   #:ns-appearance

   ;; Animation
   #:ns-view-animation
   #:ns-animation-context
   #:ns-animation

   ;; Windows, Panels, and Screens
   #:ns-window-style-mask
   #:ns-window-style-mask-p
   #:decode-ns-window-style-mask
   #:ns-backing-store-type
   #:ns-backing-store-type-p
   #:decode-backing-store-type
   #:ns-window
   #:content-view-controller
   #:content-view
   #:style-mask
   #:alpha-value
   #:background-color
   #:color-space
   #:can-hide-p
   #:on-active-space
   #:hides-on-deactivate
   #:collection-behavior
   #:opaquep
   #:has-shadow-p
   #:visiblep
   #:title
   #:windows
   #:ns-backing-store-type
   #:*ns-window-style*
   #:*ns-backing-store*
   #:make-main-window
   #:window-did-resize
   #:window-did-miniaturize
   #:window-did-deminiaturize
   #:window-did-move
   #:window-did-change-screen
   #:window-should-close
   #:window-will-close
   #:ns-panel
   #:ns-window-tab
   #:ns-window-tab-group
   #:ns-screen
   #:ns-main-screen
   #:ns-screens
   #:ns-popover
   #:ns-alert
   #:ns-alert-style
   #:as-ns-alert-style
   #:decode-ns-alert-style
   #:ns-alert-style-p
   #:alert-style
   #:shows-help-p
   #:help-anchor
   #:delegate
   #:message
   #:informative-text
   #:icon
   #:ns-open-panel
   #:ns-save-panel
   #:can-choose-files-p
   #:can-choose-directories-p
   #:resolves-aliases-p
   #:allows-multiple-selection-p
   #:accessory-view-disclosed-p
   #:can-download-ubiquitous-contents-p
   #:can-resolve-ubiquitous-conflicts-p
   #:title
   #:prompt
   #:message
   #:name-field-label
   #:name-field-string-value
   #:directory-pathname
   #:ns-sharing-service-picker
   #:ns-preview-representing-activity-item
   #:ns-pdf-panel
   #:ns-color-panel-mode
   #:as-ns-color-panel-mode
   #:decode-ns-color-panel-mode
   #:ns-color-panel-mode-p
   #:change-color
   #:ns-color-panel
   #:mode
   #:continuousp
   #:color
   #:ns-color-picker
   #:ns-font-panel
   #:change-font
   #:selected-font

   ;; Sound, Speech, and Haptics
   #:ns-sound
   #:ns-speech-recognizer
   #:ns-speech-synthesizer
   #:ns-haptic-feedback-manager
   #:ns-alignment-feedback-filter

   ;; Supporting Continuity Camera in Your Mac App

   ;; Mouse, Keyboard, and Trackpad
   #:ns-event-type
   #:ns-event-type-p
   #:decode-ns-event-type
   #:ns-event-subtype
   #:ns-event-subtype-p
   #:decode-ns-event-subtype
   #:ns-event-modifier-flags
   #:ns-event-modifier-flags-p
   #:decode-ns-event-modifier-flags
   #:ns-event-mask
   #:as-ns-event-mask
   #:ns-event-mask-p
   #:decode-ns-event-mask
   #:ns-event
   #:location-in-window
   #:event-type
   #:event-subtype
   #:modifier-flags

   ;; Menus, Cursors, and the Dock
   #:ns-menu-selection-mode
   #:ns-menu-selection-mode-p
   #:as-ns-menu-selection-mode
   #:decode-ns-menu-selection-mode
   #:ns-menu-properties
   #:ns-menu-properties-p
   #:as-ns-menu-properties
   #:decode-ns-menu-properties
   #:ns-menu-presentation-style
   #:ns-menu-presentation-style-p
   #:as-ns-menu-presentation-style
   #:decode-ns-menu-presentation-style
   #:ns-menu
   #:items
   #:supermenu
   #:autoenables-items-p
   #:selected-items
   #:minimum-width
   #:size
   #:properties-to-update
   #:shows-state-column-p
   #:highlighted-item
   #:user-interface-layout-direction
   #:selection-mode
   #:presentation-style
   #:as-ns-menu
   #:ns-menu-item
   #:enabledp
   #:hiddenp
   #:hidden-or-has-hidden-ancestor-p
   #:attributed-title
   #:tag
   #:state
   #:image
   #:on-state-image
   #:off-state-image
   #:mixed-state-image
   #:badge
   #:section-header-p
   #:submenu
   #:has-submenu-p
   #:parent-item
   #:separator-item-p
   #:menu
   #:key-equivalent
   #:key-equivalent-modifier-mask
   #:tooltip
   #:add-item
   #:as-ns-menu-item
   #:ns-menu-badge-type
   #:ns-menu-badge-type-p
   #:as-ns-menu-badge-type
   #:decode-ns-menu-badge-type
   #:ns-menu-item-badge
   #:item-count
   #:string-value
   #:badge-type
   #:ns-status-bar
   #:ns-status-item
   #:ns-status-bar-button
   #:ns-cursor
   #:ns-tracking-area
   #:ns-dock-tile

   ;; Gestures
   #:ns-click-gesture-recognizer
   #:ns-press-gesture-recognizer
   #:ns-pan-gesture-recognizer
   #:ns-rotation-gesture-recognizer
   #:ns-magnification-gesture-recognizer
   #:ns-gesture-recognizer

   ;; Touch Bar
   #:ns-touch-bar
   #:ns-touch-bar-item
   #:ns-candidate-list-touch-bar-item
   #:ns-color-picker-touch-bar-item
   #:ns-custom-touch-bar-item
   #:ns-group-touch-bar-item
   #:ns-popover-touch-bar-item
   #:ns-sharing-service-picker-touch-bar-item
   #:ns-slider-touch-bar-item
   #:ns-stepper-touch-bar-item
   #:ns-user-interface-compression-options
   #:ns-button-touch-bar-item
   #:ns-picker-touch-bar-item
   #:ns-scrubber
   #:ns-scrubber-item-view
   #:ns-scrubber-arranged-view
   #:ns-scrubber-image-item-view
   #:ns-scrubber-selection-style
   #:ns-scrubber-selection-view
   #:ns-scrubber-text-item-view
   #:ns-scrubber-flow-layout
   #:ns-scrubber-proportional-layout
   #:ns-scrubber-layout-attributes
   #:ns-scrubber-layout

   ;; Drag and Drop
   #:ns-dragging-item
   #:ns-dragging-session
   #:ns-dragging-image-component

   ;; Accessibility for AppKit
   #:ns-accessibility-element

   ;; Images and PDF
   #:ns-image
   #:size
   #:as-ns-image
   #:ns-image-rep
   #:ns-bitmap-image-rep
   #:ns-ci-image-rep
   #:ns-pict-image-rep
   #:ns-pdf-image-rep
   #:ns-pdf-info
   #:ns-eps-image-rep
   #:ns-custom-image-rep

   ;; Drawing
   #:ns-graphics-context
   #:ns-bezier-path
   #:ns-string-drawing-context
   #:ns-gradient
   #:ns-shadow

   ;; Color
   #:ns-color
   #:alpha-component
   #:white-component
   #:red-component
   #:green-component
   #:blue-component
   #:cyan-component
   #:magenta-component
   #:yellow-component
   #:black-component
   #:hue-component
   #:saturation-component
   #:brightness-component
   #:catalog-name-component
   #:localized-catalog-name-component
   #:color-name-component
   #:localized-color-name-component
   #:ns-color-srgba
   #:ns-color-rgba
   #:ns-color-display-p3-rgba
   #:ns-color-calibrated-rgba
   #:ns-color-device-rgba
   #:ns-color-calibrated-hsb
   #:ns-color-device-hsb
   #:ns-color-hsb
   #:ns-color-cmyk
   #:ns-color-white
   #:ns-color-calibrated-white
   #:ns-color-device-white
   #:ns-color-generic-gamma-22-white
   #:ns-color-hdr-rgba-linear
   #:ns-color-hdr-rgba-exposure
   #:ns-color-pattern-image
   #:as-ns-color
   #:ns-color-list
   #:ns-color-space
   #:ns-color-picker
   #:ns-color-well
   #:ns-color-picker-touch-bar-item
   #:ns-color-sampler

   ;; Printing
   #:ns-print-panel
   #:ns-page-layout
   #:ns-printer
   #:ns-print-info
   #:ns-print-operation

   ;; Text Display
   #:ns-line-break-strategy
   #:decode-ns-line-break-strategy
   #:ns-line-break-strategy-p
   #:ns-text-field-bezel-style
   #:decode-ns-text-field-bezel-style
   #:ns-text-field-bezel-style-p
   #:ns-text-field
   #:selectable
   #:editable
   #:allows-editing-text-attributes-p
   #:imports-graphics-p
   #:placeholder-string
   #:placeholder-attributed-string
   #:line-break-strategy
   #:allows-default-tightening-for-truncation-p
   #:maximum-number-of-lines
   #:preferred-max-layout-width
   #:text-color
   #:background-color
   #:draws-background-p
   #:bezeledp
   #:bezel-style
   #:ns-text-view
   #:ns-text
   #:ns-text-input-context
   #:ns-text-insertion-indicator
   #:ns-spell-checker

   ;; TextKit
   #:ns-text-content-storage
   #:ns-text-content-manager
   #:ns-attributed-string
   #:ns-mutable-attributed-string
   #:ns-paragraph-style
   #:ns-mutable-paragraph-style
   #:ns-text-tab
   #:ns-text-list
   #:ns-text-table
   #:ns-text-table-block
   #:ns-text-block
   #:ns-text-paragraph
   #:ns-text-list-element
   #:ns-text-element
   #:ns-text-range
   #:ns-text-selection
   #:ns-text-selection-navigation
   #:ns-text-layout-manager
   #:ns-text-container
   #:ns-text-layout-fragment
   #:ns-text-line-fragment
   #:ns-text-viewport-layout-controller
   #:ns-text-attachment
   #:ns-text-attachment-view-provider
   #:ns-adaptive-image-glyph
   #:ns-cell
   #:ns-text-attachment-cell
   #:ns-glyph-generator
   #:ns-glyph-info
   #:ns-text-storage
   #:ns-layout-manager
   #:ns-ats-typesetter
   #:ns-typesetter

   ;; Fonts
   #:ns-font
   #:font-descriptor
   #:family-name
   #:font-name
   #:as-ns-font-size
   #:+ns-font-weight-ultra-light+
   #:+ns-font-weight-thin+
   #:+ns-font-weight-light+
   #:+ns-font-weight-regular+
   #:+ns-font-weight-medium+
   #:+ns-font-weight-semibold+
   #:+ns-font-weight-bold+
   #:+ns-font-weight-heavy+
   #:+ns-font-weight-black+
   #:as-ns-font-weight
   #:as-ns-font
   #:ns-font-descriptor
   #:ns-font-manager
   #:convert-font
   #:selected-font
   #:available-fonts
   #:available-font-families
   #:ns-font-trait-mask
   #:ns-font-trait-mask-p
   #:as-ns-font-trait-mask
   #:decode-ns-font-trait-mask
   #:ns-font-collection
   #:ns-mutable-font-collection

   ;; Writing Tools
   #:ns-writing-tools-coordinator
   #:ns-writing-tools-coordinator-context
   #:ns-writing-tools-coordinator-animation-parameters
   #:ns-text-preview
   ))

(in-package :coca.appkit)

(cffi:define-foreign-library appkit
  (:darwin (:framework "AppKit")))
(cffi:use-foreign-library appkit)


;;;; Documents, Data, and Pasteboard
;; Organize your app’s data and preferences, and share that data on the pasteboard or in iCloud.
;; see https://developer.apple.com/documentation/appkit/documents-data-and-pasteboard?language=objc

;;; Documents

(define-objc-class "NSDocument" ()
  ()
  (:documentation
   "An abstract class that defines the interface for macOS documents.
A document is an object that can internally represent data
displayed in a window and that can read data from and write data to a
file or file package. Documents create and manage one or more window
controllers and are in turn managed by a document
controller. Documents respond to first-responder action messages to
save, revert, and print their data.

Conceptually, a document is a container for a body of information
identified by a name under which it is stored in a disk file. In this
sense, however, the document is not the same as the file but is an
object in memory that owns and manages the document data. In the
context of `Coca.AppKit', a document is an instance of a custom
`ns-document' subclass that knows how to represent internally, in one
or more formats, persistent data that is displayed in windows.

A document can read that data from a file and write it to a file. It
is also the first-responder target for many menu commands related to
documents, such as Save, Revert, and Print. A document manages its
window’s edited status and is set up to perform undo and redo
operations. When a window is closing, the document is asked before the
window delegate to approve the closing.

`ns-document' is one of the triad of AppKit classes that establish an
architectural basis for document-based apps (the others being
`ns-document-controller' and `ns-window-controller').

For more information about using `ns-document' in a document-based app,
see Developing a Document-Based App.
see https://developer.apple.com/documentation/appkit/developing-a-document-based-app?language=objc
see https://developer.apple.com/documentation/appkit/nsdocument?language=objc"))

(define-objc-class "NSDocumentController" ()
  ()
  (:documentation
   "An object that manages an app’s documents.
As the first-responder target of New and Open menu commands,
`ns-document-controller' creates and opens documents and tracks them
throughout a session of the app. When opening documents, a document
controller runs and manages the modal Open panel. `ns-document-controller'
objects also maintain and manage the mappings of document types,
extensions, and `ns-document' subclasses as specified in the
CFBundleDocumentTypes property loaded from the information property
list (Info.plist).

You can use various `ns-document-controller' methods to get a list of the
current documents, get the current document (which is the document
whose window is currently key), get documents based on a given
filename or window, and find out about a document’s extension, type,
display name, and document class.

In some situations, it’s worthwhile to subclass `ns-document-controller'
in non-`ns-document'-based apps to get some of its features. For example,
the NSDocument management of the Open Recent menu is useful
in apps that don’t use subclasses of `ns-document'.
see https://developer.apple.com/documentation/appkit/nsdocumentcontroller?language=objc"))

(define-objc-class "NSPersistentDocument" ()
  ()
  (:documentation
   "A document object that can integrate with Core Data.
see https://developer.apple.com/documentation/appkit/nspersistentdocument?language=objc"))

;;; User Preferences

;;; Pasteboard

(define-objc-class "NSPasteboard" ()
  ()
  (:documentation
   "An object that transfers data to and from the pasteboard server.
see https://developer.apple.com/documentation/appkit/nspasteboard?language=objc"))

;;; File Promises

(define-objc-class "NSFilePromiseProvider" ()
  ()
  (:documentation
   "An object that provides a promise for the pasteboard.
see https://developer.apple.com/documentation/appkit/nsfilepromiseprovider?language=objc"))

(define-objc-class "NSFilePromiseReceiver" ()
  ()
  (:documentation
   "An object that receives a file promise from the pasteboard.
see https://developer.apple.com/documentation/appkit/nsfilepromisereceiver?language=objc"))

;;; Object Editing


;;;; Cocoa Bindings
;; Automatically synchronize your data model with your app’s interface using Cocoa Bindings.
;; see https://developer.apple.com/documentation/appkit/cocoa-bindings?language=objc

;;; Core Controllers

(define-objc-class "NSObjectController" ()
  ()
  (:documentation
   "A controller that can manage an object’s properties referenced by key-value paths.
see https://developer.apple.com/documentation/appkit/nsobjectcontroller?language=objc"))

(define-objc-class "NSController" ()
  ()
  (:documentation
   "An abstract class that implements the NSEditor and NSEditorRegistration informal protocols required for controller classes.
see https://developer.apple.com/documentation/appkit/nscontroller?language=objc"))

;;; Tree-Based Data

(define-objc-class "NSTreeController" ()
  ()
  (:documentation
   "A bindings-compatible controller that manages a tree of objects.
see https://developer.apple.com/documentation/appkit/nstreecontroller?language=objc"))

(define-objc-class "NSTreeNode" ()
  ()
  (:documentation
   "A node in a tree of nodes.
see https://developer.apple.com/documentation/appkit/nstreenode?language=objc"))

;;; Array-Based Data

(define-objc-class "NSArrayController" ()
  ()
  (:documentation
   "A bindings-compatible controller that manages a collection of objects.
see https://developer.apple.com/documentation/appkit/nsarraycontroller?language=objc"))

;;; Key-Value Data

(define-objc-class "NSDictionaryController" ()
  ()
  (:documentation
   "A bindings-compatible controller that manages the display and editing of
a dictionary of key-value pairs.
see https://developer.apple.com/documentation/appkit/nsdictionarycontroller?language=objc"))

(define-objc-class "NSDictionaryControllerKeyValuePair" ()
  ()
  (:documentation
   "A set of methods implemented by arranged objects to give access to information about those objects.
see https://developer.apple.com/documentation/appkit/nsdictionarycontrollerkeyvaluepair?language=objc"))

;;; Data Placeholders


;;;; Resource Management
;; Manage the storyboards and nib files containing your app’s user interface,
;; and learn how to load data that is stored in resource files.
;; see https://developer.apple.com/documentation/appkit/resource-management?language=objc

;;; Storyboard

(define-objc-class "NSStoryboard" ()
  ()
  (:documentation
   "An encapsulation of the design-time view controller and window controller
graph represented in an Interface Builder storyboard resource file.
see https://developer.apple.com/documentation/appkit/nsstoryboard?language=objc"))

(define-objc-class "NSStoryboardSegue" ()
  ()
  (:documentation
   "A transition or containment relationship between two scenes in a storyboard.
see https://developer.apple.com/documentation/appkit/nsstoryboardsegue?language=objc"))

;;; Assets

(define-objc-class "NSDataAsset" ()
  ()
  (:documentation
   "An object from a data set type stored in an asset catalog.
see https://developer.apple.com/documentation/appkit/nsdataasset?language=objc"))

;;; Nib Files

(define-objc-class "NSNib" ()
  ()
  (:documentation
   "An object wrapper, or container, for an Interface Builder nib file.
see https://developer.apple.com/documentation/appkit/nsnib?language=objc"))

(define-objc-class "NSNibConnector" ()
  ()
  (:documentation
   "A connection between two nibs.
see https://developer.apple.com/documentation/appkit/nsnibconnector?language=objc"))

(define-objc-class "NSNibControlConnector" ()
  ()
  (:documentation
   "A control connection between two Interface Builder objects.
see https://developer.apple.com/documentation/appkit/nsnibcontrolconnector?language=objc"))

(define-objc-class "NSNibOutletConnector" ()
  ()
  (:documentation
   "An outlet connection between Interface Builder objects.
see https://developer.apple.com/documentation/appkit/nsniboutletconnector?language=objc"))


;;;; App Extensions
;; Extend your app’s basic functionality to other parts of the system.
;; see https://developer.apple.com/documentation/appkit/app-extensions?language=objc

;;; Extension Support

;;; Quick Actions

;;; Mail Extensions

;;; UTI Subtypes for Data Detector Types


;;;; Views and Controls
;; Present your content onscreen and handle user input and events.
;; see https://developer.apple.com/documentation/appkit/views-and-controls?language=objc

;;; View fundamentals

(define-objc-mask ns-autoresizing-mask-options
  "Constants that specify the autoresizing behaviors for views.
see https://developer.apple.com/documentation/appkit/nsview/autoresizingmask-swift.struct?language=objc"
  "Getting the Autoresizing mask"
  (:not-sizable    0  "The view cannot be resized.")
  (:min-x-margin   1  "The left margin between the view and its superview is flexible.")
  (:width-sizable  2  "The view’s width is flexible.")
  (:max-x-margin   4  "The right margin between the view and its superview is flexible.")
  (:min-y-margin   8  "The bottom margin between the view and its superview is flexible.")
  (:height-sizable 16 "The view’s height is flexible.")
  (:max-y-margin   32 "The top margin between the view and its superview is flexible."))

(define-objc-class "NSView" ()
  (;;; Configuring the view
   ;; View Hierarchy
   ("superview"
    :reader superview
    :documentation
    "The view that is the parent of the current view.
see https://developer.apple.com/documentation/appkit/nsview/superview?language=objc")
   ("subviews"
    :reader subviews
    :after  ns-array-to-list
    :documentation
    "The array of views embedded in the current view.
see https://developer.apple.com/documentation/appkit/nsview/subviews?language=objc")
   ("window"
    :reader window
    :documentation
    "The view’s window object, if it is installed in a window.

The value of this property is nil if the view is not currently
installed in a window.

see https://developer.apple.com/documentation/appkit/nsview/window?language=objc")
   ("needsDisplay"
    :accessor needs-display-p
    :documentation
    "A Boolean value that determines whether the view needs to be
redrawn before being displayed.

The displayIfNeeded methods check the value of this property to avoid
unnecessary drawing, and all display methods set the value back to
`nil' when the view is up to date.

Whenever the data or state affecting the view’s appearance changes,
set this property to `t'. This marks the view as needing to update
its display. On the next pass through the app’s event loop, the view
is automatically redisplayed.

see https://developer.apple.com/documentation/appkit/nsview/needsdisplay?language=objc")
   ("frame"
    :accessor frame
    :documentation
    "The view’s frame rectangle, which defines its position and size
in its superview’s coordinate system.

Changing the value of this property repositions and resizes the view
within the coordinate system of its superview. Changing the frame does
not mark the view as needing to be displayed. Set the needsDisplay
property to true when you want the view to be redisplayed.

If your view does not use a custom bounds rectangle, this method also
sets the view’s bounds to match the size of the new frame. You can
specify a custom bounds rectangle by changing the bounds property or
by calling the setBoundsOrigin: or setBoundsSize: method
explicitly. Once set, the view creates an internal transform to
convert from frame coordinates to bounds coordinates. As long as the
width-to-height ratio of the two coordinate systems remains the same,
your content appears normal. If the ratios differ, your content may
appear skewed.

The frame rectangle may be rotated relative to its superview’s
coordinate system. For more information, see the frameRotation
property.

Changing the value of this property results in the posting of an
NSViewFrameDidChangeNotification to the default notification center if
the view is configured to do so.

see https://developer.apple.com/documentation/appkit/nsview/frame?language=objc")
   ;;; Managing the view's content
   ;; Layout
   ("autoresizingMask"
    :accessor autoresizing-mask
    :before   as-ns-autoresizing-mask-options
    :after    decode-ns-autoresizing-mask-options
    :documentation
    "The options that determine how the view is resized relative to
its superview.
see https://developer.apple.com/documentation/appkit/nsview/autoresizingmask-swift.property?language=objc"))
  (:documentation
   "The infrastructure for drawing, printing, and handling events in an app.

You typically don’t use `ns-view' objects directly. Instead, you use
objects that descend from NSView or you subclass NSView yourself and
override its methods to implement the behavior you need. An instance
of the NSView class (or one of its subclasses) is commonly known as a
view object, or simply as a view.

Views handle the presentation and interaction with your app’s visible
content. You arrange one or more views inside an NSWindow object,
which acts as a wrapper for your content. A view object defines a
rectangular region for drawing and receiving mouse events. Views
handle other chores as well, including the dragging of icons and
working with the NSScrollView class to support efficient scrolling.

AppKit handles most of your app’s `ns-view' management. Unless you’re
implementing a concrete subclass of `ns-view' or working intimately with
the content of the view hierarchy at runtime, you don’t need to know
much about this class’s interface. For any view, there are many
methods that you can use as-is. The following methods are commonly
used.

+ `frame'  returns the location and size of the `ns-view' object.
+ `bounds' returns the internal origin and size of the `ns-view' object.
+ `needs-display-p' determines whether the NSView object needs to be
  redrawn.
+ `window' returns the `ns-window' object that contains the `ns-view'
  object.
+ `draw-rect' draws the `ns-view' object.
  All subclasses must implement this method, but it’s rarely invoked
  explicitly.
  An alternative to drawing is to update the layer directly using the
  updateLayer method.

For more information on how `ns-view' instances handle event and action
messages, see Cocoa Event Handling Guide. For more information on
displaying tooltips and contextual menus, see Displaying Contextual
Menus and Managing Tooltips.

Subclassing notes
`ns-view' is perhaps the most important class in AppKit when it comes to
subclassing and inheritance. Most user-interface objects you see in a
Cocoa application are objects that inherit from NSView. If you want to
create an object that draws itself in a special way, or that responds
to mouse clicks in a special way, you would create a custom subclass
of `ns-view' (or of a class that inherits from NSView). Subclassing
NSView is such a common and important procedure that several technical
documents describe how to both draw in custom subclasses and respond
to events in custom subclasses. See Cocoa Drawing Guide and Cocoa
Event Handling Guide (especially “Handling Mouse Events” and “Mouse
Events”).

Handling events in your subclass
If you subclass `ns-view' directly and handle specific types of events,
don’t call super in the implementations of your event-related
methods. Views inherit their event-handling capabilities from their
`ns-responder' parent class. The default behavior for responders is to
pass events up the responder chain, which isn’t the behavior you
typically want for a custom view. Therefore, don’t call super if your
view implements any of the following methods and handles the event:

+ `mouse-down'
+ `mouse-dragged'
+ `mouse-up'
+ `mouse-moved'
+ `mouse-entered'
+ `mouse-exited'
+ `right-mouse-dragged'
+ `right-mouse-up'
+ `other-mouse-down'
+ `other-mouse-dragged'
+ `other-mouse-up'
+ `scroll-wheel'
+ `key-down'
+ `key-up'
+ `flags-changed'
+ `tablet-point'
+ `tablet-proximity'

Note
`ns-view' changes the default behavior of `right-mouse-down' so that it
calls `menu-for-event' and, if non nil, presents the contextual menu. In
macOS 10.7 and later, if the event is not handled, `ns-view' passes the
event up the responder chain. Because of these behaviorial changes,
call super when implementing `right-mouse-down' in your custom NSView
subclasses.

If your view descends from a class other than `ns-view', call super to
let the parent view handle any events that you don’t.

see https://developer.apple.com/documentation/appkit/nsview?language=objc"))

;;; Creating a view object

;;; Configuring the view

;;; View Hierarchy
;; Manage the subviews, superview, and window of a view and respond to
;; notifications when the view hierarchy changes.
;; see https://developer.apple.com/documentation/appkit/view-hierarchy?language=objc

;; Getting the Related Objects
;; Adding and Removing Subviews

(define-objc-enum ns-window-ordering-mode
  "Constants that let you specify how a window is ordered relative to
another window.

For more information, see orderWindow:relativeTo:.

see https://developer.apple.com/documentation/appkit/nswindow/orderingmode?language=objc"
  (:above 1                  "Moves the window above the indicated window.")
  (:below #xFFFFFFFFFFFFFFFF "Moves the window below the indicated window.")
  (:out   0                  "Moves the window off the screen."))

(defgeneric add-subview (view subview &key &allow-other-keys)
  (:documentation "Add SUBVIEW to VIEW. ")
  (:method :around (view subview &key)
    "Return SUBVIEW. "
    (call-next-method)
    subview)
  (:method ((view ns-view) (subview ns-view)
            &key
              (frame       nil    frame?)
              (positioned  :above positioned?)
              (relative-to nil    relative-to?))
    "Add SUBVIEW to VIEW.

Parameters:
+ POSITIONED: can be `:above' and `:below'
  see `ns-window-ordering-mode' (default `:above')
+ RELATIVE-TO: (`ns-view')
  the other `ns-view' SUBVIEW should be positioned relative to.
  if `nil' or isn't a subview of the VIEW, SUBVIEW will be added
  above or below all of its neww siblings
+ FRAME: (`ns-rect')
  setting the location and size of SUBVIEW within VIEW
  see `frame' property
"
    (declare (type (member :above :below) positioned)
             (type (or null ns-view) relative-to)
             (type (or null ns-rect) frame))
    (when frame? (setf (frame subview) frame))
    (if (or positioned? relative-to?)
        (invoke view "addSubview:positioned:relativeTo:"
                subview
                (as-ns-window-ordering-mode positioned)
                relative-to)
        (invoke view "addSubview:" subview))))

(defgeneric remove-from-superview (view &key &allow-other-keys)
  (:documentation
   "Unlinks the view from its superview and its window, removes it
from the responder chain, and invalidates its cursor rectangles.")
  (:method ((view ns-view) &key (displayp t))
    "If DISPLAYP is nil, invoke removeFromSuperviewWithoutNeedingDisplay;
otherwise invoke removeFromSuperview. "
    (if displayp
        (invoke view "removeFromSuperview")
        (invoke view "removeFromSuperviewWithoutNeedingDisplay"))))

(defmethod replace-subview ((view ns-view) (old ns-view) (new ns-view))
  "Replaces one of the view’s subviews with another view.

Parameters:
+ VIEW: superview
+ OLD: The view to be replaced by newView. May not be nil.
+ NEW: The view to replace oldView. May not be nil.

This method does nothing if oldView is not a subview of the view.

Neither oldView nor newView may be nil, and the behavior is undefined
if either of these parameters is nil.

This method causes oldView to be released; if you plan to reuse it, be
sure to retain it before sending this message and to release it as
appropriate when adding it as a subview of another NSView.

Calling this method also removes any constraints associated with
oldView and its subtree."
    (invoke view "replaceSubview:with:" old new))

;; Responding to View-Related Notifications
;; Identifying Views by Tag

;;; Managing interactions

(define-objc-enum ns-text-alignment
  "Constants that specify text alignment.
see https://developer.apple.com/documentation/appkit/nstextalignment?language=objc"
  (:left       0 "Text is left-aligned.")
  (:right      2 "Text is right-aligned.")
  (:center     1 "Text is center-aligned.")
  (:justified  3 "Text is justified.")
  (:natural    4 "Text uses the default alignment for the current"
               "localization of the app."))

(define-objc-enum ns-line-break-mode
  "Constants that specify what happens when a line is too long for a container.
see https://developer.apple.com/documentation/appkit/nslinebreakmode?language=objc"
  (:word-wrapping     0
                      "The value that indicates wrapping occurs at word "
                      "boundaries, unless the word doesn’t fit on a "
                      "single line.")
  (:char-wrapping     1
                      "The value that indicates wrapping occurs before the"
                      "first character that doesn’t fit.")
  (:clipping          2
                      "The value that indicates lines don’t extend past the "
                      "edge of the text container.")
  (:truncating-head   3
                      "The value that indicates that a line displays"
                      "so that the end fits in the container and an"
                      "ellipsis glyph indicates the missing text at the"
                      "beginning of the line.")
  (:truncating-tail   4
                      "The value that indicates a line displays so"
                      "that the beginning fits in the container and an"
                      "ellipsis glyph indicates the missing text at the"
                      "end of the line.")
  (:truncating-middle 5
                      "The value that indicates that a line displays"
                      "so that the beginning and end fit in the"
                      "container and an ellipsis glyph indicates the"
                      "missing text in the middle."))

(define-objc-class "NSControl" ()
  (;; Enabling and Disabling the Control
   ("enabled"
    :accessor enabledp
    :documentation
    "A Boolean value that indicates whether the receiver reacts to
mouse events.

The value of this property is `t' if the receiver responds to mouse
events; otherwise, `nil'.

see https://developer.apple.com/documentation/appkit/nscontrol/isenabled?language=objc")
   ;; Accessing the Control's Value
   ("stringValue"
    :reader string-value          ; see (setf string-value) for writer
    :after  ns-string-to-string
    :documentation
    "The value of the receiver’s cell as an NSString object.

If the control contains many cells (for example, NSMatrix), then this
property contains the value of the currently selected cell. If the
control is in the process of editing the affected cell, then it
invokes the validateEditing method before getting the value.

If the cell is being edited, setting this property aborts all editing
before setting the value. If the cell does not inherit from
NSActionCell, setting this property marks the cell’s interior as
needing to be redisplayed; NSActionCell performs its own updating of
cells.

see https://developer.apple.com/documentation/appkit/nscontrol/stringvalue?language=objc")
   ;; Formatting Text
   ("alignment"
    :accessor alignment
    :before   as-ns-text-alignment
    :after    decode-ns-text-alignment
    :documentation
    "The alignment mode of the text in the receiver’s cell.

The value of this property can be one of the following constants:
NSLeftTextAlignment, NSRightTextAlignment,NSCenterTextAlignment,
NSJustifiedTextAlignment, or NSNaturalTextAlignment. The default value
is NSNaturalTextAlignment. Setting this property while the cell is
currently being edited aborts the edits to change the alignment.

see https://developer.apple.com/documentation/appkit/nscontrol/alignment?language=objc")
   ("font"
    :accessor font
    :before   as-ns-font
    :documentation
    "The font used to draw text in the receiver’s cell.

If the cell is being edited, setting this property causes the text in
the cell to be redrawn in the new font, and the cell’s editor (the
`ns-text' object used globally for editing) is updated with the new
font object.

see https://developer.apple.com/documentation/appkit/nscontrol/font?language=objc")
   ("lineBreakMode"
    :accessor line-break-mode
    :before   ns-line-break-mode
    :after    decode-ns-line-break-mode
    :documentation
    "The line break mode to use for text in the control’s cell.

see `ns-line-break-mode'

see https://developer.apple.com/documentation/appkit/nscontrol/linebreakmode?language=objc")
   ;; Implementing the Target-Action Mechanism
   ("action"
    :accessor action
    :before   coerce-to-selector
    :documentation
    "The default action-message selector associated with the control.
see https://developer.apple.com/documentation/appkit/nscontrol/action?language=objc")
   ("target"
    :accessor target
    :documentation
    "The target object that receives action messages from the cell.
see https://developer.apple.com/documentation/appkit/nscontrol/target?language=objc")
   ("continuous"
    :accessor continuousp
    :documentation
    "A Boolean value indicating whether the receiver’s cell sends its
action message continuously to its target during mouse tracking.
see https://developer.apple.com/documentation/appkit/nscontrol/iscontinuous?language=objc"))
  (:documentation
   "A specialized view, such as a button or text field, that notifies
your app of relevant events using the target-action design pattern.

see https://developer.apple.com/documentation/appkit/nscontrol?language=objc"))

(defmethod (setf string-value) ((string string) (control ns-control))
  (invoke control "setStringValue:" (string-to-ns-string string)))

(defmethod (setf string-value) ((string ns-string) (control ns-control))
  (invoke control "setStringValue:" string))

(define-objc-class "NSCell" ()
  ()
  (:documentation
   "A mechanism for displaying text or images in a view object without
the overhead of a full NSView subclass.
see https://developer.apple.com/documentation/appkit/nscell?language=objc"))

(define-objc-class "NSActionCell" ()
  ()
  (:documentation
   "An active area inside a control.
see https://developer.apple.com/documentation/appkit/nsactioncell?language=objc"))

;;; Container views

(define-objc-class "NSSplitView" ()
  ()
  (:documentation
   "A view that arranges two or more views in a linear stack running horizontally or vertically.
see https://developer.apple.com/documentation/appkit/nssplitview?language=objc"))

(define-objc-class "NSStackView" ()
  ()
  (:documentation
   "A view that arranges an array of views horizontally or vertically and updates their placement and sizing when the window size changes.
see https://developer.apple.com/documentation/appkit/nsstackview?language=objc"))

(define-objc-class "NSTabView" ()
  ()
  (:documentation
   "A multipage interface that displays one page at a time.
see https://developer.apple.com/documentation/appkit/nstabview?language=objc"))

;;; Content views

(define-objc-class "NSTextView" ()
  ()
  (:documentation
   "A view that draws text and handles user interactions with that text.
see https://developer.apple.com/documentation/appkit/nstextview?language=objc"))

;;; Controls

(define-objc-enum ns-button-type
  "Button types that you can specify using setButtonType:.

For examples of how these types behave, see Button Programming Topics.

see https://developer.apple.com/documentation/appkit/nsbutton/buttontype?language=objc"
  "Configuring Button Behavior"
  (:momentary-push-in       7 "A button that illuminates when the user clicks it. ")
  (:momentary-light         0
                            "A button that displays a highlight when the user"
                            "clicks it and returns to its normal state when"
                            "the user releases it.")
  (:momentary-change        5
                            "A button that displays its alternate content"
                            "when clicked and returns to its normal content"
                            "when the user releases it.")
  (:push-on-push-off        1
                            "A button that switches between on and off states "
                            "with each click.")
  (:on-off                  6
                            "A button that switches between a normal and "
                            "emphasized bezel on each click.")
  (:toggle                  2
                            "A button that switches between its normal "
                            "and alternate content on each click.")
  (:switch                  3 "A standard checkbox button.")
  (:radio                   4
                            "A button that displays a single selected "
                            "value from group of possible choices.")
  (:accelerator             8
                            "A button that sends repeating actions as pressure "
                            "changes occur.")
  (:multi-level-accelerator 9
                            "A button that allows for a configurable number of "
                            "stepped pressure levels and provides tactile feedback "
                            "as the user reaches each step."))

(declaim (type (satisfies ns-button-type-p) *ns-button-type*))
(defparameter *ns-button-type* :momentary-light
  "Default `ns-button-type'. ")

(define-objc-enum ns-cell-image-position
  "A constant for specifying the position of a button’s image relative
to its title.

Use these constants with the imagePosition property of NSButton and
NSButtonCell.

see https://developer.apple.com/documentation/appkit/nscontrol/imageposition?language=objc"
  "Positioning a Control's Image"
  (:no-image       0 "The cell doesn’t display an image.")
  (:image-only     1 "The cell displays an image but not a title.")
  (:image-leading  7 "The image is on the title’s leading edge.")
  (:image-trailing 8 "The image is on the title’s trailing edge.")
  (:left           2 "The image is to the left of the title.")
  (:right          3 "The image is to the right of the title.")
  (:below          4 "The image is below the title.")
  (:above          5 "The image is above the title.")
  (:overlaps       6 "The image overlaps the title."))

(define-objc-enum ns-bezel-style
  "The set of bezel styles to style buttons in your app.

For design guidance on buttons, see Human Interface Guidelines > Buttons.

see https://developer.apple.com/documentation/appkit/nsbutton/bezelstyle-swift.enum?language=objc"
  "Default"
  (:automatic            0
                         "The default button style based on the"
                         "button’s contents and position within the window.")
  "Push"
  (:push                 1  "A standard push style button.")
  (:flexible-push        2
                         "A push button with a flexible height to accommodate "
                         "longer text labels or an image.")
  "Disclosure"
  (:disclosure           5  "A bezel style button for use with a disclosure triangle.")
  (:push-disclosure      14 "A bezel style push button with a disclosure triangle.")
  "Toolbar"
  (:toolbar              11 "A button style that’s appropriate for a toolbar item.")
  (:accessory-bar        13
                         "A button style that’s typically used in the context of "
                         "an accessory toolbar for buttons that narrow the focus of "
                         "a search or other operation.")
  (:accessory-bar-action 12
                         "A button style that you use for extra actions in an "
                         "accessory toolbar.")
  "Informational"
  (:help-button          9
                         "A round button with a question mark, providing the "
                         "standard help button look.")
  (:badge                15
                         "A button style suitable for displaying additional "
                         "information.")
  (:circular             7
                         "A round button that can contain either a single character "
                         "or an icon.")
  "Other"
  (:small-square         10 "A simple square bezel style that can scale to any size."))

(define-objc-enum ns-control-state-value
  "A constant that indicates whether a control is on, off, or in a mixed state.

see https://developer.apple.com/documentation/appkit/nscontrol/statevalue?language=objc"
  (:on    1  "Control is on or selected.")
  (:off   0  "Control is off or unselected.")
  (:mixed -1 "Control is in a mixed state, neither on nor off."))

(define-objc-class "NSButton" ()
  (;; Configuring buttons
   (%button-type                        ; (setf button-type)
    :reader button-type
    :documentation
    "see `ns-button-type'")
   ("title"
    :accessor title
    :before   as-ns-string
    :documentation
    "The title displayed on the button when it’s in an off state.
see https://developer.apple.com/documentation/appkit/nsbutton/title?language=objc")
   ("alternateTitle"
    :accessor alternate-title
    :before   as-ns-string
    :documentation
    "The title that the button displays when the button is in an on state.
see https://developer.apple.com/documentation/appkit/nsbutton/alternatetitle?language=objc")
   ;; Configuring button images
   ("image"
    :accessor image
    ;; :before   as-ns-image
    :documentation
    "The image that appears on the button when it’s in an off state,
or nil if there is no such image.
see https://developer.apple.com/documentation/appkit/nsbutton/image?language=objc")
   ("alternateImage"
    :accessor alternate-image
    ;; :before as-ns-image
    :documentation
    "An alternate image that appears on the button when the button is
in an on state.
see https://developer.apple.com/documentation/appkit/nsbutton/alternateimage?language=objc")
   ("imagePosition"
    :accessor image-position
    :before   as-ns-cell-image-position
    :after    decode-ns-cell-image-position
    :documentation
    "The position of the button’s image relative to its title.
see https://developer.apple.com/documentation/appkit/nsbutton/imageposition?language=objc")
   ("bordered"
    :accessor borderedp
    :documentation
    "A Boolean value that determines whether the button has a border.
see https://developer.apple.com/documentation/appkit/nsbutton/isbordered?language=objc")
   ("transparent"
    :accessor transparentp
    :documentation
    "A Boolean value that indicates whether the button is transparent.
see https://developer.apple.com/documentation/appkit/nsbutton/istransparent?language=objc")
   ("bezelStyle"
    :accessor bezel-style
    :before   as-ns-bezel-style
    :after    decode-ns-bezel-style
    :documentation
    "The appearance of the button’s border.
see https://developer.apple.com/documentation/appkit/nsbutton/bezelstyle-swift.property?language=objc")
   ("bezelColor"
    :accessor bezel-color
    :before   as-ns-color
    :documentation
    "The color of the button’s bezel, in appearances that support it.
see https://developer.apple.com/documentation/appkit/nsbutton/bezelcolor?language=objc")
   ("showsBorderOnlyWhileMouseInside"
    :accessor shows-border-only-while-mouse-inside-p
    :before   as-boolean
    :documentation
    "A Boolean value that determines whether the button displays its
border only when the pointer is over it.
see https://developer.apple.com/documentation/appkit/nsbutton/showsborderonlywhilemouseinside?language=objc")
   ;; Managing button state
   ("allowsMixedState"
    :accessor allows-mixed-state-p
    :before   as-boolean
    :documentation
    "A Boolean value that indicates whether the button allows a mixed state.

The value of this property is true if the button has three states (on,
off, and mixed), or false if the button has two states (on and
off). The default value is false. On and off states (also referred to
as alternate and normal) indicate that the button is either clicked or
not clicked. Mixed state is typically used for checkboxes or radio
buttons. For example, suppose the state of a checkbox is used to
denote whether a text field contains bold text. If all of the text in
the text field is bold, then the checkbox appears checked (on). If
none of the text is bold, then the checkbox appears unchecked
(off). If some of the text is bold, then the checkbox contains a dash
(mixed).

see https://developer.apple.com/documentation/appkit/nsbutton/allowsmixedstate?language=objc")
   ("state"
    :accessor state
    :before   as-ns-control-state-value
    :after    decode-ns-control-state-value
    :documentation
    "The button’s state.

The value of this property represents the button’s state. A button can
have two or three states. If it has two, this value is either on
(NSOnState) or off (NSOffState). If it has three, this value is on,
off, or mixed (NSMixedState). A three-state button can be enabled by
calling the allowsMixedState method. On and off states (also referred
to as alternate and normal) indicate that the button is either clicked
or not clicked. Mixed state is typically used for checkboxes or radio
buttons, which allow for an additional intermediate state. For
example, suppose the state of a checkbox is used to denote whether a
text field contains bold text. If all of the text in the text field is
bold, then the checkbox appears checked (on). If none of the text is
bold, then the checkbox appears unchecked (off). If some of the text
is bold, then the checkbox contains a dash (mixed).

Note that if the button has only two states and you set the value of
state to mixed, the button’s state changes to on. Setting this
property redraws the button, if necessary.

Although using the enumerated constants is preferred, you can also set
state to an integer value. If the button has two states, 0 is treated
as NSOffState, and a nonzero value is treated as NSOnState. If the
button has three states, 0 is treated as NSOffState; a negative value,
as NSMixedState; and a positive value, as NSOnState.

To check whether the button uses the mixed state, use the
allowsMixedState property.

see https://developer.apple.com/documentation/appkit/nsbutton/state?language=objc"))
  (:documentation
   "A control that defines an area on the screen that a user clicks to
trigger an action.

Buttons are a standard control for initiating actions within your
app. You can configure buttons with many different visual styles, but
the behavior is the same. When a user clicks it, a button calls the
action method of its associated target object. (If you configure a
button as continuous, it calls its action method at timed intervals
until the user releases the mouse button or the cursor leaves the
button boundaries). You use the action method to perform your
app-specific tasks.

There are multiple types of buttons, each with a different user
interface and behavior. The NSButtonCell class defines the button
types, and calling the setButtonType: method configures them.

If you configure it as an accelerator button (type NSAcceleratorButton
or NSMultiLevelAcceleratorButton), you can set a button to send action
messages when changes in pressure occur when the user clicks the
button.

Buttons can either have two states (on and off) or three states (on,
off, and mixed). You enable a three-state button by calling the
allowsMixedState method. On and off (also referred to as alternate and
normal) states indicate that the user clicked or didn’t click the
button. Mixed is typically used for checkboxes or radio buttons, which
allow for an additional intermediate state. For example, suppose the
state of a checkbox denotes whether a text field contains bold
text. If all text in the text field is bold, then the checkbox is
on. If none of the text is bold, then the checkbox is off. If some of
the text is bold, then the checkbox is mixed.

For most types of buttons, the value of the button matches its
state—the value is 1 for on, 0 for off, or -1 for mixed. For
pressure-sensitive buttons, the value of the button indicates pressure
level instead.

NSButton and NSMatrix both provide a control view, which displays an
NSButtonCell object. However, while a matrix requires you to access
the button cell objects directly, most button class methods act as
“covers” for identically declared button cell methods. In other words,
the implementation of the button method invokes the corresponding
button cell method for you, allowing you to be unconcerned with the
existence of the button cell. The only button cell methods that don’t
have covers relate to the font you use to display the key equivalent
and to specific methods for highlighting or showing the state of the
button.

see https://developer.apple.com/documentation/appkit/nsbutton?language=objc"))

(defmethod (setf button-type) (button-type (button ns-button))
  (declare (type ns-button-type button-type))
  (invoke button "setButtonType:" (as-ns-button-type button-type))
  (setf (slot-value button '%button-type) button-type))

(defmethod init ((button ns-button)
                 &key frame
                   (button-type *ns-button-type*)
                   (title       nil  title?)
                   (image       nil  image?)
                   (borderedp   nil  bordered?)
                   (bezel-style nil  bezel-style?)
                   (bezel-color nil  bezel-color?)
                   (target      nil  target?)
                   (action      nil  action?)
                   (state       :off state?))
  "Creates a standard push button with the title you specify.

Parameters:
+ TARGET: The target object that receives action messages from the control.
+ ACTION: The action the button sends to the target.
  see `coerce-to-selector'
+ TITLE:  The localized title string to display on the button.
+ IMAGE:  The image to display in the body of the button.
+ BORDEREDP: if BUTTON has border
  see `borderedp'
+ BEZEL-STYLE: appearance of button border (`ns-bezel-style')
  see `bezel-style'
+ BEZEL-COLOR: color of the button’s bezel (`as-ns-color')
  see `bezel-color'
+ BUTTON-TYPE: button type (`*ns-button-type*')
  see `ns-button-type'

Dev Note:
at least one of TITLE and IMAGE should be given.
"
  (declare (type ns-rect* frame)
           (type (or null standard-objc-object) target)
           (type ns-control-state-value         state))
  (unless (or title? image?)
    (error "At least one of `:title' and `:image' should be given. "))
  (let ((button (invoke button "initWithFrame:" frame)))
    (when title?  (setf (title  button) title))
    (when image?
      (setf (image  button) image)
      ;; Configuring button image
      (when bordered?    (setf (borderedp   button) borderedp))
      (when bezel-style? (setf (bezel-style button) bezel-style))
      (when bezel-color? (setf (bezel-color button) bezel-color)))
    (when target? (setf (target button) target))
    (when action? (setf (action button) action))
    (when state?  (setf (state  button) state))
    (setf (button-type button) button-type)
    button))

(define-objc-class "NSColorWell" ()
  ()
  (:documentation
   "A control that displays a color value and lets the user change that color value.
see https://developer.apple.com/documentation/appkit/nscolorwell?language=objc"))

(define-objc-enum ns-combo-button-style
  "Constants that indicate how a combo button presents its menu.
see https://developer.apple.com/documentation/appkit/nscombobutton/style-swift.enum?language=objc"
  (:split   0
            "A style that separates the button’s title and image"
            "from the menu" "indicator people use to activate the"
            "button.")
  (:unified 1
            "A style that unifies the button’s title and image with"
            "the menu indicator."))

(define-objc-enum ns-image-scaling
  "Constants that specify a cell’s image scaling behavior.
see https://developer.apple.com/documentation/appkit/nsimagescaling?language=objc"
  (:proportionally-down
   0 "If it is too large for the destination, scale"
   "the image down while preserving the aspect ratio.")
  (:axes-independently
   1 "Scale each dimension to exactly fit destination.")
  (:none
   2 "Do not scale the image.")
  (:proportionally-up-or-down
   3 "Scale the image to its maximum possible"
   "dimensions while both staying within the destination area and"
   "preserving its aspect ratio."))

(define-objc-class "NSComboButton" ()
  (;; Configuring the Button Appearance
   ("style"
    :accessor style
    :before   as-ns-combo-button-style
    :after    decode-ns-combo-button-style
    :documentation
    "The appearance setting that determines how the button presents its menu.
see https://developer.apple.com/documentation/appkit/nscombobutton/style-swift.property?language=objc")
   ("title"
    :accessor title
    :before   as-ns-string
    :after    ns-string-to-string
    :documentation
    "The localized string that the button displays.
see https://developer.apple.com/documentation/appkit/nscombobutton/title?language=objc")
   ("image"
    :accessor image
    :before   as-ns-image
    :documentation
    "The image that the button displays.
see https://developer.apple.com/documentation/appkit/nscombobutton/image?language=objc")
   ("imageScaling"
    :accessor image-scaling
    :before   as-ns-image-scaling
    :after    decode-ns-image-scaling
    :documentation
    "The scaling behavior to apply to the button’s image.
see https://developer.apple.com/documentation/appkit/nscombobutton/imagescaling?language=objc")
   ;; Specifying the Alternative Actions
   ("menu" ; ns-menu
    :accessor menu
    :before   as-ns-menu
    :documentation
    "The menu that contains the button’s alternate actions.

The combo button executes the menu item’s action when someone selects
that item, so make sure to configure the targets and actions for each
menu item in your menu.

An NSComboButton doesn’t support the addition of a contextual menu.

see https://developer.apple.com/documentation/appkit/nscombobutton/menu?language=objc"))
  (:documentation
   "A button with a pull-down menu and a default action.

An NSComboButton object is a button that displays a title string,
image, and an optional control for displaying a menu. Use this control
in places where you want to offer a button with a default action and
one or more alternative actions. Clicking the title or image executes
the default action you provide, and clicking the menu control displays
a menu for selecting a different action. If you configure the button
to hide the menu control, a long-press gesture displays the menu.

After you create a combo button programmatically or in Interface
Builder, choose the button style you want and add a title or image for
your content. A combo button has a default action, which you specify
at creation time. You can also change that action later using the
inherited target and action properties. To specify one or more
alternative actions, configure a menu with those actions and assign it
to the button’s menu property.

This control doesn’t use an NSCell object for its underlying
implementation. It also doesn’t support the addition of a contextual
menu.

see https://developer.apple.com/documentation/appkit/nscombobutton?language=objc"))

(defmethod init ((combo ns-combo-button)
                 &key
                   (title ""  title?)
                   (image nil image?)
                   (image-scaling :proportionally-down)
                   menu
                   (style :split style?)
                   frame
                   target
                   action
                 &allow-other-keys)
  "Initialize `ns-combo-button'.

Parameters:
+ TITLE:
  localized string that the button displays.
+ IMAGE (`ns-image')
  image that the button displays.
+ IMAGE-SCALING (`ns-image-scaling')
  scaling behavior to apply to the button’s image.
+ MENU
+ STYLE (`ns-combo-button-style')
  appearance setting that determines how the button presents its menu.
+ FRAME (`ns-rect')
  bouding frame of COMBO
+ TARGET
  target of COMBO action to send
+ ACTION
  action of COMBO, triggered when clicking button left
"
  (declare (type ns-image-scaling image-scaling)
           (type ns-combo-button-style style)
           (type ns-rect* frame))
  (let ((combo (invoke combo "initWithFrame:" frame)))
    (when title? (setf (title combo) title))
    (when style? (setf (style combo) style))
    (when image?
      (setf (image         combo) image
            (image-scaling combo) image-scaling))
    (setf (target combo) target
          (action combo) action
          (menu   combo) menu)))

(define-objc-class "NSComboBox" ()
  (;; Setting Display Attributes
   ("hasVerticalScroller"
    :accessor has-vertical-scroller-p
    :before   as-boolean
    :documentation
    "Whether the combo box has a vertical scroller.

When the value of this property is true, the combo box displays a
vertical scroller even when the pop-up list contains few enough items
that a scroller is not needed. The default value of this property is
true.

If the value of this property is false and the combo box has more list
items (either in its internal item list or from its data source) than
are allowed by numberOfVisibleItems, only a subset of items are
displayed. The NSComboBox class’ scroll... methods can be used to
position this subset within the pop-up list.

see https://developer.apple.com/documentation/appkit/nscombobox/hasverticalscroller?language=objc")
   ("intercellSpacing"
    :accessor intercell-spacing
    :documentation
    "The horizontal and vertical spacing between cells in the pop-up list.

Spacing values are measured in points.
The default spacing is (3.0, 2.0).

see https://developer.apple.com/documentation/appkit/nscombobox/intercellspacing?language=objc")
   ("buttonBordered"
    :accessor borderedp
    :before   as-boolean
    :documentation
    "Whether the combo box displays a border.

When the value of this property is true, the combo box displays a
border. For example, when displaying a combo box in a table, it is
often useful to display the combo box without a border. The default
value of this property is true.

see https://developer.apple.com/documentation/appkit/nscombobox/isbuttonbordered?language=objc")
   ("itemHeight"
    :accessor item-height
    :documentation
    "The height of each item in the pop-up list.

The height of items is measured in points.
The default item height is 16.0 points.

see https://developer.apple.com/documentation/appkit/nscombobox/itemheight?language=objc")
   ("numberOfVisibleItems"
    :accessor number-of-visible-items
    :documentation
    "The maximum number of visible items to display in the pop-up list
at one time.

Use this property to configure how many items can be displayed at the
same time. If the combo box has a scroller, the user can scroll to
view additional items beyond the visible range.

see https://developer.apple.com/documentation/appkit/nscombobox/numberofvisibleitems?language=objc")
   ;; Configuring the Combo Box Items
   ("objectValues"
    :reader items
    :after  ns-array-to-list
    :documentation
    "A list of the items from the combo box’s internal list.

The array contains the objects you added or inserted into the combo
box, so the type of each object can vary. Accessing this property logs
a warning if the usesDataSource property is true.

see https://developer.apple.com/documentation/appkit/nscombobox/objectvalues?language=objc")
   ("numberOfItems"
    :reader len
    :documentation
    "The total number of items in the pop-up list.
see https://developer.apple.com/documentation/appkit/nscombobox/numberofitems?language=objc")
   ;; Manipulating the Selection
   ("indexOfSelectedItem"
    :reader index-of-selected-item
    :documentation
    "The index of the last item selected from the pop-up list.

The value of this property is -1 if no item is selected; otherwise, it
is the index of the selected item. Nothing is selected in a newly
initialized combo box.

see https://developer.apple.com/documentation/appkit/nscombobox/indexofselecteditem?language=objc"))
  (:documentation
   "A view that displays a list of values in a pop-up menu where the
user selects a value or types in a custom value.

A combo box combines the behavior of an NSTextField object with an
NSPopUpButton object. A combo box displays a list of values from a
pop-up list, but also provides a means for users to type in custom
values. For example, here’s a combo box in its initial state.

Clicking in the text portion of the control allows the user to edit
the current value. When the user clicks the down arrow at the right
side of the text field, the pop-up list appears.

The NSComboBox class uses NSComboBoxCell to implement its user
interface.

Also see the NSComboBoxDataSource protocol, which declares the methods
that NSComboBox uses to access the contents of its data source object.

see https://developer.apple.com/documentation/appkit/nscombobox?language=objc"))

(defmethod delegate ((combo ns-combo-box))
  (invoke combo "delegate"))

(defmethod (setf delegate) (delegate (combo ns-combo-box))
  (invoke combo "setDelegate:" delegate))

(defmethod init ((combo ns-combo-box)
                 &key
                   frame
                   (has-vertical-scroller-p nil            vertical?)
                   (intercell-spacing       #(3.0d0 2.0d0) spacing?)
                   (borderedp               nil            bordered?)
                   (item-height             16.0d0         height?)
                   (number-of-visible-items 3              visible?)
                   (delegate :self)
                   items
                   (action nil action?)
                   (target nil target?)
                 &allow-other-keys)
  "Initialize instance of `ns-combo-box' of COMBO.

Parameters:
+ HAS-VERTICAL-SCROLLER-P
  Whether COMBO has a vertical scroller.
+ INTERCELL-SPACING (`ns-size')
  horizontal and vertical spacing between cells in the pop-up list.
+ BORDEREDP
  whether COMBO displays a border
+ ITEM-HEIGHT
  height of each item in the pop-up list in points
+ NUMBER-OF-VISIBLE-ITEMS
  maximum number of visible items to display in the pop-up list
  at one time.
+ DELEGATE
  the COMBO's delegate
+ ITEMS
  a list of items to insert into COMBO
+ ACTION
  actions triggered when user confirms (enter, select),
  or loss focus
+ TARGET
  target to send actions
"
  (let ((combo (invoke combo "initWithFrame:" frame)))
    (when vertical? (setf (has-vertical-scroller-p combo)
                          has-vertical-scroller-p))
    (when spacing?  (setf (intercell-spacing       combo)
                          intercell-spacing))
    (when bordered? (setf (borderedp   combo) borderedp))
    (when height?   (setf (item-height combo) item-height))
    (when visible?  (setf (number-of-visible-items combo)
                          number-of-visible-items))
    (setf (delegate combo) (if (eq delegate :self) combo delegate))
    (dolist (item items)
      (add-item combo item))
    (when action? (setf (action combo) action))
    (when target? (setf (target combo) target))))

(defmethod add-item ((combo ns-combo-box) item &key (index -1 index?))
  "Add ITEM to COMBO.

Parameters:
+ INDEX:
  index of where to insert,
  if not given, by default invokes addItemWithObjectValue:
  if given, invokes insertItemWithObjectValue:atIndex:
"
  (if index?
      (invoke combo "insertItemWithObjectValue:atIndex:" item index)
      (invoke combo "addItemWithObjectValue:" item)))

(defmethod add-item ((combo ns-combo-box) (string string) &rest keys &key)
  (apply #'add-item combo (string-to-ns-string string) keys))

(defmethod select-item-at-index ((combo ns-combo-box) (index integer))
  "Selects the pop-up list row at the given index.

Parameter:
+ The index of the item to select in the pop-up list.

Posts an NSComboBoxSelectionDidChangeNotification to the default
notification center if the selection does in fact change. Note that
this method does not alter the contents of the combo box’s text
field—see Setting the Combo Box’s Value for more information.

see https://developer.apple.com/documentation/appkit/nscombobox/selectitem(at:)?language=objc"
  (invoke combo "selectItemAtIndex:" index))

(defmethod (setf index-of-selected-item) (index (combo ns-combo-box))
  "Invokes `select-item-at-index'. "
  (invoke combo "selectItemAtIndex:" index))

(define-objc-class "NSImageView" ()
  ()
  (:documentation
   "A display of image data in a frame.
see https://developer.apple.com/documentation/appkit/nsimageview?language=objc"))

(define-objc-class "NSLevelIndicator" ()
  ()
  (:documentation
   "A visual representation of a level or quantity, using discrete values.
see https://developer.apple.com/documentation/appkit/nslevelindicator?language=objc"))

(define-objc-class "NSPopUpButton" ()
  ()
  (:documentation
   "A control for selecting an item from a list.
see https://developer.apple.com/documentation/appkit/nspopupbutton?language=objc"))

(define-objc-class "NSProgressIndicator" ()
  ()
  (:documentation
   "An interface that provides visual feedback to the user about the status of an ongoing task.
see https://developer.apple.com/documentation/appkit/nsprogressindicator?language=objc"))

(define-objc-class "NSRuleEditor" ()
  ()
  (:documentation
   "An interface for configuring a rule-based list of options.
see https://developer.apple.com/documentation/appkit/nsruleeditor?language=objc"))

(define-objc-class "NSPredicateEditor" ()
  ()
  (:documentation
   "A defined set of rules that allows the editing of predicate objects.
see https://developer.apple.com/documentation/appkit/nspredicateeditor?language=objc"))

(define-objc-class "NSSegmentedControl" ()
  ()
  (:documentation
   "Display one or more buttons in a single horizontal group.
see https://developer.apple.com/documentation/appkit/nssegmentedcontrol?language=objc"))

(define-objc-class "NSStepper" ()
  ()
  (:documentation
   "An interface with up and down arrow buttons for incrementing or decrementing a value.
see https://developer.apple.com/documentation/appkit/nsstepper?language=objc"))

(define-objc-class "NSSwitch" ()
  ()
  (:documentation
   "A control that offers a binary choice.
see https://developer.apple.com/documentation/appkit/nsswitch?language=objc"))

(define-objc-class "NSMatrix" ()
  ()
  (:documentation
   "A legacy interface for grouping radio buttons or other types of cells together.
see https://developer.apple.com/documentation/appkit/nsmatrix?language=objc"))

;;; Liquid Glass effects

;; (define-objc-class "NSGlassEffectView" ()
;;   ()
;;   (:documentation
;;    "A view that embeds its content view in a dynamic glass effect.
;; see https://developer.apple.com/documentation/appkit/nsglasseffectview?language=objc"))

;; (define-objc-class "NSGlassEffectContainerView" ()
;;   ()
;;   (:documentation
;;    "A view that efficiently merges descendant glass effect views together when they are within a specified proximity to each other.
;; see https://developer.apple.com/documentation/appkit/nsglasseffectcontainerview?language=objc"))

;;; Interacting with adjacent views

;; (define-objc-class "NSBackgroundExtensionView" ()
;;   ()
;;   (:documentation
;;    "A view that extends content to fill its own bounds.
;; see https://developer.apple.com/documentation/appkit/nsbackgroundextensionview?language=objc"))

;;; Visual adornments

(define-objc-class "NSVisualEffectView" ()
  ()
  (:documentation
   "A view that adds translucency and vibrancy effects to the views in your interface.
see https://developer.apple.com/documentation/appkit/nsvisualeffectview?language=objc"))

(define-objc-enum ns-box-type
  "These constants and data type identifies box types, which, in
conjunction with a box’s border type, define the appearance of the
box.

see https://developer.apple.com/documentation/appkit/nsbox/boxtype-swift.enum?language=objc"
  (:primary   0 "Specifies the primary box appearance. "
               "This is the default box type.")
  (:separator 2 "Specifies that the box is a separator.")
  (:custom    4 "Specifies that the appearance of the box "
              "is determined entirely by the by box-configuration"
              "methods, without automatically applying Apple human"
              "interface guidelines. See Customizing for details."))

(define-objc-enum ns-title-position
  "Specify the location of a box’s title with respect to its border.
see https://developer.apple.com/documentation/appkit/nsbox/titleposition-swift.enum?language=objc"
  (:no-title     0 "The box has no title.")
  (:above-top    1 "Title positioned above the box’s top border.")
  (:at-top       2 "Title positioned within the box’s top border.")
  (:below-top    3 "Title positioned below the box’s top border.")
  (:above-bottom 4 "Title positioned above the box’s bottom border.")
  (:at-bottom    5 "Title positioned within the box’s bottom border.")
  (:below-bottom 6 "Title positioned below the box’s bottom border."))

(define-objc-class "NSBox" ()
  (("borderRect"
    :reader border-rect
    :documentation
    "The rectangle in which the receiver’s border is drawn.
see https://developer.apple.com/documentation/appkit/nsbox/borderrect?language=objc")
   ("boxType"
    :accessor box-type
    :before   as-ns-box-type
    :after    decode-ns-box-type
    :documentation
    "The receiver’s box type.

A constant describing the type of box.
These constants are described in `ns-box-type'.
By default, the box type of an NSBox is `:primary'.

see https://developer.apple.com/documentation/appkit/nsbox/boxtype-swift.property?language=objc")
   ("transparent"
    :accessor transparentp
    :before   as-boolean
    :documentation
    "Whether the receiver is transparent.
see https://developer.apple.com/documentation/appkit/nsbox/istransparent?language=objc")
   ("title"
    :accessor title
    :before   as-ns-string
    :after    ns-string-to-string
    :documentation
    "The receiver’s title.

The title of the NSBox.
By default, a box’s title is “Title.”
If the size of the new title is different from that of the old title,
the content view is resized to absorb the difference.

see https://developer.apple.com/documentation/appkit/nsbox/title?language=objc")
   ("titleFont"
    :accessor title-font
    :before   as-ns-font
    :documentation
    "The font object used to draw the receiver’s title.
see https://developer.apple.com/documentation/appkit/nsbox?language=objc")
   ("titlePosition"
    :accessor title-position
    :before   as-ns-title-position
    :after    decode-ns-title-position
    :documentation
    "A constant representing the title position.

A constant representing the position of the receiver’s title.
See `ns-title-position' for possible values.
If the new title position changes the size of the box’s border area,
the content view is resized to absorb the difference, and the box is
marked as needing redisplay.

see https://developer.apple.com/documentation/appkit/nsbox/titleposition-swift.property?language=objc")
   ("borderColor"
    :accessor border-color
    :before   as-ns-color
    :documentation
    "The color of the receiver’s border when the receiver is a custom
box with a simple line border.

The receiver’s border color.
It must be a custom box—that is, it has a type of NSBoxCustom - and it
must have a border style of NSLineBorder.

Special Considerations
Functional only when the receiver’s box type (boxType) is NSBoxCustom
and its border type (borderType) is NSLineBorder.

see https://developer.apple.com/documentation/appkit/nsbox/bordercolor?language=objc")
   ("borderWidth"
    :accessor border-width
    :documentation
    "The width of the receiver’s border when the receiver is a custom
box with a simple line border.
see https://developer.apple.com/documentation/appkit/nsbox/borderwidth?language=objc")
   ("fillColor"
    :accessor fill-color
    :before   as-ns-color
    :documentation
    "The color of the receiver’s background when the receiver is a
custom box with a simple line border.
see https://developer.apple.com/documentation/appkit/nsbox/fillcolor?language=objc"))
  (:documentation
   "A stylized rectangular box with an optional title.

Use box objects to visually group the contents of your window. For
example, you might use boxes to group related views. Use an NSBox
object to configure the appearance of the box.

Subclassing Notes
An NSBox object is a view that draws a line around its rectangular
bounds and that displays a title on or near the line (or might display
neither line nor title). You can adjust the style of the line (bezel,
grooved, or plain) as well as the placement and font of the title. An
NSBox also has a content view to which other views can be added; it
thus offers a way for an application to group related views. You could
create a custom subclass of NSBox that alters or augments its
appearance or that modifies its grouping behavior. For example, you
might add color to the lines or background, add a new line style, or
have the views in the group automatically snap to an invisible grid
when added.

Methods to Override
You must override the drawRect: method (inherited from NSView) if you
want to customize the appearance of your NSBox objects. Depending on
the visual effect you’re trying to achieve, you may have to invoke
super‘s implementation first. For example, if you are compositing a
small image in a corner of the box, you would invoke the superclass
implementation first. If you’re adding a new style of line, you would
provide a way to store a request for this line type (such as a boolean
instance variable and related accessor methods). Then, in drawRect:,
if a request for this line type exists, you would draw the entire view
yourself (that is, without calling super). Otherwise, you would invoke
the superclass implementation.

If you wish to change grouping behavior or other behavioral
characteristics of the NSBox class, consider overriding contentView,
sizeToFit, or addSubview: (inherited from NSView).

Special Considerations
If you are drawing the custom NSBox entirely by yourself, and you want
it to look exactly like the superclass object (except for your
changes), it may take some effort and time to get the details right.

see https://developer.apple.com/documentation/appkit/nsbox?language=objc"))

(defmethod corner-radius ((box ns-box))
  "The radius of the receiver’s corners when the receiver is a
custom box with a simple line border.
see https://developer.apple.com/documentation/appkit/nsbox/cornerradius?language=objc"
  (invoke box "cornerRadius"))

(defmethod (setf corner-radius) (radius (box ns-box))
  (invoke box "setCornerRadius:" radius))

(defmethod init ((box ns-box)
                 &key
                   frame
                   (box-type       :primary  box-type?)
                   (transparentp   nil       transparent?)
                   (title          ""        title?)
                   (title-font     nil       title-font?)
                   (title-position :no-title title-position?)
                   (border-color   nil       border-color?)
                   border-width
                   corner-radius
                   (fill-color     nil       fill-color?)
                 &allow-other-keys)
  "Initialize `ns-box' BOX.

Parameters:
+ FRAME:
  size and position of the box
+ BOX-TYPE (`ns-box-type')
  The BOX's box type.
+ TRANSPARENTP
  Whether BOX is transparent.
+ TITLE
  The title of BOX
+ TITLE-FONT
  The font of TITLE
+ TITLE-POSITION (`ns-title-position')
  The position of TITLE
+ BORDER-COLOR
  The color of the receiver’s border when the receiver is a custom
  box with a simple line border.
+ BORDER-WIDTH
  The width of the receiver’s border when the receiver is a custom
  box with a simple line border.
+ CORNER-RADIUS
  The radius of the receiver’s corners when the receiver is a
  custom box with a simple line border.

Note: when setting BORDER-COLOR, BORDER-WIDTH, CORNER-RADIUS,
the BOX-TYPE would be updated as :custom if necessary. "
  (declare (type ns-rect*          frame)
           (type ns-box-type       box-type)
           (type ns-title-position title-position))
  (invoke box "initWithFrame:" frame)
  (cond
    ;; if specifies BOX-TYPE directly, use it
    (box-type? (setf (box-type box) box-type))
    ;; if customing border, force as `:custom'
    ((or border-color?
         border-width
         corner-radius)
     (setf (box-type box) :custom)))
  (when transparent? (setf (transparentp box) transparentp))
  (cond (title?
         (setf (title box) title)
         (when title-font? (setf (title-font box) title-font)))
        (t
         (setf title-position? t
               title-position  :no-title)))
  (when title-position? (setf (title-position box) title-position))
  (when border-color?   (setf (border-color   box) border-color))
  (when border-width    (setf (border-width   box) border-width))
  (when corner-radius   (setf (corner-radius  box) corner-radius))
  (when fill-color?     (setf (fill-color     box) fill-color)))

;;; UI validation

;;; Tool tips

;;; Related types



;;;; View Management
;; Manage your user interface, including the size and position of views in a window.
;; see https://developer.apple.com/documentation/appkit/view-management?language=objc

;;; Content Controllers

(define-objc-class "NSWindowController" ()
  ()
  (:documentation
   "A controller that manages a window, usually a window stored in a nib file.
see https://developer.apple.com/documentation/appkit/nswindowcontroller?language=objc"))

(define-objc-class "NSViewController" ()
  ()
  (:documentation
   "A controller that manages a view, typically loaded from a nib file.
see https://developer.apple.com/documentation/appkit/nsviewcontroller?language=objc"))

(define-objc-class "NSTitlebarAccessoryViewController" ()
  ()
  (:documentation
   "An object that manages a custom view—known as an accessory view-in
the title bar–toolbar area of a window.
see https://developer.apple.com/documentation/appkit/nstitlebaraccessoryviewcontroller?language=objc"))

;;; Split View Interface

(define-objc-class "NSSplitViewController" ()
  ()
  (:documentation
   "An object that manages an array of adjacent child views, and has a split view object for managing dividers between those views.
see https://developer.apple.com/documentation/appkit/nssplitviewcontroller?language=objc"))

(define-objc-class "NSSplitView" ()
  ()
  (:documentation
   "A view that arranges two or more views in a linear stack running horizontally or vertically.
see https://developer.apple.com/documentation/appkit/nssplitview?language=objc"))

(define-objc-class "NSSplitViewItem" ()
  ()
  (:documentation
   "An item in a split view controller.
see https://developer.apple.com/documentation/appkit/nssplitviewitem?language=objc"))

;;; Stack View Interface

(define-objc-class "NSStackView" ()
  ()
  (:documentation
   "A view that arranges an array of views horizontally or vertically
and updates their placement and sizing when the window size changes.
see https://developer.apple.com/documentation/appkit/nsstackview?language=objc"))

;;; Tab View Interface

(define-objc-class "NSTabViewController" ()
  ()
  (:documentation
   "A container view controller that manages a tab view interface,
which organizes multiple pages of content but displays only one page at a time.
see https://developer.apple.com/documentation/appkit/nstabviewcontroller?language=objc"))

(define-objc-class "NSTabView" ()
  ()
  (:documentation
   "A multipage interface that displays one page at a time.
see https://developer.apple.com/documentation/appkit/nstabview?language=objc"))

(define-objc-class "NSTabViewItem" ()
  ()
  (:documentation
   "An item in a tab view.
see https://developer.apple.com/documentation/appkit/nstabviewitem?language=objc"))

;;; Paged Interface

(define-objc-class "NSPageController" ()
  ()
  (:documentation
   "An object that controls swipe navigation and animations between views or view content.
see https://developer.apple.com/documentation/appkit/nspagecontroller?language=objc"))

;;; Media Library Interface

(define-objc-class "NSMediaLibraryBrowserController" ()
  ()
  (:documentation
   "An object that configures and displays a Media Library Browser panel.
see https://developer.apple.com/documentation/appkit/nsmedialibrarybrowsercontroller?language=objc"))


;;;; View Layout
;; Position and size views using a stack view or Auto Layout constraints.
;; see https://developer.apple.com/documentation/appkit/view-layout?language=objc

;;; Stack View

(define-objc-class "NSStackView" ()
  ()
  (:documentation
   "A view that arranges an array of views horizontally or vertically
and updates their placement and sizing when the window size changes.
see https://developer.apple.com/documentation/appkit/nsstackview?language=objc"))

;;; Auto Layout Constraints

(define-objc-class "NSLayoutConstraint" ()
  ()
  (:documentation
   "The relationship between two user interface objects that must be satisfied by
the constraint-based layout system.
see https://developer.apple.com/documentation/appkit/nslayoutconstraint?language=objc"))

;;; Layout Guides

(define-objc-class "NSLayoutGuide" ()
  ()
  (:documentation
   "A rectangular area that can interact with Auto Layout.
see https://developer.apple.com/documentation/appkit/nslayoutguide?language=objc"))

(define-objc-class "NSLayoutDimension" ()
  ()
  (:documentation
   "A factory class for creating size-based layout constraint objects using a fluent API.
see https://developer.apple.com/documentation/appkit/nslayoutdimension?language=objc"))

;;; Anchors

(define-objc-class "NSLayoutAnchor" ()
  ()
  (:documentation
   "A factory class for creating layout constraint objects using a fluent API.
see https://developer.apple.com/documentation/appkit/nslayoutanchor?language=objc"))

(define-objc-class "NSLayoutXAxisAnchor" ()
  ()
  (:documentation
   "A factory class for creating horizontal layout constraint objects using a fluent API.
see https://developer.apple.com/documentation/appkit/nslayoutxaxisanchor?language=objc"))

(define-objc-class "NSLayoutYAxisAnchor" ()
  ()
  (:documentation
   "A factory class for creating vertical layout constraint objects using a fluent API.
see https://developer.apple.com/documentation/appkit/nslayoutyaxisanchor?language=objc"))

;;; View Compression


;;;; Appearance Customization
;; Add Dark Mode support to your app, and use appearance proxies to modify your UI.
;; see https://developer.apple.com/documentation/appkit/appearance-customization?language=objc

;;; Dark Mode

;;; Appearance System

(define-objc-class "NSAppearance" ()
  ()
  (:documentation
   "An object that manages standard appearance attributes for UI elements in an app.
see https://developer.apple.com/documentation/appkit/nsappearance?language=objc"))


;;;; Animation
;; Animate your views and other content to create a more engaging experience for users.
;; see https://developer.apple.com/documentation/appkit/animation?language=objc

;;; View-Based Animations

(define-objc-class "NSViewAnimation" ()
  ()
  (:documentation
   "An animation of an app’s views, limited to changes in frame location and size,
and to fade-in and fade-out effects.
see https://developer.apple.com/documentation/appkit/nsviewanimation?language=objc"))

(define-objc-class "NSAnimationContext" ()
  ()
  (:documentation
   "An animation context, which contains information about environment and state.
see https://developer.apple.com/documentation/appkit/nsanimationcontext?language=objc"))

;;; Presentations

;;; Custom Animations

(define-objc-class "NSAnimation" ()
  ()
  (:documentation
   "An object that manages the timing and progress of animations in the user interface.
see https://developer.apple.com/documentation/appkit/nsanimation?language=objc"))

;;; System Animations


;;;; Windows, Panels, and Screens
;; Organize your view hierarchies and facilitate their display onscreen.
;; see https://developer.apple.com/documentation/appkit/windows-panels-and-screens?language=objc

;;; Windows

(define-objc-mask ns-window-style-mask
  "Constants that specify the style of a window,
and that you can combine with the C bitwise OR operator.

See https://developer.apple.com/documentation/appkit/nswindow/stylemask-swift.struct?language=objc"
  (:borderless                0    "The window displays none of the usual peripheral elements. ")
  (:titled                    1    "The window displays a title bar. ")
  (:closable                  2    "The window displays a close button. ")
  (:miniaturizable            4    "The window displays a minimize button. ")
  (:resizable                 8    "The window can be resized by the user. ")
  (:textured-background       256
                                   "Deprecated"
                                   "The window uses a textured background that darkens"
                                   "when the window is key or main and lightens when it is inactive,"
                                   "and may have a second gradient in the section below the window content.")
  (:unified-title-and-toolbar 4096
                                   "This constant has no effect,"
                                   "because all windows that include a toolbar use the unified style. ")
  (:full-screen               16384
                                   "The window can appear full screen. "
                                   "A fullscreen window does not draw its title bar, "
                                   "and may have special handling for its toolbar. "
                                   "(This mask is automatically toggled when toggleFullScreen: is called.)")
  (:full-size-content-view    32768
                                   "When set, the window's contentView consumes the full size of the window."
                                   "Although you can combine this constant with other window style masks, "
                                   "it is respected only for windows with a title bar. "
                                   "Note that using this mask opts in to layer-backing. "
                                   "Use the contentLayoutRect or the contentLayoutGuide to lay out views "
                                   "underneath the title bar–toolbar area.")
  (:utility-window            16   "The window is a panel or a subclass of `ns-panel'.")
  (:doc-modal-window          64   "The window is a document-modal panel (or a subclass of `ns-panel').")
  (:nonactivating-panel       128
                                   "The window is a panel or a subclass of NSPanel"
                                   "that does not activate the owning app.")
  (:hud-window                8192 "The window is a HUD panel. "))

(define-objc-mask ns-window-collection-behavior
  "Window collection behaviors related to Mission Control, Spaces, and
Stage Manager.

Collection behaviors are properties you set on windows to control
their display characteristics in window management technologies. Use
them to specify a preference on how windows behave in window
management technologies like Mission Control, Spaces, and Stage
Manager.

To set a collection behavior on a window, assign one or more behavior
options to the window’s `collection-behavior' property.

Not all collection behaviors apply to all windowing management
technologies, and some are mutually exclusive to their respective
groups. For example, `:primary', `:auxiliary',
and `:can-join-all-applications' only apply to full screen and
Stage Manager. They’re also mutually exclusive. Specify at
most one per window."
  "Stage Manager and full screen"
  (:primary                      65536
                                 "The behavior marking this window as primary "
                                 "for both Stage Manager and full screen.")
  (:auxiliary                    131072
                                 "The behavior marking this window as auxiliary"
                                 "for both Stage Manager and full screen.")
  (:can-join-all-applications    262144
                                 "The behavior marking this window as one that "
                                 "can join all apps for both Stage Manager and "
                                 "full screen.")
  "Spaces"
  (:default                      0
                                 "The window appears in only one space at a time.")
  (:can-join-all-spaces          1
                                 "The window can appear in all spaces.")
  (:move-to-active-space         2
                                 "When the window becomes active, move it to the "
                                 "active space instead of switching spaces.")
  "Mission Control"
  (:stationary                   16
                                 "Mission Control doesn’t affect the window, so it"
                                 "stays visible and stationary, like the desktop "
                                 "window.")
  "Spaces and Mission Control"
  (:managed                      4
                                 "The window participates in Mission Control and "
                                 "Spaces.")
  (:transient                    8
                                 "The window floats in Spaces and hides in "
                                 "Mission Control.")
  "Full screen"
  (:full-screen-primary          128
                                 "The window can enter full-screen mode.")
  (:full-screen-auxiliary        256
                                 "The window displays on the same space as the "
                                 "full screen window.")
  (:full-screen-none             512
                                 "The window doesn’t support full-screen mode.")
  (:full-screen-allows-tiling    2048
                                 "The window can be a secondary full screen tile"
                                 "even if it can’t be a full screen window itself.")
  (:full-screen-disallows-tiling 4096
                                 "The window doesn’t support being a full-screen "
                                 "tile window, but may support being a full-screen"
                                 "window.")
  "Window cycling"
  (:participates-in-cycle        32
                                 "The window participates in the window cycle for "
                                 "use with the Cycle Through Windows menu item.")
  (:ignores-cycle                64
                                 "The window isn’t part of the window cycle for "
                                 "use with the Cycle Through Windows menu item."))

(define-objc-class "NSWindow" ()
  (;; Managing the Window's Behavior
   ("delegate"
    :accessor delegate
    :documentation
    "The window’s delegate. (default as `:self')

The value of this property is nil if the window doesn’t have a
delegate.

A window object’s delegate is inserted in the responder chain after
the window itself and is informed of various actions by the window
through delegation messages.

see https://developer.apple.com/documentation/appkit/nswindow/delegate?language=objc")
   ;; Configuring the Window's Content
   ("contentViewController"
    :reader content-view-controller
    :documentation
    "Get the main content view controller for the window.

The value of this property provides the content view of the
window. Setting this value removes the existing value of `content-view'
and makes the `content-view-controller' .view the main content view for the
window. By default, the value of this property is `nil'.

The content view controller controls only the `content-view' object, and
not the title of the window. The window title can easily be bound to
the `content-view-controller' object using code such as:

    [window bind:NSTitleBinding
        toObject:contentViewController
     withKeyPath:@\"title\"
         options:nil]

Setting `content-view-controller' causes the window to resize based on
the current size of the `content-view-controller'; to restrict the
size of the window, use Auto Layout (note that the value of this
property is encoded in the NIB). Directly assigning a `content-view'
value clears out the root view controller.

see https://developer.apple.com/documentation/appkit/nswindow/contentviewcontroller?language=objc")
   ("contentView"
    :reader content-view
    :documentation
    "Get the window’s content view,
the highest accessible view object in the window’s view hierarchy.

The window retains the new content view and owns it thereafter. The
view object is resized to fit precisely within the content area of the
window. You can modify the content view’s coordinate system through
its bounds rectangle, but you can’t alter its frame rectangle
(its size or location) directly.

Setting this property releases the old content view. If you plan to
reuse it, be sure to retain it before changing the property value and
to release it as appropriate when adding it to another `ns-window' or
`ns-view' object.

see https://developer.apple.com/documentation/appkit/nswindow/contentview?language=objc")
   ;; Configuring the Window's Appearance
   ("styleMask"
    :accessor style-mask
    :after    decode-ns-window-style-mask
    :before   as-ns-window-style-mask
    :documentation
    "Flags that describe the window’s current style,
such as if it’s resizable or in full-screen mode.

The styleMask is settable on macOS 10.6 and later. Setting this
property has the same restrictions as the styleMask parameter of
initWithContentRect:styleMask:backing:defer:. Changing the style mask
may cause the view hierarchy to be rebuilt.

Dev Note:
see `ns-window-style-mask'.

see https://developer.apple.com/documentation/appkit/nswindow/stylemask-swift.property?language=objc")
   ("alphaValue"
    :accessor alpha-value
    :documentation
    "The window's alpha value.
see https://developer.apple.com/documentation/appkit/nswindow/alphavalue?language=objc")
   ("backgroundColor"
    :accessor background-color
    :before   as-ns-color
    :documentation
    "The color of window's background.
see https://developer.apple.com/documentation/appkit/nswindow/backgroundcolor?language=objc")
   ("colorSpace"
    :accessor color-space
    :documentation
    "The window’s color space.

The value of this property is nil if the window does not have a
backing store, and is off-screen.

see https://developer.apple.com/documentation/appkit/nswindow/colorspace?language=objc")
   ("canHide"
    :accessor can-hide-p
    :before   as-boolean
    :documentation
    "A Boolean value that indicates whether the window can hide
when its application becomes hidden.

The value of this property is true if the window can hide when its
application becomes hidden (during execution of the `ns-application'
`ns-application' `hide' method); otherwise, `nil'.
By default, the value of the property is `t'.

see https://developer.apple.com/documentation/appkit/nswindow/canhide?language=objc")
   ("onActiveSpace"
    :accessor on-active-space-p
    :before   as-boolean
    :documentation
    "Whether the window is on the currently active space.

The value of this property is true if the window is on the currently
active space; otherwise, false. For visible windows, this property
indicates whether the window is currently visible on the active
space. For nonvisible windows, it indicates whether ordering the
window onscreen would cause it to be on the active space.

see https://developer.apple.com/documentation/appkit/nswindow/isonactivespace?language=objc")
   ("hidesOnDeactivate"
    :reader hides-on-deactivate-p
    :before as-boolean
    :documentation
    "Whether the window is removed from the screen
when its application becomes inactive.

The value of this property is true if the window is removed from the
screen when its application is deactivated; false if it remains
onscreen. The default value for NSWindow is false; the default value
for NSPanel is true.

see https://developer.apple.com/documentation/appkit/nswindow/hidesondeactivate?language=objc")
   ("collectionBehavior"
    :accessor collection-behavior
    :before   decode-ns-window-collection-behavior
    :documentation
    "A value that identifies the window’s behavior in window collections.

The possible values for this property are listed in `ns-window-collection-behavior'.

see https://developer.apple.com/documentation/appkit/nswindow/collectionbehavior-swift.property?language=objc")
   ("opaque"
    :accessor opaquep
    :before   as-boolean
    :documentation
    "Whether the window is opaque.
see https://developer.apple.com/documentation/appkit/nswindow/isopaque?language=objc")
   ("hasShadow"
    :accessor has-shadow-p
    :before   as-boolean
    :documentation
    "Whether the window has a shadow.
see https://developer.apple.com/documentation/appkit/nswindow/hasshadow?language=objc")
   ;; Accessing Window Information
   ;; Getting Layout Information
   ;; Managing Windows
   ;; Managing Sheets
   ;; Sizing Windows
   ;; Sizing Content
   ;; Managing Window Layers
   ;; Managing Window Visibility and Occlusion State
   ("visible"
    :accessor visiblep
    :before   as-boolean
    :setter   "setIsVisible:"
    :documentation
    "If window is visible on screen.

Dev Note:
Using (setf visible-p) would invoke setIsVisible: method.

see https://developer.apple.com/documentation/appkit/nswindow/isvisible?language=objc")
   ;; Managing Window Frames in User Defaults
   ;; Managing Key Status
   ;; Managing Main Status
   ;; Managing Toolbars
   ;; Managing Attached Windows
   ;; Managing Default Buttons
   ;; Managing Field Editors
   ;; Managing the Window Menu
   ;; Managing Cursor Rectangles
   ;; Managing Title Bars
   ;; Managing Title Bar Accessories
   ;; Managing Window Tabs
   ;; Managing Tooltips
   ;; Handling Events
   ;; Managing Responders
   ;; Managing the Key View Loop
   ;; Managing Window Sharing
   ;; Handling Window Restoration
   ;; Drawing Windows
   ;; Window Animation
   ;; Updating Windows
   ;; Dragging Items
   ;; Accessing Edited Status
   ;; Converting Coordinates
   ;; Managing Titles
   ("title"
    :accessor title
    :before   as-ns-string
    :after    ns-string-to-string
    :documentation
    "The string that appears in the title bar of the window or the
path to the represented file.

If the title has been set using setTitleWithRepresentedFilename:, this
property contains the file’s path. Setting this property also sets the
title of the window’s miniaturized window.

see https://developer.apple.com/documentation/appkit/nswindow/title?language=objc")
   ;; Accessing Screen Information
   ;; Moving Windows
   ;; Closing Windows
   ;; Minimizing Windows
   ;; Getting the Dock Tile
   ;; Printing Windows
   ;; Providing Services
   ;; Triggering Constraint-Based Layout
   ;; Debugging Constraint-Based Layout
   ;; Constraint-Based Layouts
   ;; Working with Window Depths
   ;; Getting Information About Scripting Attributes
   ;; Setting Scripting Attributes
   ;; Handling Script Commands
   )
  (:documentation
   "A window that an app displays on the screen.

A single `ns-window' object corresponds to, at most, one on-screen
window.  Windows perform two principal functions:

+ To place views in a provided area
+ To accept and distribute mouse and keyboard events the user
  generates to the appropriate views

Notes:
Although the `ns-window' class inherits the `ns-coding' protocol from
`ns-responder', the class doesn't support coding. Legacy support for
archivers exists, but its use is deprecated and may not work. Any
attempt to archive or unarchive a window object using a keyed coding
object raises an `ns-invalid-argument-exception' exception.  For
details about window restoration, see restorationClass.
see https://developer.apple.com/documentation/appkit/nswindow?language=objc"))

(defmethod (setf delegate) ((value (eql :self)) object)
  (setf (delegate object) object))

;; Creating a window

(define-objc-enum ns-backing-store-type
  (:retained    0 "Deprecated"
                "The window uses a buffer"
                "but draws directly to the screen where possible and "
                "to the buffer for obscured portions.")
  (:nonretained 1 "Deprecated"
                "The window draws directly to the screen without using any buffer.")
  (:buffered    2
                "The window renders all drawing into a display buffer"
                "and then flushes it to the screen."))

(declaim (type (satisfies ns-window-style-mask-p) *ns-window-style*))
(defparameter *ns-window-style* '(:titled :closable :miniaturizable :resizable)
  "Default `ns-window-style-mask'.
see `ns-window-init'. ")

(declaim (type (and keyword (satisfies ns-backing-store-type-p)) *ns-backing-store*))
(defparameter *ns-backing-store* :buffered
  "Default `ns-backing-store-type'.
see `ns-window-init'. ")

(defmethod (setf title) ((path pathname) (window ns-window))
  "Sets a given path as the window’s title, formatting it as a
file-system path, and records this path as the window’s associated
file.

Parameters:
+ PATH: The file path to set as the window’s title.

The windows’ title bar displays the filename, not the file’s path.

see https://developer.apple.com/documentation/appkit/nswindow/settitlewithrepresentedfilename(_:)?language=objc"
  (invoke window
          "setTitleWithRepresentedFilename:"
          (string-to-ns-string (uiop:native-namestring path))))

(defmethod init ((window ns-window)
                 &key frame
                   (style       *ns-window-style*)
                   (backing     *ns-backing-store*)
                   (defer        t)
                   (visiblep     t)
                   (has-shadow-p t)
                   (opaquep      nil)
                   (delegate     :self)
                   title
                   screen
                 &allow-other-keys)
  "Initializes the window with the specified values.
Return the initialized `ns-window'.

Parameters:
+ FRAME: `ns-rect'
  Origin and size of the window’s content area in screen
  coordinates. Note that the window server limits window position
  coordinates to ±16,000 and sizes to 10,000.
+ STYLE: see `ns-window-style-mask' (default `*ns-window-style*')
  The window’s style. It can be NSBorderlessWindowMask, or it can
  contain any of the options described in NSWindowStyleMask, combined
  using the C bitwise OR operator. Borderless windows display none of
  the usual peripheral elements and are generally useful only for
  display or caching purposes; you should normally not need to create
  them. Also, note that a window’s style mask should include
  NSTitledWindowMask if it includes any of the others.
+ BACKING: see `ns-backing-store-type' (default `*ns-backing-store*')
  Specifies how the drawing done in the window is buffered by the
  window device, and possible values are described in
  NSBackingStoreType.
+ DEFER:
  Specifies whether the window server creates a window device for the
  window immediately. When true, the window server defers creating the
  window device until the window is moved onscreen. All display
  messages sent to the window or its views are postponed until the
  window is created, just before it’s moved onscreen.
+ SCREEN: `ns-screen'
  Specifies the screen on which the window is positioned. The content
  rectangle is positioned relative to the bottom-left corner of
  screen. When nil, the content rectangle is positioned relative to
  (0, 0), which is the origin of the primary screen.

Styls Modification:
+ VISIBLE:
  if window is visible on screen (default `t')
+ HAS-SHADOW:
  if window has shadow (default `t')
+ OPAQUE:
  if window is opaque (default `nil')
+ TITLE:
  the title of window (default not set)

Dev Note:
this invokes
+ initWithContentRect:styleMask:backing:defer:screen:
+ initWithContentRect:styleMask:backing:defer:

see https://developer.apple.com/documentation/appkit/nswindow/init(contentrect:stylemask:backing:defer:)?language=objc
see https://developer.apple.com/documentation/appkit/nswindow/init(contentrect:stylemask:backing:defer:screen:)?language=objc"
  (declare (type ns-rect* frame)
           (type (satisfies ns-window-style-mask-p)  style)
           (type (satisfies ns-backing-store-type-p) backing)
           (type (or null ns-screen)                 screen))
  (if screen
      (invoke window
              "initWithContentRect:styleMask:backing:defer:screen:"
              frame
              (as-ns-window-style-mask  style)
              (as-ns-backing-store-type backing)
              (as-boolean defer)
              screen)
      (invoke window
              "initWithContentRect:styleMask:backing:defer:"
              frame
              (as-ns-window-style-mask  style)
              (as-ns-backing-store-type backing)
              (as-boolean defer)))
  (setf (visiblep     window) visiblep
        (has-shadow-p window) has-shadow-p
        (opaquep      window) opaquep
        (delegate     window) delegate)
  (when title (setf (title window) title)))

;; Managing the Window's Behavior

;; Configuring the Window's Content

(defmethod add-subview ((window ns-window) (subview ns-view) &rest keys &key)
  "This is equal to calling (add-subview (content-view WINDOW) SUBVIEW). "
  (apply #'add-subview (content-view window) subview keys))

;; Configuring the Window's Appearance

;; Accessing Window Information

;; Getting Layout Information

;; Managing Windows

;; Managing Sheets

;; Sizing Windows

;; Sizing Content

;; Managing Window Layers

;; Managing Window Visibility and Occlusion State

;; Managing Window Frames in User Defaults

;; Managing Key Status

;; Managing Main Status

(defmethod make-main-window ((window ns-window))
  "Makes the window the main window.
see https://developer.apple.com/documentation/appkit/nswindow/makemain()?language=objc"
  (invoke window "makeMainWindow"))

;; Managing Toolbars

;; Managing Attached Windows

;; Managing Default Buttons

;; Managing Field Editors

;; Managing the Window Menu

;; Managing Cursor Rectangles

;; Managing Title Bars

;; Managing Title Bar Accessories

;; Managing Window Tabs

;; Managing Tooltips

;; Handling Events

;; Managing Responders

;; Managing the Key View Loop

;; Managing Window Sharing

;; Handling Mouse Events

;; Handling Window Restoration

;; Drawing Windows

;; Window Animation

;; Updating Windows

;; Dragging Items

;; Accessing Edited Status

;; Converting Coordinates

;; Managing Titles

;; Accessing Screen Information

;; Moving Windows

;; Closing Windows

;; Minimizing Windows

;; Getting the Dock Tile

;; Printing Windows

;; Providing Services

;; Triggering Constraint-Based Layout

;; Debugging Constraint-Based Layout

;; Constraint-Based Layouts

;; Working with Window Depths

;; Getting Information About Scripting Attributes

;; Setting Scripting Attributes

;; Handling Script Commands

;; Constants

;;; NSWindowDelegate (protocoal)
;; A set of optional methods that a window’s delegate can implement to
;; respond to events, such as window resizing, moving, exposing, and
;; minimizing.
;; https://developer.apple.com/documentation/appkit/nswindowdelegate?language=objc

;; Managing Sheets

;; Sizing Windows

(define-objc-method ("NSWindow" "windowDidResize:" window-did-resize) :void
    ((sender :object))
  (:documentation
   "Tells the delegate that the window has been resized.
see https://developer.apple.com/documentation/appkit/nswindowdelegate/windowdidresize(_:)?language=objc"))

;; Minmizing Windows

(define-objc-method ("NSWindow" "windowDidMiniaturize:" window-did-miniaturize) :void
    ((sender :object))
  (:documentation
   "Tells the delegate that the window has been minimized.
see https://developer.apple.com/documentation/appkit/nswindowdelegate/windowdidminiaturize(_:)?language=objc"))

(define-objc-method ("NSWindow" "windowDidDeminiaturize:" window-did-deminiaturize) :void
    ((sender :object))
  (:documentation
   "Tells the delegate that the window has been deminimized.
see https://developer.apple.com/documentation/appkit/nswindowdelegate/windowdiddeminiaturize(_:)?language=objc"))

;; Zooming Window

;; Managing Full-Screen Presentation

;; Custom Full-Screen Presentation Animations

;; Moving Windows

(define-objc-method ("NSWindow" "windowDidMove:" window-did-move) :void
    ((sender :object))
  (:documentation
   "Tells the delegate that the window has moved.
see https://developer.apple.com/documentation/appkit/nswindowdelegate/windowdidmove(_:)?language=objc"))

(define-objc-method ("NSWindow" "windowDidChangeScreen:" window-did-change-screen) :void
    ((sender :object))
  (:documentation
   "Tells the delegate that the window has changed screens.
see https://developer.apple.com/documentation/appkit/nswindowdelegate/windowdidchangescreen(_:)?language=objc"))

;; Closing Windows

(define-objc-method ("NSWindow" "windowShouldClose:" window-should-close) :bool
    ((sender :object))
  (:documentation
   "Tells the delegate that the user has attempted to close a window or
the window has received a performClose: message.

Parameters:
+ WINDOW:
+ SENDER: the window being closed

This method may not always be called during window closing.
Specifically, this method is not called when a user quits an
application.

Dev Note:
Return `t' to allow sender to be closed; otherwise `nil'.
By default this function returns `t'. ")
  (:default t))

(define-objc-method ("NSWindow" "windowWillClose:" window-will-close) :void
    ((notification :object))
  (:documentation
   "Tells the delegate that the window is about to close.

Parameters:
+ WINDOW: WINDOW to be closed
+ NOTIFICATION: A notification named NSWindowWillCloseNotification.

You can retrieve the NSWindow object in question by sending object to
notification.

Dev Note:
this is called when window will close, do nothing by default"))

;; Managing Key Status

;; Managing Main Status

;; Managing Field Editors

;; Updating Windows

;; Exposing Windows

;; Managing Occlusion State

;; Dragging Windows

;; Getting the Undo Manager

;; Managing Titles

;; Managing Restorable State

;; Managing Presentation in Version Browsers

;; Instance Methods

;; Notifications

(define-objc-class "NSPanel" ()
  ()
  (:documentation
   "A special kind of window that typically performs a function that is auxiliary to the main window.
For details about how panels work,
especially to find out how their behavior differs from window behavior, see How Panels Work:
https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/WinPanel/Concepts/UsingPanels.html#//apple_ref/doc/uid/20000224
see https://developer.apple.com/documentation/appkit/nspanel?language=objc"))

;; Configuring Panels

;; Constants

(define-objc-class "NSWindowTab" ()
  ()
  (:documentation
   "A tab associated with a window that is part of a tabbing group.
see https://developer.apple.com/documentation/appkit/nswindowtab?language=objc"))

;; Customizing the Title

;; Customizing the Tooltip

;; Adding an Accessory View

;; Relationships

(define-objc-class "NSWindowTabGroup" ()
  ()
  (:documentation
   "A group of windows that display together as a single tabbed window.
see https://developer.apple.com/documentation/appkit/nswindowtabgroup?language=objc"))

;; Checking the Group Identifier

;; Configuring the Tab User Interface

;; Managing Tabbed Windows

;;; Window Restoration

;;; Screens

(define-objc-class "NSScreen" ()
  (("frame"
    :reader frame
    :documentation
    "The dimensions and location of the screen.

This is the full screen rectangle at the current resolution. This
rectangle includes any space currently occupied by the menu bar and
dock.

see https://developer.apple.com/documentation/appkit/nsscreen/frame?language=objc"))
  (:documentation
   "An object that describes the attributes of a computer’s monitor or screen.

An app may use an NSScreen object to retrieve information about a
screen and use this information to decide what to display on that
screen. For example, an app may use the deepestScreen method to find
out which of the available screens can best represent color and then
might choose to display all of its windows on that screen.
Create the application object before you use the methods in this
class, so that the application object can make the necessary
connection to the window system. You can make sure the application
object exists by invoking the sharedApplication method of
NSApplication. If you created your app with Xcode, the application
object is automatically created for you during initialization.
Note

The NSScreen class is only for getting information about the available
displays. If you need additional information or want to change the
attributes relating to a display, you must use Quartz Services. For
more information, see Quartz Display Services.

see https://developer.apple.com/documentation/appkit/nsscreen?language=objc"))

(defun ns-main-screen ()
  "Returns the screen object containing the window with the keyboard focus. "
  (invoke 'ns-screen "mainScreen"))

(defun ns-screens ()
  "Returns a list of screen objects representing all of the screens
available on the system.

The screen at index 0 in the returned array corresponds to the primary
screen of the user’s system. This is the screen that contains the menu
bar and whose origin is at the point (0, 0). In the case of mirroring,
the first screen is the largest drawable display; if all screens are
the same size, it is the screen with the highest pixel depth. This
primary screen may not be the same as the one returned by the
mainScreen method, which returns the screen with the active window.

The array should not be cached. Screens can be added, removed, or
dynamically reconfigured at any time. When the display configuration
is changed, the default notification center sends a
NSApplicationDidChangeScreenParametersNotification notification.

see https://developer.apple.com/documentation/appkit/nsscreen/screens?language=objc"
  (ns-array-to-list (invoke 'ns-screen "screens")))

;; Getting Screen Objects

;; Getting Screen Information

;; Converting Between Screen and Backing Coordinates

;; Getting the Visible Portion of the Screen

;; Getting Extended Dynamic Range Details

;; Getting Variable Refresh Rate Details

;; Receiving Screen-Related Notifications

;; Synchronizing with the display's refresh rate

;; Instance Properties

;;; Popovers

(define-objc-class "NSPopover" ()
  ()
  (:documentation
   "A means to display additional content related to existing content on the screen.
The popover is positioned relative to the existing content and an
anchor is used to express the relation between these two units of
content. A popover has an appearance that specifies its visual
characteristics, as well as a behavior that determines which user
interactions will cause the popover to close. A transient popover is
closed in response to most user interactions, whereas a
semi-transient popover is closed when the user interacts with the
window containing the popover’s positioning view. Popovers with
application-defined behavior are not usually closed on the
developer’s behalf.

The system automatically positions each popover relative to its
positioning view and moves the popover whenever its positioning view
moves. A positioning rectangle within the positioning view can be
specified for additional granularity.

Popovers can be detached to become a separate window when they are
dragged by implementing the appropriate delegate method.
see https://developer.apple.com/documentation/appkit/nspopover?language=objc"))

;; Accessing a Popover's Content View Controller

;; Managing a Popover's Position and Size

;; Managing a Popover's Appearance

;; Closing a Popover

;; Getting and Setting the Delegate

;; Constants

;; Notifications

;; Initializers

;; Instance Properties

;; Instance Methods

;;; Alerts

(define-objc-enum ns-alert-style
  "The set of alert styles to style alerts in your app.

Currently, there’s no visual difference between informational and
warning alerts. You should only use the critical (or “caution”) alert
style if warranted. For design guidance on alert styles, see Human
Interface Guidelines > Alerts. The default alert style is
NSAlertStyleWarning.

see https://developer.apple.com/documentation/appkit/nsalert/style?language=objc"
  (:critical      2 "An alert style to inform someone about a critical event.")
  (:warning       0 "An alert style to warn someone about a current or impending event.")
  (:informational 1 "An alert style to inform someone about a current or impending event."))

(define-objc-class "NSAlert" ()
  (;; Configuring Alerts
   ("alertStyle"
    :accessor alert-style
    :before   as-ns-alert-style
    :after    decode-ns-alert-style
    :documentation
    "See `ns-alert-style' for alert style constants.
see https://developer.apple.com/documentation/appkit/nsalert/alertstyle?language=objc")
   ("showsHelp"
    :accessor shows-help-p
    :before   as-boolean
    :documentation
    "Specifies whether the alert has a help button.
see https://developer.apple.com/documentation/appkit/nsalert/showshelp?language=objc")
   ("helpAnchor"
    :accessor help-anchor
    :before   as-ns-string
    :after    ns-string-to-string
    :documentation
    "The alert’s HTML help anchor.

To provide a help anchor for the alert, set this property to the
appropriate string value. To remove the help anchor, set this
property’s value to nil.

see https://developer.apple.com/documentation/appkit/nsalert/helpanchor?language=objc")
   ("delegate"
    :accessor delegate
    :documentation
    "The alert’s delegate.")
   ("messageText"
    :accessor message
    :before   as-ns-string
    :after    ns-string-to-string
    :documentation
    "The alert’s message text or title.
see https://developer.apple.com/documentation/appkit/nsalert/messagetext?language=objc")
   ("informativeText"
    :accessor informative-text
    :before   as-ns-string
    :after    ns-string-to-string
    :documentation
    "The alert’s informative text.
see https://developer.apple.com/documentation/appkit/nsalert/informativetext?language=objc")
   ("icon"
    :accessor icon
    :before   as-ns-image
    :documentation
    "The custom icon displayed in the alert.

By default, the image used in an alert is the app icon. If you set
this property’s value, your specified custom image is used in place of
the app icon.

If you’ve set a custom alert icon, you can clear it by setting this
property’s value to nil, which restores use of the app icon for the
alert.

Note
AppKit may omit the icon from the alert if it’s the app icon and the
alert’s context is clear, such as being presented as a sheet on an app
window.

see https://developer.apple.com/documentation/appkit/nsalert/icon?language=objc"))
  (:documentation
   "A modal dialog or sheet attached to a document window.

The methods of the NSAlert class allow you to specify alert level,
alert text, button titles, and a custom icon should you require
it. The class also lets your alerts display a help button and provides
ways for apps to offer help specific to an alert.

To display an alert as a sheet, call the
beginSheetModalForWindow:completionHandler: method; to display one as
an app-modal dialog, use the runModal method.

By design, an NSAlert object is intended for a single alert—that is,
an alert with a unique combination of title, buttons, and so on—that
is displayed upon a particular condition. You should create an NSAlert
object for each alert dialog, creating it only when you need to
display an alert, and release it when you are done. If you have a
particular alert dialog that you need to show repeatedly, you can
retain and reuse an instance of NSAlert for this dialog.

After creating an alert using one of the alert creation methods, you
can customize it further prior to displaying it by customizing its
attributes. See Instance Attributes.

Unless you must maintain compatibility with existing alert-processing
code that uses the function-based API, you should allocate (alloc) and
initialize (init) the alert object, and then set its attributes using
the appropriate methods of the NSAlert class.  Instance Attributes

NSAlert objects have the following attributes:

+ Type
  An alert’s type helps convey the importance or gravity of its
  message to the user. Specified with the alertStyle property.
+ Message text
  The main message of the alert. Specified with messageText.
+ Informative text
  Additional information about the alert.
  Specified with informativeText.
+ Icon
  An optional, custom icon to display in the alert, which is
  used instead of the default app icon. Specified with icon.
+ Help
  Alerts can let the user get help about them.
  Use helpAnchor and showsHelp.
+ Response buttons
  By default an alert has one response button: the OK button.
  You can add more response buttons using the
  addButtonWithTitle: method.
+ Suppression checkbox
  A suppression checkbox allows the user to suppress the display
  of a particular alert in subsequent occurrences of the event
  that triggers it. Use showsSuppressionButton.
+ Accessory view
  An accessory view lets you add additional information to an
  alert; for example, a text field with contact information.
  Use accessoryView, layout.

Subclassing Notes
The NSAlert class is not designed for subclassing.

see https://developer.apple.com/documentation/appkit/nsalert?language=objc"))

(defmethod init ((alert ns-alert)
                 &key
                   (alert-style     :warning alert-style?)
                   (message          nil     message?)
                   (informative-text nil     info?)
                   (icon             nil     icon?)
                   help-anchor
                   (shows-help-p     (and help-anchor t))
                   buttons
                 &allow-other-keys)
  "Initialize ns-alert ALERT.

Parameters:
+ ALERT-STYLE (`ns-alert-style')
+ MESSAGE
  main message of the alert
+ INFORMATIVE-TEXT
  Additional information about the alert.
+ ICON
  An optional, custom icon to display in the alert, which is
  used instead of the default app icon.
+ SHOWS-HELP-P
  if or not shows help
+ HELP-ANCHOR
  help message
+ BUTTONS
  a list of buttons titles"
  (declare (type list           buttons)
           (type ns-alert-style alert-style))
  (call-next-method)
  (when alert-style? (setf (alert-style  alert) alert-style))
  (when message?     (setf (message      alert) (as-ns-string message)))
  (when icon?        (setf (icon         alert) icon))
  (when info?  (setf (informative-text   alert) informative-text))
  (when shows-help-p
    (setf (shows-help-p alert) shows-help-p
          (help-anchor  alert) help-anchor))
  (dolist (button buttons)
    (invoke alert "addButtonWithTitle:" (as-ns-string button))))

(defun ns-alert (message &rest keys
                 &key
                   alert-style
                   informative-text
                   icon
                   help-anchor
                   shows-help-p
                   buttons
                 &allow-other-keys)
  "Open an alert window with MESSAGE.
Return `ns-modal-response' code.

Parameters:
+ ALERT-STYLE (`ns-alert-style')
+ MESSAGE
  main message of the alert
+ INFORMATIVE-TEXT
  Additional information about the alert.
+ ICON
  An optional, custom icon to display in the alert, which is
  used instead of the default app icon.
+ SHOWS-HELP-P
  if or not shows help
+ HELP-ANCHOR
  help message
+ BUTTONS
  a list of buttons titles"
  (declare (ignore alert-style
                   informative-text
                   icon
                   help-anchor
                   shows-help-p
                   buttons))
  (setf (getf keys :message) message)
  (with-autorelease-pool ()
    (decode-ns-modal-response
     (invoke (autorelease (apply #'alloc-init 'ns-alert keys))
             "runModal"))))

;;; Open and Save Panels

(define-objc-class "NSOpenPanel" ()
  (("canChooseFiles"
    :accessor can-choose-files-p
    :before   as-boolean
    :documentation
    "A Boolean that indicates whether the user can choose files in the panel.

When the value of this property is true, users can choose files in the panel.

see https://developer.apple.com/documentation/appkit/nsopenpanel/canchoosefiles?language=objc")
   ("canChooseDirectories"
    :accessor can-choose-directories-p
    :before   as-boolean
    :documentation
    "A Boolean that indicates whether the user can choose directories in the panel.

When the value of this property is true, users can choose directories in the panel.

see https://developer.apple.com/documentation/appkit/nsopenpanel/canchoosedirectories?language=objc")
   ("resolvesAliases"
    :accessor resolves-aliases-p
    :before   as-boolean
    :documentation
    "A Boolean that indicates whether the panel resolves aliases.

When the value of this property is true, dropping an alias on the
panel or asking for filenames or URLs returns the resolved
aliases. The default value of this property is true. When this value
is false, selecting an alias returns the alias instead of the file or
directory it represents.

see https://developer.apple.com/documentation/appkit/nsopenpanel/resolvesaliases?language=objc")
   ("allowsMultipleSelection"
    :accessor allows-multiple-selection-p
    :before   as-boolean
    :documentation
    "A Boolean that indicates whether the user may select multiple
files and directories.

When the value of this property is true, the user may select multiple
items from the browser. When the selection contains multiple items,
use the URLs property to retrieve those items instead of the inherited
URL property.

see https://developer.apple.com/documentation/appkit/nsopenpanel/allowsmultipleselection?language=objc")
   ("accessoryViewDisclosed"
    :accessor accessory-view-disclosed-p
    :documentation
    "A Boolean value that indicates whether the panel’s accessory view is visible.

The value of this property is true when the accessory view is visible,
and false when it isn’t. Setting the value of this property
programmatically changes the visibility of the accessory panel. If no
accessory panel is present, setting this property does nothing.

see https://developer.apple.com/documentation/appkit/nsopenpanel/isaccessoryviewdisclosed?language=objc")
   ("canDownloadUbiquitousContents"
    :accessor can-download-ubiquitous-contents-p
    :documentation
    "A Boolean value that indicates how the panel responds to iCloud
documents that aren’t fully downloaded locally.

When the value of this property is true, the panel disallows opening
non-local iCloud files. If the user selects a non-local file, the
panel attempts to download that file. When the value of this property
is false, the user may select and open non-local files. Your app is
responsible for downloading the files and reporting progress or any
issues.

The default value of this property is true, except for applications
linked against the OS X v10.9 SDK or earlier that have adopted iCloud
by specifying a ubiquitous container identifier entitlement.

For a better user experience, set this property to false and download
the file’s contents with an NSFileCoordinator object. Show the
dlownload progress using a NSProgress or NSMetadataQuery object.

see https://developer.apple.com/documentation/appkit/nsopenpanel/candownloadubiquitouscontents?language=objc")
   ("canResolveUbiquitousConflicts"
    :accessor can-resolve-ubiquitous-conflicts-p
    :documentation
    "A Boolean value that indicates how the panel responds to iCloud
documents that have conflicting versions.

When the value of this property is true, and the user attempts to open
one or more documents with conflicts, the panel displays the conflict
resolution UI. The user must resolve any conflicts before opening the
documents. When the value of this property is false, the your
application is responsible for handling any conflicts.

The default value of this property is true, except for applications
linked against the OS X v10.9 SDK or earlier that have adopted iCloud
by specifying a ubiquitous container identifier entitlement.

For a better user experience, set this property to false and check the
NSURLUbiquitousItemHasUnresolvedConflictsKey key of each item. When a
conflict exists, retrieve a NSFileVersion object for each version and
present your own UI to resolve that conflict.

see https://developer.apple.com/documentation/appkit/nsopenpanel/canresolveubiquitousconflicts?language=objc"))
  (:documentation
   "A panel that prompts the user to select a file to open.

Apps use the Open panel as a convenient way to query the user for the
name of a file to open. In macOS 10.15 and later, the system always
draws Open panels in a separate process, regardless of whether the app
is sandboxed. When the user chooses a file to open, macOS adds that
file to the app’s sandbox. Prior to macOS 10.15, the system drew the
panels in a separate process only for sandboxed apps.

see https://developer.apple.com/documentation/appkit/nsopenpanel?language=objc"))

(define-objc-class "NSSavePanel" ()
  (;; Configuring the Panel's Appearance
   ("title"
    :accessor title
    :before   as-ns-string
    :after    ns-string-to-string
    :documentation
    "The title of the panel.
see https://developer.apple.com/documentation/appkit/nssavepanel/title?language=objc")
   ("prompt"
    :accessor prompt
    :before   as-ns-string
    :after    ns-string-to-string
    :documentation
    "The text to display in the default button.
see https://developer.apple.com/documentation/appkit/nssavepanel/prompt?language=objc")
   ("message"
    :accessor message
    :before   as-ns-string
    :after    ns-string-to-string
    :documentation
    "The message text displayed in the panel.
see https://developer.apple.com/documentation/appkit/nssavepanel/message?language=objc")
   ("nameFieldLabel"
    :accessor name-field-label
    :before   as-ns-string
    :after    ns-string-to-string
    :documentation
    "The label text displayed in front of the filename text field.
see https://developer.apple.com/documentation/appkit/nssavepanel/namefieldlabel?language=objc")
   ("nameFieldStringValue"
    :accessor name-field
    :before   as-ns-string
    :after    ns-string-to-string
    :documentation
    "The user-editable filename currently shown in the name field.
see https://developer.apple.com/documentation/appkit/nssavepanel/namefieldstringvalue?language=objc")
   ("directoryURL"
    :accessor directory-url
    :before   as-ns-url
    :documentation
    "The current directory shown in the panel.
see https://developer.apple.com/documentation/appkit/nssavepanel/directoryurl?language=objc")
   ("showsTagField"
    :accessor shows-tag-field-p
    :before   as-boolean
    :documentation
    "A Boolean value that indicates whether the panel displays the Tags field.
see https://developer.apple.com/documentation/appkit/nssavepanel/showstagfield?language=objc")
   ;;; Configuring the Panel’s Behavior
   ("canCreateDirectories"
    :accessor can-create-directories-p
    :documentation
    "A Boolean value that indicates whether the panel displays UI for
creating directories.
see https://developer.apple.com/documentation/appkit/nssavepanel/cancreatedirectories?language=objc")
   ("canSelectHiddenExtension"
    :accessor can-select-hidden-extension-p
    :documentation
    "A Boolean value that indicates whether the panel displays UI for
hiding or showing filename extensions.
see https://developer.apple.com/documentation/appkit/nssavepanel/canselecthiddenextension?language=objc")
   ("showsHiddenFiles"
    :accessor shows-hidden-files-p
    :documentation
    "A Boolean value that indicates whether the panel displays files
that are normally hidden from the user.
see https://developer.apple.com/documentation/appkit/nssavepanel/showshiddenfiles?language=objc")
   ("extensionHidden"
    :accessor extension-hidden-p
    :documentation
    "A Boolean value that indicates whether to display filename extensions.
see https://developer.apple.com/documentation/appkit/nssavepanel/isextensionhidden?language=objc")
   ("expanded"
    :reader expanded-p
    :documentation
    "A Boolean value that indicates whether whether the panel is expanded.
see https://developer.apple.com/documentation/appkit/nssavepanel/isexpanded?language=objc"))
  (:documentation
   "A panel that prompts the user for information about where to save a file.

The Save panel provides an interface for specifying the location to
save a file and the name of that file. You present this panel when the
user attempts to save a new document, or when the user saves a copy of
an existing document to a new location. The panel includes UI for
browsing the file system, selecting a directory, and specifying the
new name for the file. You can also add custom UI for your app using
an accessory view.

An NSSavePanel object reports user interactions to its associated
delegate object, which must adopt the NSOpenSavePanelDelegate
protocol. Use your delegate object to validate the user’s selection
and respond to user interactions with the panel.

In macOS 10.15, the system always displays the Save dialog in a
separate process, regardless of whether the app is sandboxed. When the
user saves the document, macOS adds the saved file to the app’s
sandbox (if necessary) so that the app can write to the file. Prior to
macOS 10.15, the system used a separate process only for sandboxed
apps.

see https://developer.apple.com/documentation/appkit/nssavepanel?language=objc"))

(defmethod run-modal ((panel ns-save-panel) &key )
  "Displays the panel and begins its event loop with the current
working (or last-selected) directory as the default starting point.

Return NSFileHandlingPanelOKButton (if the user clicks the OK button)
or NSFileHandlingPanelCancelButton (if the user clicks the Cancel
button).

This method invokes NSApplication’s runModalForWindow: method with
self as the argument.

see https://developer.apple.com/documentation/appkit/nssavepanel/runmodal()?language=objc"
  (decode-ns-modal-response (invoke panel "runModal")))

(defmethod run-model-for-window ((panel ns-save-panel) (window ns-window))
  "Starts a modal event loop for the specified window.
Return

Parameters:
+ PANEL:  `ns-save-panel'
+ WINDOW: The window to be displayed modally. If it is not already
  visible, the window is centered on the screen using the value in its
  center method and made visible and key. If it is already visible, it
  is simply made key.

This method runs a modal event loop for the specified window
synchronously. It displays the specified window, makes it key, starts
the run loop, and processes events for that window. (You do not need
to show the window yourself.) While the app is in that loop, it does
not respond to any other events (including mouse, keyboard, or
window-close events) unless they are associated with the window. It
also does not perform any tasks (such as firing timers) that are not
associated with the modal run loop. In other words, this method
consumes only enough CPU time to process events and dispatch them to
the action methods associated with the modal window.

You can exit the modal loop by calling the stopModal,
stopModalWithCode:, or abortModal methods from your modal window
code. If you use the stopModalWithCode: method to stop the modal event
loop, this method returns the argument passed to
stopModalWithCode:. If you use stopModal instead, this method returns
the constant NSModalResponseStop. If you use abortModal, this method
returns the constant NSModalResponseAbort.

see https://developer.apple.com/documentation/appkit/nsapplication/runmodal(for:)?language=objc"
  (decode-ns-modal-response (invoke panel "runModelForWindow:" window)))

(defmethod allowed-content-types ((panel ns-save-panel))
  "Return a list of `ut-types' that specify the files types to which
you can save. "
  (ns-array-to-list (invoke panel "allowedContentTypes")))

(defmethod (setf allowed-content-types) ((content sequence) (panel ns-save-panel))
  (invoke panel "setAllowedContentTypes:"
          (as-ns-array (map 'list #'as-ut-type content))))

(defmethod (setf allowed-content-types) (content (panel ns-save-panel))
  (invoke panel "setAllowedContentTypes:"
          (as-ns-array (list (as-ut-type content)))))

(defun %configure-ns-save-open-panel
    (panel
     &key
       (title                         nil title?)
       (prompt                        nil prompt?)
       (message                       nil message?)
       (name-field-label              nil name?)
       (name-field                    nil name-field?)
       (directory-url                 nil directory-url?)
       (can-create-directories-p      nil directories?)
       (can-select-hidden-extension-p nil hidden?)
       (shows-hidden-files-p          nil show-hidden?)
       (extension-hidden-p            nil ext-hidden?)
       (allowed-content-types         nil allowed-content-types?)
     &allow-other-keys)
  "Configuring the Panel’s Appearance

Parameters:
+ TITLE
  save/open panel window title
+ PROMPT
  text to display in the default button
+ MESSAGE
  message text displayed in the panel
+ NAME-FIELD-LABEL:
  label text displayed in front of the filename text field
+ NAME-FIELD:
  user-editable filename currently shown in the name field
+ DIRECTORY-URL:
  current directory shown in the panel
+ CAN-CREATE-DIRECTORIES-P
  whether the panel displays UI for creating directories
+ CAN-SELECT-HIDDEN-EXTENSION-P
  whether the panel displays UI for hiding or showing
  filename extensions
+ SHOWS-HIDDEN-FILES-P
  whether the panel displays files that are normally
  hidden from the user
+ EXTENSION-HIDDEN-P
  whether to display filename extensions
+ ALLOWED-CONTENT-TYPES (`as-ut-type')
  a list of `ut-types' like specification that specify the
  files types to which you can save

Dev Note:
these are common configurations for NSOpenPanel and NSSavePanel.

see https://developer.apple.com/documentation/appkit/nssavepanel?language=objc#Configuring-the-Panels-Appearance"
  (declare (type ns-save-panel panel))
  (when title?                 (setf (title                         panel) title))
  (when prompt?                (setf (prompt                        panel) prompt))
  (when message?               (setf (message                       panel) message))
  (when name?                  (setf (name-field-label              panel) name-field-label))
  (when name-field?            (setf (name-field                    panel) name-field))
  (when directory-url?         (setf (directory-url                 panel) directory-url))
  (when directories?           (setf (can-create-directories-p      panel) can-create-directories-p))
  (when hidden?                (setf (can-select-hidden-extension-p panel) can-select-hidden-extension-p))
  (when show-hidden?           (setf (shows-hidden-files-p          panel) shows-hidden-files-p))
  (when ext-hidden?            (setf (extension-hidden-p            panel) extension-hidden-p))
  (when allowed-content-types? (setf (allowed-content-types         panel) allowed-content-types)))

(defun ns-save-panel (&rest keys
                      &key
                        title
                        prompt
                        message
                        name-field-label
                        directory-url
                        can-create-directories-p
                        can-select-hidden-extension-p
                        shows-hidden-files-p
                        extension-hidden-p
                        allowed-content-types
                      &allow-other-keys)
  "Creates a new Save panel and initializes it with default information.
Return pathname of selected save file path or nil if cancelled.

Parameters:
+ TITLE
  save/open panel window title
+ PROMPT
  text to display in the default button
+ MESSAGE
  message text displayed in the panel
+ NAME-FIELD-LABEL:
  label text displayed in front of the filename text field
+ NAME-FIELD:
  user-editable filename currently shown in the name field
+ DIRECTORY-URL:
  current directory shown in the panel
+ CAN-CREATE-DIRECTORIES-P
  whether the panel displays UI for creating directories
+ CAN-SELECT-HIDDEN-EXTENSION-P
  whether the panel displays UI for hiding or showing
  filename extensions
+ SHOWS-HIDDEN-FILES-P
  whether the panel displays files that are normally
  hidden from the user
+ EXTENSION-HIDDEN-P
  whether to display filename extensions
+ ALLOWED-CONTENT-TYPES (`as-ut-type')
  a list of `ut-types' like specification that specify the
  files types to which you can save
"
  (declare (ignore title
                   prompt
                   message
                   name-field-label
                   directory-url
                   can-create-directories-p
                   can-select-hidden-extension-p
                   shows-hidden-files-p
                   extension-hidden-p
                   allowed-content-types))
  (with-autorelease-pool ()
    (let ((panel (invoke 'ns-save-panel "savePanel")))
      (apply #'%configure-ns-save-open-panel panel keys)
      (when (eq (run-modal panel) :ok)
        (ns-url-to-pathname (invoke panel "URL"))))))

(defun ns-open-panel (&rest keys &key
                        title
                        prompt
                        message
                        name-field-label
                        directory-url
                        can-create-directories-p
                        can-select-hidden-extension-p
                        shows-hidden-files-p
                        extension-hidden-p
                        allowed-content-types
                        (can-choose-files-p                 nil files?)
                        (can-choose-directories-p           nil dir?)
                        (resolves-aliases-p                 nil aliases?)
                        (allows-multiple-selection-p        nil multiple?)
                        (accessory-view-disclosed-p         nil accessory?)
                        (can-download-ubiquitous-contents-p nil download?)
                        (can-resolve-ubiquitous-conflicts-p nil conflicts?)
                      &allow-other-keys)
  "Creates a new Open panel and initializes it.
Return a list of pathname for opened files.

Parameters:
+ TITLE
  save/open panel window title
+ PROMPT
  text to display in the default button
+ MESSAGE
  message text displayed in the panel
+ NAME-FIELD-LABEL:
  label text displayed in front of the filename text field
+ NAME-FIELD:
  user-editable filename currently shown in the name field
+ DIRECTORY-URL:
  current directory shown in the panel
+ CAN-CREATE-DIRECTORIES-P
  whether the panel displays UI for creating directories
+ CAN-SELECT-HIDDEN-EXTENSION-P
  whether the panel displays UI for hiding or showing
  filename extensions
+ SHOWS-HIDDEN-FILES-P
  whether the panel displays files that are normally
  hidden from the user
+ EXTENSION-HIDDEN-P
  whether to display filename extensions
+ ALLOWED-CONTENT-TYPES (`as-ut-type')
  a list of `ut-types' like specification that specify the
  files types to which you can save
+ CAN-CHOOSE-FILES-P
  whether the user can choose files in the panel
+ CAN-CHOOSE-DIRECTORIES-P
  whether the user can choose directories in the panel
+ RESOLVES-ALIASES-P
  whether the panel resolves aliases
+ ALLOWS-MULTIPLE-SELECTION-P
  whether the user may select multiple files and directories
+ ACCESSORY-VIEW-DISCLOSED-P
  whether the panel’s accessory view is visible
+ CAN-DOWNLOAD-UBIQUITOUS-CONTENTS-P
  whether the panel responds to iCloud documents that aren’t
  fully downloaded locally.
+ CAN-RESOLVE-UBIQUITOUS-CONFLICTS-P
  whether the panel responds to iCloud documents that have
  conflicting versions
"
  (declare (ignore title
                   prompt
                   message
                   name-field-label
                   directory-url
                   can-create-directories-p
                   can-select-hidden-extension-p
                   shows-hidden-files-p
                   extension-hidden-p
                   allowed-content-types))
  (with-autorelease-pool ()
    (let ((panel (invoke 'ns-open-panel "openPanel")))
      (apply #'%configure-ns-save-open-panel panel keys)
      (when files?     (setf (can-choose-files-p                 panel) can-choose-files-p))
      (when dir?       (setf (can-choose-directories-p           panel) can-choose-directories-p))
      (when aliases?   (setf (resolves-aliases-p                 panel) resolves-aliases-p))
      (when multiple?  (setf (allows-multiple-selection-p        panel) allows-multiple-selection-p))
      (when accessory? (setf (accessory-view-disclosed-p         panel) accessory-view-disclosed-p))
      (when download?  (setf (can-download-ubiquitous-contents-p panel) can-download-ubiquitous-contents-p))
      (when conflicts? (setf (can-resolve-ubiquitous-conflicts-p panel) can-resolve-ubiquitous-conflicts-p))
      (when (eq (run-modal panel) :ok)
        (mapcar #'ns-url-to-pathname (ns-array-to-list (invoke panel "URLs")))))))


;;; Share Panel

(define-objc-class "NSSharingServicePicker" ()
  ()
  (:documentation
   "A list of sharing services that the user can choose from.
see https://developer.apple.com/documentation/appkit/nssharingservicepicker?language=objc"))

(define-objc-class "NSPreviewRepresentingActivityItem" ()
  ()
  (:documentation
   "A type that adds metadata to an item you share using the macOS share sheet.
see https://developer.apple.com/documentation/appkit/nspreviewrepresentingactivityitem?language=objc"))

;;; Print and PDF Panels

(define-objc-class "NSPDFPanel" ()
  ()
  (:documentation
   "A Save or Export as PDF panel that’s consistent with the macOS user interface.
see https://developer.apple.com/documentation/appkit/nspdfpanel?language=objc"))

;;; Color Panels

(define-objc-enum ns-color-panel-mode
  "A type defined for the enum constants specifying color panel modes.
see https://developer.apple.com/documentation/appkit/nscolorpanel/mode-swift.enum?language=objc"
  "Color Panel Modes"
  (:none           18446744073709551615 "No color panel mode.")
  (:gray           0                    "The grayscale-alpha color mode.")
  (:rgb            1                    "The red-green-blue color mode.")
  (:cmyk           2                    "The cyan-magenta-yellow-black color mode.")
  (:hsb            3                    "The hue-saturation-brightness color mode.")
  (:custom-palette 4                    "The custom palette color mode.")
  (:color-list     5                    "The custom color list mode.")
  (:wheel          6                    "The color wheel mode.")
  (:crayon         7                    "The crayon picker mode."))

(define-objc-class "NSColorPanel" ()
  (("mode"
    :accessor mode
    :before   as-ns-color-panel-mode
    :after    decode-ns-color-panel-mode
    :documentation
    "The mode of the receiver the mode is one of the modes allowed by the color mask.
see https://developer.apple.com/documentation/appkit/nscolorpanel/mode-swift.property?language=objc")
   ("continuous"
    :accessor continuousp
    :before   as-boolean
    :documentation
    "A Boolean value indicating whether the receiver continuously
sends the action message to the target.
see https://developer.apple.com/documentation/appkit/nscolorpanel/iscontinuous?language=objc")
   ("color"
    :accessor color
    :before   as-ns-color
    :documentation "The color of the receiver. "))
  (:documentation
   "A standard user interface for selecting color in an app.
see https://developer.apple.com/documentation/appkit/nscolorpanel?language=objc"))

(defmethod (setf target) ((target ns-object) (panel ns-color-panel))
  (invoke panel "setTarget:" target))

(defmethod (setf action) (action (panel ns-color-panel))
  (invoke panel "setAction:" (coerce-to-selector action)))

(define-objc-method ("NSObject" "changeColor:" change-color) :void
    ((sender :object))
  (:documentation
   "Called to respond to color changing events.

Parameters:
+ SELF:   responder to the message
+ SENDER: the control that send the message
"))

(defun ns-color-panel (&key target
                         (action #'change-color)
                         (color nil color?)
                         (continuousp nil continuous?)
                         (mode nil mode?)
                       &allow-other-keys)
  "Returns the shared NSColorPanel instance, creating it if necessary.

Parameters:
+ TARGET: sets the target of the receiver
+ ACTION: sets the color panel's action message (default `change-color')
+ COLOR:  sets the color panel initial color
+ CONTINUOUSP: whether the receiver continuously sends
  the action message to the target
+ MODE: sets the mode of the receiver (`ns-color-panel-mode')

see https://developer.apple.com/documentation/appkit/nscolorpanel/shared?language=objc"
  (declare (type (or null ns-object) target))
  (let ((panel (invoke 'ns-color-panel "sharedColorPanel")))
    (when target
      (setf (target panel) target
            (action panel) action))
    (when color?      (setf (color       panel) color))
    (when continuous? (setf (continuousp panel) continuousp))
    (when mode?       (setf (mode        panel) mode))
    panel))

;;; Protocol
;; NSColorPickingCustom
;; A set of methods that provides a way to add color pickers—custom
;; user interfaces for color selection—to an app’s color panel.
;;
;; NSColorPickingCustom works with the NSColorPickingDefault
;; protocol—which provides basic behavior for a color picker—to enable
;; custom color pickers.
;;
;; Note:
;; This protocol must be implemented by a custom picker, or an error
;; will occur.

(defmethod set-color ((object ns-object) color)
  "Adjusts the receiver to make the specified color the currently
selected color.

Parameters:
+ COLOR: The color to set as the currently selected color.

This method is invoked on the current color picker each time
NSColorPanel‘s color method is invoked. If color is actually different
from the color picker’s color (as it would be if, for example, the
user dragged a color into NSColorPanel‘s color well), this method
could be used to update the color picker’s color to reflect the
change.

see https://developer.apple.com/documentation/appkit/nscolorpickingcustom/setcolor(_:)?language=objc"
  (invoke object "setColor:" (as-ns-color color)))

(define-objc-class "NSColorPicker" ()
  ()
  (:documentation
   "An abstract superclass that implements the default color picking protocol.
see https://developer.apple.com/documentation/appkit/nscolorpicker?language=objc"))

;;; Font Panels

(define-objc-class "NSFontPanel" ()
  ()
  (:documentation
   "The Font panel a user interface object that displays a list of
available fonts, letting the user preview them and change the font
used to display text.

Actual changes to the font panel are made through conversion messages
sent to the shared NSFontManager instance.

There’s only one Font panel for each app.

see https://developer.apple.com/documentation/appkit/nsfontpanel?language=objc"))

(define-objc-method ("NSObject" "changeFont:" change-font) :void
    ((sender :object))
  (:documentation
   "Called to respond to font changing events.

Parameters:
+ SELF:   responder to the message
+ SENDER: the control that send the message
"))

(defun ns-font-panel (&key
                        font
                        target
                        (action #'change-font)
                        (multiple nil)
                        (sender   nil)
                      &allow-other-keys)
  "Returns the single NSFontPanel instance for the application,
creating it if necessary.

Parameters:
+ FONT:     `ns-font' object to be show in font panel
+ MULTIPLE: If true, the Font panel indicates that more than one font
  is contained in the selection; if false, it does not.
+ TARGET:   target to send ACTION when changing font
+ ACTION:   action to be sent (default `change-font')
"
  (let ((manager (ns-font-manager)))
    (when font
      (invoke manager "setSelectedFont:isMultiple:"
              font
              (as-boolean multiple)))
    (when target
      (invoke manager "setTarget:" target)
      (invoke manager "setAction:" (coerce-to-selector action)))
    (invoke manager "orderFrontFontPanel:" sender)))

;;; User Interface



;;;; Sound, Speech, and Haptics
;; Play sounds and haptic feedback, and incorporate speech recognition and synthesis into your interface.
;; see https://developer.apple.com/documentation/appkit/sound-speech-and-haptics?language=objc

;;; Sounds

(define-objc-class "NSSound" ()
  ()
  (:documentation
   "A simple interface for loading and playing audio files.
see https://developer.apple.com/documentation/appkit/nssound?language=objc"))

;;; Speech

(define-objc-class "NSSpeechRecognizer" ()
  ()
  (:documentation
   "The Cocoa interface to speech recognition in macOS.
see https://developer.apple.com/documentation/appkit/nsspeechrecognizer?language=objc"))

(define-objc-class "NSSpeechSynthesizer" ()
  ()
  (:documentation
   "The Cocoa interface to speech synthesis in macOS.
see https://developer.apple.com/documentation/appkit/nsspeechsynthesizer?language=objc"))

;;; Haptics

(define-objc-class "NSHapticFeedbackManager" ()
  ()
  (:documentation
   "An object that provides access to the haptic feedback management attributes on a system with a Force Touch trackpad.
see https://developer.apple.com/documentation/appkit/nshapticfeedbackmanager?language=objc"))

(define-objc-class "NSAlignmentFeedbackFilter" ()
  ()
  (:documentation
   "An object that can filter the movement of an object and provides haptic feedback when alignment occurs.
see https://developer.apple.com/documentation/appkit/nsalignmentfeedbackfilter?language=objc"))


;;;; Supporting Continuity Camera in Your Mac App
;; Incorporate scanned documents and pictures from a user’s iPhone, iPad, or iPod touch
;; into your Mac app using Continuity Camera.
;; see https://developer.apple.com/documentation/appkit/supporting-continuity-camera-in-your-mac-app?language=objc


;;;; Mouse, Keyboard, and Trackpad
;; Handle events related to mouse, keyboard, and trackpad input.
;; see https://developer.apple.com/documentation/appkit/mouse-keyboard-and-trackpad?language=objc

(define-objc-mask ns-event-mask
  "Constants that you use to filter out specific event types
from the stream of incoming events.
see https://developer.apple.com/documentation/appkit/nsevent/eventtypemask?language=objc"
  "Getting Any Event"
  (:any                  18446744073709551615 "A mask that matches any type of event.")
  "Getting Mouse-Related Events"
  (:left-mouse-down                         2 "A mask for left mouse-down events.")
  (:left-mouse-dragged                     64 "A mask for left mouse-dragged events.")
  (:left-mouse-up                           4 "A mask for left mouse-up events.")
  (:right-mouse-down                        8 "A mask for right mouse-down events.")
  (:right-mouse-dragged                   128 "A mask for right mouse-dragged events.")
  (:right-mouse-up                         16 "A mask for right mouse-up events.")
  (:other-mouse-down                 33554432 "A mask for tertiary mouse-down events.")
  (:other-mouse-dragged             134217728 "A mask for tertiary mouse-dragged events.")
  (:other-mouse-up                   67108864 "A mask for tertiary mouse-up events.")
  (:mouse-entered                         256 "A mask for mouse-entered events.")
  (:mouse-moved                            32 "A mask for mouse-moved events.")
  (:mouse-exited                          512 "A mask for mouse-exited events.")
  "Getting Keyboard Events"
  (:key-down                             1024 "A mask for key-down events.")
  (:key-up                               2048 "A mask for key-up events.")
  "Getting Touch Events"
  (:begin-gesture                      524288 "A mask for begin-gesture events.")
  (:end-gesture                       1048576 "A mask for end-gesture events.")
  (:magnify                        1073741824 "A mask for magnify-gesture events.")
  (:smart-magnify                  4294967296 "A mask for smart-zoom gesture events.")
  (:swipe                          2147483648 "A mask for swipe-gesture events.")
  (:rotate                             262144 "A mask for rotate-gesture events.")
  (:gesture                         536870912 "A mask for generic gesture events.")
  (:direct-touch                 137438953472 "A mask for touch events.")
  (:tablet-point                      8388608 "A mask for tablet-point events.")
  (:tablet-proximity                 16777216 "A mask for tablet-proximity events.")
  (:pressure                      17179869184 "A mask for pressure-change events.")
  "Getting Input Events"
  (:scroll-wheel                      4194304 "A mask for scroll-wheel events.")
  (:change-mode                  274877906944 "A mask for change-mode events.")
  "Getting System Events"
  (:appkit-defined                       8192 "A mask for AppKit–defined events.")
  (:application-defined                 32768 "A mask for app-defined events.")
  (:cursor-update                      131072 "A mask for cursor-update events.")
  (:flags-changed                        4096 "A mask for flags-changed events.")
  (:periodic                            65536 "A mask for periodic events.")
  (:system-defined                      16384 "A mask for system-defined events.")
  "Creating an Event Mask"
  (:from-type                      4342827560 "Returns the event mask for the specified type."))

(define-objc-enum ns-event-type
  "Constants for the types of events that responder objects can handle.
see https://developer.apple.com/documentation/appkit/nsevent/eventtype?language=objc"
  "Getting Mouse-Related Event Types"
  (:left-mouse-down      1 "The user pressed the left mouse button.")
  (:left-mouse-dragged   6 "The user moved the mouse while holding down the left mouse button.")
  (:left-mouse-up        2 "The user released the left mouse button.")
  (:right-mouse-down     3 "The user pressed the right mouse button.")
  (:right-mouse-up       4 "The user released the right mouse button.")
  (:right-mouse-dragged  7 "The user moved the mouse while holding down the right mouse button.")
  (:other-mouse-down    25 "The user pressed a tertiary mouse button.")
  (:other-mouse-dragged 27 "The user moved the mouse while holding down a tertiary mouse button.")
  (:other-mouse-up      26 "The user released a tertiary mouse button.")
  (:mouse-moved          5 "The user moved the mouse in a way that caused the cursor to move onscreen.")
  (:mouse-entered        8 "The cursor entered a well-defined area, such as a view.")
  (:mouse-exited         9 "The cursor exited a well-defined area, such as a view.")
  "Getting Keyboard Event Types"
  (:key-down            10 "The user pressed a key on the keyboard.")
  (:key-up              11 "The user released a key on the keyboard.")
  (:begin-gesture       19 "An event marking the beginning of a gesture.")
  (:end-gesture         20 "An event that marks the end of a gesture.")
  (:magnify             30 "The user performed a pinch-open or pinch-close gesture.")
  (:smart-magnify       32 "The user performed a smart-zoom gesture.")
  (:swipe               31 "The user performed a swipe gesture.")
  (:rotate              18 "The user performed a rotate gesture.")
  (:gesture             29 "The user performed a nonspecific type of gesture.")
  (:direct-touch        37 "The user touched a portion of the touch bar.")
  (:tablet-point        23 "The user touched a point on a tablet.")
  (:tablet-proximity    24 "A pointing device is near, but not touching, the associated tablet.")
  (:pressure            34 "An event that reports a change in pressure on a pressure-sensitive device.")
  "Getting Other Input Types"
  (:scroll-wheel        22 "The scroll wheel position changed.")
  (:change-mode         38 "The user changed the mode of a connected device.")
  "Getting System Event Types"
  (:appkit-defined      13 "An AppKit-related event occurred.")
  (:application-defined 15 "An app-defined event occurred.")
  (:cursor-update       17 "An event that updates the cursor.")
  (:flags-changed       12 "The event flags changed.")
  (:periodic            16 "An event that provides execution time to periodic tasks.")
  (:quick-look          33 "An event that initiates a Quick Look request.")
  (:system-defined      14 "A system-related event occurred."))

(define-objc-enum ns-event-subtype
  "Subtypes for various types of events.
see https://developer.apple.com/documentation/appkit/nsevent/eventsubtype?language=objc"
  "Getting AppKit Event Subtypes
These subtypes apply when the event type is `:appkit-defined'. "
  (:application-activated     1 "An app-activation event occurred.")
  (:application-deactivated   2 "An app-deactivation event occurred.")
  (:screen-changed            8 "An event that indicates a window changed screens.")
  (:window-exposed            0 "An event that indicates a window’s contents are visible again.")
  "Getting System Event Subtypes
These subtypes apply when the event type is `:system-defined'. "
  (:window-moved              4 "An event that indicates a window moved.")
  (:power-off                 1 "An event that indicates a system shutdown"
                              "or restart operation is in progress.")
  (:mouse-event               0 "A mouse event occurred.")
  "Getting Other Subtypes"
  (:tablet-point              1 "A tablet-pointer event occurred.")
  (:tablet-proximity          2 "A tablet-proximity event occurred.")
  (:touch                     3 "A touch event occurred."))

(define-objc-mask ns-event-modifier-flags
  "Flags that represent key states in an event object.
see https://developer.apple.com/documentation/appkit/nsevent/modifierflags-swift.struct?language=objc"
  "Event Modifer Flags. "
  (:caps-lock                          65536 "The Caps Lock key has been pressed.")
  (:shift                             131072 "The Shift key has been pressed.")
  (:control                           262144 "The Control key has been pressed.")
  (:option                            524288 "The Option or Alt key has been pressed.")
  (:command                          1048576 "The Command key has been pressed.")
  (:numeric-pad                      2097152 "A key in the numeric keypad"
                                     "or an arrow key has been pressed.")
  (:help                             4194304 "The Help key has been pressed.")
  (:function                         8388608 "A function key has been pressed.")
  (:device-independent-flags-mask 4294901760 "Device-independent modifier flags are masked."))

(define-objc-class "NSEvent" ()
  (("type"
    :reader event-type
    :after  decode-ns-event-type
    :documentation
    "The event’s type.
See https://developer.apple.com/documentation/appkit/nsevent/type?language=objc")
   ("subtype"
    :reader event-subtype
    :after decode-ns-event-subtype
    :documentation
    "The event’s subtype.
See https://developer.apple.com/documentation/appkit/nsevent/subtype?language=objc")
   ("modifierFlags"
    :reader modifier-flags
    :after decode-ns-event-modifier-flags
    :documentation
    "https://developer.apple.com/documentation/appkit/nsevent/modifierflags-swift.property?language=objc")
   ("locationInWindow"
    :reader location-in-window
    :documentation
    "The event location in the base coordinate system of the associated window.")
   ("timestamp"
    :reader timestamp
    :documentation
    "The time when the event occurred in seconds since system startup.")
   ("window"
    :reader window
    :documentation
    "The window object associated with the event. "))
  (:documentation
   "An object that contains information about an input action,
such as a mouse click or a key press.
AppKit reports events that occur in a window to the app that created
the window. Events include mouse clicks, key presses, and other types
of input to the system. An NSEvent object contains pertinent
information about each event, such as the event type and when the
event occurred. The event type defines what other information is
available in the event object. For example, a keyboard event contains
information about the pressed keys.

Although you can create NSEvent objects directly, you typically
don’t. The system generates them automatically in response to input
from the mouse, keyboard, trackpad, or other peripherals such as
connected tablets. It enqueues those events in its event queue, and
dequeues them when it’s ready to process them. The system delivers
events to the most relevant NSResponder object, which might be the
first responder or the object where the event occurred. For example,
the system delivers mouse-click events to the view that contains the
event location.

To handle events, add support to your app’s NSResponder objects. You
can also use gesture recognizers to handle some events for you and
execute your app’s code at appropriate times. For more information,
see the NSResponder reference.

You can also monitor the events your app receives and modify or cancel
some events as needed. Install a local monitor using the
addLocalMonitorForEventsMatchingMask:handler: method to detect
specific types of events and take action when your app receives
them. Install a global monitor using the
addGlobalMonitorForEventsMatchingMask:handler: method to monitor
events systemwide, although without the ability to modify them.
see https://developer.apple.com/documentation/appkit/nsevent?language=objc"))


;;;; Menus, Cursors, and the Dock
;; Implement menus and cursors to facilitate interactions with your app, and use your
;; app’s Dock tile to convey updated information.
;; see https://developer.apple.com/documentation/appkit/menus-cursors-and-the-dock?language=objc

;;; Menus

(define-objc-enum ns-menu-selection-mode
  "Describes how the menu manages selection states of the menu items
that belong to the same selection group.

This doesn’t apply to menu items that have distinct target-action values.

see https://developer.apple.com/documentation/appkit/nsmenu/selectionmode-swift.enum?language=objc"
  (:automatic  0
               "A selection mode where the menu determines the"
               "appropriate selection mode based on the context "
               "and its constants.")
  (:select-any 2
               "A selection mode where someone can select "
               "multiple items in the menu.")
  (:select-one 1
               "A selection mode where someone can select "
               "at most one menu item in the same selection "
               "group at the same time."))

(define-objc-mask ns-menu-properties
  "These constants are used as a bitmask for specifying a set of menu
or menu item properties, and are contained by the propertiesToUpdate
property.

see https://developer.apple.com/documentation/appkit/nsmenu/properties?language=objc"
  (:title                     1  "The menu item’s title.")
  (:attributed-title          2  "The menu item’s attributed string title.")
  (:key-equivalent            4  "The menu item’s key equivalent.")
  (:image                     8  "The menu image.")
  (:enabled                   16 "Whether the menu item is enabled or disabled.")
  (:accessibility-description 32 "The menu item’s accessibility description."))

(define-objc-enum ns-menu-presentation-style
  "Specifies the style of a menu. "
  (:palette 1 "A menu presentation style where items to"
            "display align horizontally.")
  (:regular 0 "The default presentation style for a menu."))

(define-objc-enum ns-user-interface-layout-direction
  "Specifies the directional flow of the user interface."
  (:left-to-right 0 "Layout direction is left to right.")
  (:right-to-left 1 "Layout direction is right to left."))

(define-objc-class "NSMenu" ()
  (;; Finding Menu Items
   ("itemArray"
    :reader items
    :after  ns-array-to-list
    :documentation
    "A list containing the menu items in the menu.

This property contains an array of menu items in the menu.

see https://developer.apple.com/documentation/appkit/nsmenu/items?language=objc")
   ("numberOfItems"
    :reader len
    :documentation
    "The number of menu items in the menu, including separator items.
see https://developer.apple.com/documentation/appkit/nsmenu/numberofitems?language=objc")
   ;; Managing Submenus
   ("supermenu"
    :reader supermenu
    :documentation
    "The parent menu that contains the menu as a submenu.

This property contains a value of type NSMenu representing the the
parent menu that contains the menu as a submenu. If the menu has no
parent menu, then the value of this property is nil.

You should never invoke the setter method for this property
directly. The setter method is called automatically when changes to
the parent menu occur. You can, however, override the setter method
for this property in order to take action when changes to the parent
menu occur.

see https://developer.apple.com/documentation/appkit/nsmenu/supermenu?language=objc")
   ;; Enabling and Disabling Menu Items
   ("autoenablesItems"
    :accessor autoenables-items-p
    :documentation
    "Whether automatically enables and disables its menu items.

This property contains a Boolean value, indicating whether the menu
automatically enables and disables its menu items. If set to true,
menu items of the menu are automatically enabled and disabled
according to rules computed by the NSMenuValidation informal
protocol. By default, NSMenu objects autoenable their menu items.

see https://developer.apple.com/documentation/appkit/nsmenu/autoenablesitems?language=objc")
   ;; Getting and Setting the Menu Font
   ("font"
    :accessor font
    :before   as-ns-font
    :documentation
    "The font of the menu and its submenus.
see https://developer.apple.com/documentation/appkit/nsmenu/font?language=objc")
   ;; Managing the Title
   ("title"
    :accessor title
    :before   as-ns-string
    :after    ns-string-to-string
    :documentation
    "The title of the menu.
see https://developer.apple.com/documentation/appkit/nsmenu/title?language=objc")
   ;; Selecting Items
   ("selectedItems"
    :reader selected-items
    :after  ns-array-to-list
    :documentation
    "The menu items that are currently selected.

An item selects when its state is NSControlStateValueOn. If the
tracking mode is NSMenuSelectionModeSelectOne or
NSMenuSelectionModeSelectAny, the property only selects or returns
menu items whose show-target action is nil.

see https://developer.apple.com/documentation/appkit/nsmenu/selecteditems?language=objc")
   ;; Configuring Menu Size
   ("minimumWidth"
    :accessor minimum-width
    :documentation
    "The minimum width of the menu in screen coordinates.

This property contains a value of type CGFloat, indicating the minimum
width of the menu in screen coordinates.

The menu will not draw smaller than its minimum width, but may draw
larger if it needs more space. The default value for this property is
0.

see https://developer.apple.com/documentation/appkit/nsmenu/minimumwidth?language=objc")
   ("size"
    :reader size
    :documentation
    "The size of the menu in screen coordinates

This property contains a value of type NSSize, indicating the size of
the menu in screen coordinates.

The menu may draw at a smaller size when shown, depending on its
positioning and display configuration.

see https://developer.apple.com/documentation/appkit/nsmenu/size?language=objc")
   ;; Getting Menu Properties
   ("propertiesToUpdate"
    :reader properties-to-update
    :after  decode-ns-menu-properties
    :documentation
    "The available properties for the menu.

This property contains a bitwise-C OR set of NSMenuProperties values
that are applicable to the menu.

This property may be queried from specific callbacks to determine
which menu properties are defined, and whether or not they are
relevant to changes you need to make to the menu. This property is
intended to allow for more efficient updating of the menu in certain
circumstances.

For example, if the NSMenuPropertyItemImage property isn’t set, your
delegate doesn’t need to spend time updating the images of the menu
items, because the images aren’t needed (for example, during
key-equivalent matching).

You have to update a menu property only if it has changed since you
last set it, even if the corresponding bit is 1. For example, if the
title of a menu item never changes, you have to set it only once.

Accessing this property is optional; it is always acceptable to fully
update all properties of the menu.

see https://developer.apple.com/documentation/appkit/nsmenu/propertiestoupdate?language=objc")
   ;; Managing Presentation Styles
   ;; Working with Palettes
   ;; Managing Menu Change Notifications
   ;; Displaying Contextual Menus
   ;; Managing Display of the State Column
   ("showsStateColumn"
    :accessor shows-state-column-p
    :before   as-boolean
    :documentation
    "Indicates whether the menu displays the state column.

This property contains a Boolean value indicating whether the menu
displays the state column. The default value for this property is
true.

see https://developer.apple.com/documentation/appkit/nsmenu/showsstatecolumn?language=objc")
   ;; Controlling Allocation Zones
   ;; Handling Highlighting
   ("highlightedItem"
    :reader highlighted-item
    :documentation
    "Indicates the currently highlighted item in the menu.

This property indicates the currently highlighted item in the menu. If
no menu is highlighted, this property has a value of nil.

see https://developer.apple.com/documentation/appkit/nsmenu/highlighteditem?language=objc")
   ;; Managing the User Interface
   ("userInterfaceLayoutDirection"
    :accessor user-interface-layout-direction
    :before   as-ns-user-interface-layout-direction
    :after    decode-ns-user-interface-layout-direction
    :documentation
    "Configures the layout direction of menu items in the menu.

This property configures the layout direction (a value of type
NSUserInterfaceLayoutDirection) of menu items in the menu. If no
layout direction is explicitly set for a menu, then the menu defaults
to the layout direction specified for the application object. See
userInterfaceLayoutDirection in NSApplication.

see https://developer.apple.com/documentation/appkit/nsmenu/userinterfacelayoutdirection?language=objc")
   ("delegate"
    :accessor delegate
    :documentation
    "The delegate of the menu.
see https://developer.apple.com/documentation/appkit/nsmenu/delegate?language=objc"))
  (:documentation
   "An object that manages an app’s menus.
see https://developer.apple.com/documentation/appkit/nsmenu?language=objc"))

(defmethod selection-mode ((menu ns-menu))
  "The selection mode of the menu.

The selection mode only affects menu items that belong to the same
selection group. A selection group consists of the items with the same
target-action.

see https://developer.apple.com/documentation/appkit/nsmenu/selectionmode-swift.property?language=objc"
  (decode-ns-menu-selection-mode
   (invoke menu "selectionMode")))

(defmethod (setf selection-mode) (mode (menu ns-menu))
  (invoke menu "setSelectionMode:"
          (as-ns-menu-selection-mode mode)))

(defmethod presentation-style ((menu ns-menu))
  "The presentation style of the menu.
see https://developer.apple.com/documentation/appkit/nsmenu?language=objc"
  (decode-ns-menu-presentation-style
   (invoke menu "presentationStyle")))

(defmethod (setf presentation-style) (style (menu ns-menu))
  (invoke menu "setPresentationStyle:"
          (as-ns-menu-presentation-style style)))

(defmethod init ((menu ns-menu)
                 &key
                   (title               ""         title?)
                   (autoenables-items-p nil        auto?)
                   (font                nil        font?)
                   (selection-mode      :automatic selection?)
                   (presentation-style  :regular   presentation?)
                   (shows-state-column-p nil       column?)
                   (delegate            :self)
                   (user-interface-layout-direction :left-to-right layout?)
                   supermenu
                   items
                   minimum-width
                   service-menu-p
                   windows-menu-p
                   help-menu-p
                   main-menu-p)
  "Initialize `ns-menu' MENU.

Parameters:
+ TITLE:
  title of `ns-menu'
+ SUPERMENU:
  the parent menu that contains the menu as a submenu
+ ITEMS:
  a list of `ns-menu-item' like things that could be
  converted by `as-ns-menu-item'.
+ AUTOENABLES-ITEMS-P
  whether automatically enables and disables its menu items.
+ FONT
  font like things for the menu and its submenus
  converted by `as-ns-font'
+ SELECTION-MODE (`ns-menu-selection-mode')
  selection mode of the menu
+ MINIMUM-WIDTH
  minimum width of the menu in screen coordinates
+ PRESENTATION-STYLE (`ns-menu-presentation-style')
  presentation style of the menu
+ SHOWS-STATE-COLUMN-P
  whether the menu displays the state column.
+ USER-INTERFACE-LAYOUT-DIRECTION
  (`ns-user-interface-layout-direction')
  layout direction of menu items in the menu.
+ DELEGATE (default `:self')
  delegate of the MENU
+ SERVICE-MENU-P
  if non-nil, will set MENU as app's service menu
+ WINDOWS-MENU-P
  if non-nil, will set MENU as app's windows menu
+ HELP-MENU-P
  if non-nil, will set MENU as app's help menu
+ MAIN-MENU-P
  if non-nil, will set MENU as app's main menu

Note: SERVICE-MENU-P, WINDOWS-MENU-P, HELP-MENU-P, MAIN-MENU-P
cannot be set at same time
"
  (declare (type list items)
           (type (or null ns-menu) supermenu)
           (type ns-menu-selection-mode selection-mode)
           (type (or (eql :self) ns-object) delegate))
  (when (> (count-if #'identity (list service-menu-p
                                      windows-menu-p
                                      help-menu-p
                                      main-menu-p))
           1)
    (error ":service-menu-p, :windows-menu-p, :help-menu-p, :main-menu-p~
 cannot be set at same time"))
  (if title?
      (invoke menu "initWithTitle:" (as-ns-string title))
      (invoke menu "init"))
  (when supermenu (add-item supermenu menu))
  (dolist (item items)
    (add-item menu (as-ns-menu-item item)))
  (when auto?         (setf (autoenables-items-p  menu) autoenables-items-p))
  (when font?         (setf (font                 menu) font))
  (when selection?    (setf (selection-mode       menu) selection-mode))
  (when minimum-width (setf (minimum-width        menu) minimum-width))
  (when presentation? (setf (presentation-style   menu) presentation-style))
  (when column?       (setf (shows-state-column-p menu) shows-state-column-p))
  (when layout?       (setf (user-interface-layout-direction menu)
                            user-interface-layout-direction))
  (setf (delegate menu) (if (eq delegate :self) menu delegate))
  (cond (service-menu-p (invoke (ns-app) "setServicesMenu:" menu))
        (windows-menu-p (invoke (ns-app) "setWindowsMenu:"  menu))
        (help-menu-p    (invoke (ns-app) "setHelpMenu:"     menu))
        (main-menu-p    (invoke (ns-app) "setMainMenu:"     menu))))

(defgeneric as-ns-menu (menu &key &allow-other-keys)
  (:documentation "Convert MENU as `ns-menu'. ")
  (:method ((null null)    &key) nil)
  (:method ((menu ns-menu) &key) menu)
  (:method ((items list) &rest keys &key)
    "Convert ITEMS as `ns-menu'.

Parameters:
+ ITEMS:
  list of `ns-menu-item' like thing

Example

    (as-ns-menu '((\"Submenu 1\"
                   :items ((\"Submenu 1.1\")
                           (\"Submenu 1.2\")))
                  (\"Submenu 2\"
                   :items ((\"Submenu 2.1\")
                           ...))))
"
    (setf (getf keys :items) items)
    (apply #'alloc-init 'ns-menu keys)))

(define-objc-class "NSMenuItem" ()
  (;; Enabling a menu item
   ("enabled"
    :accessor enabledp
    :before   as-boolean
    :documentation
    "A Boolean value that indicates whether the menu item is enabled.
see https://developer.apple.com/documentation/appkit/nsmenuitem/isenabled?language=objc")
   ;; Managing hidden status
   ("hidden"
    :accessor hiddenp
    :before   as-boolean
    :documentation
    "A Boolean value that indicates whether the menu item is hidden.
see https://developer.apple.com/documentation/appkit/nsmenuitem/ishiddenorhashiddenancestor?language=objc")
   ("hiddenOrHasHiddenAncestor"
    :reader hidden-or-has-hidden-ancestor-p
    :documentation
    "Whether the menu item or any of its superitems is hidden.

see https://developer.apple.com/documentation/appkit/nsmenuitem/ishiddenorhashiddenancestor?language=objc")
   ;; Managing the target and action
   ("target"
    :accessor target
    :documentation
    "The menu item's target.
see https://developer.apple.com/documentation/appkit/nsmenuitem/target?language=objc")
   ("action"
    :accessor action
    :documentation
    "The menu item's action-method selector.
see https://developer.apple.com/documentation/appkit/nsmenuitem/action?language=objc")
   ;; Managing the title
   ("title"
    :accessor title
    :documentation
    "The menu item’s title.
see https://developer.apple.com/documentation/appkit/nsmenuitem/title?language=objc")
   ("attributedTitle"
    :accessor attributed-title
    :documentation
    "A custom string for a menu item.
see https://developer.apple.com/documentation/appkit/nsmenuitem/attributedtitle?language=objc")
   ;; Managing the tag
   ("tag"
    :accessor tag
    :documentation
    "The menu item’s tag.
see https://developer.apple.com/documentation/appkit/nsmenuitem/tag?language=objc")
   ;; Manging the state
   ("state"
    :accessor state
    :before   as-ns-control-state-value
    :documentation
    "The state of the menu item.

The image associated with the new state is displayed to the left of
the menu item.

see https://developer.apple.com/documentation/appkit/nsmenuitem/state?language=objc")
   ;; Managing the image
   ("image"
    :accessor image
    :before   as-ns-image
    :documentation
    "The menu item’s image.
see https://developer.apple.com/documentation/appkit/nsmenuitem/image?language=objc")
   ("onStateImage"
    :accessor on-state-image
    :before   as-ns-image
    :documentation
    "The image of the menu item that indicates an “on” state.
see https://developer.apple.com/documentation/appkit/nsmenuitem/onstateimage?language=objc")
   ("offStateImage"
    :accessor off-state-image
    :before   as-ns-image
    :documentation
    "The image of the menu item that indicates an “off” state.
see https://developer.apple.com/documentation/appkit/nsmenuitem/offstateimage?language=objc")
   ("mixedStateImage"
    :accessor mixed-state-image
    :before   as-ns-image
    :documentation
    "The image of the menu item that indicates a “mixed” state,
that is, a state neither “on” nor “off.”
see https://developer.apple.com/documentation/appkit/nsmenuitem/mixedstateimage?language=objc")
   ;; Managing the badge
   ("badge"
    :accessor badge
    :before   as-ns-menu-item-badge
    :documentation
    "Additional quantitative information specific to `ns-menu-item'.
see `ns-menu-item-badge'. ")
   ;; Managing the section header
   ("sectionHeader"
    :accessor section-header-p
    :before   as-boolean
    :documentation
    "Whether the menu item is a section header.
see https://developer.apple.com/documentation/appkit/nsmenuitem/issectionheader?language=objc")
   ;; Managing submenus
   ("submenu"
    :accessor submenu
    :before   as-ns-menu
    :documentation
    "The submenu of the menu item.
see https://developer.apple.com/documentation/appkit/nsmenuitem/submenu?language=objc")
   ("hasSubmenu"
    :reader has-submenu-p
    :documentation
    "A Boolean value that indicates whether the menu item has a submenu.
see https://developer.apple.com/documentation/appkit/nsmenuitem?language=objc")
   ("parentItem"
    :reader parent-item
    :documentation
    "The menu item whose submenu contains the receiver.
see https://developer.apple.com/documentation/appkit/nsmenuitem/parent?language=objc")
   ;; Managing the separator item
   ("separatorItem"
    :reader separator-item-p
    :documentation
    "A Boolean value indicating whether the menu item is a separator item.
see https://developer.apple.com/documentation/appkit/nsmenuitem?language=objc")
   ;; Managing the owning menu
   ("menu"
    :reader menu
    :documentation
    "The menu item’s menu.
see https://developer.apple.com/documentation/appkit/nsmenuitem/menu?language=objc")
   ;; Managing key equivalents
   ("keyEquivalent"
    :accessor key-equivalent
    :before   as-ns-string
    :after    ns-string-to-string
    :documentation
    "The menu item’s unmodified key equivalent.

If you want to specify the Backspace key as the key equivalent for a
menu item, use a single character string with NSBackspaceCharacter
(defined in NSText.h as 0x08) and for the Forward Delete key, use
NSDeleteCharacter (defined in NSText.h as 0x7F). Note that these are
not the same characters you get from an NSEvent key-down event when
pressing those keys.

see https://developer.apple.com/documentation/appkit/nsmenuitem/keyequivalent?language=objc")
   ("keyEquivalentModifierMask"
    :accessor key-equivalent-modifier-mask
    :before   as-ns-event-modifier-flags
    :documentation
    "The menu item’s keyboard equivalent modifiers.

NSShiftKeyMask is a valid modifier for any key equivalent in
mask. This allows you to specify key-equivalents such as
Command-Shift-1 that are consistent across all keyboards. However,
with a few exceptions (such as the German “ß” character), a lowercase
character with NSShiftKeyMask is interpreted the same as the uppercase
character without that mask. For example, Command-Shift-c and
Command-C are considered to be identical key equivalents.

See the NSEvent class specification for more information about
modifier mask values.

see https://developer.apple.com/documentation/appkit/nsmenuitem/keyequivalentmodifiermask?language=objc")
   ;; Managing mnemonics
   ;; Managing user key equivalents
   ;; Managing alternates
   ;; Managing indentation levels
   ;; Managing tool tips
   ("toolTip"
    :accessor tooltip
    :before   as-ns-string
    :after    ns-string-to-string
    :documentation
    "A help tag for the menu item.
see https://developer.apple.com/documentation/appkit/nsmenuitem/tooltip?language=objc"))
  (:documentation
   "A command item in an app menu.

The NSMenuItem class includes some private functionality needed to
maintain binary compatibility with other components of Cocoa. Because
of this fact, you can’t replace the NSMenuItem class with a different
class, but you can subclass it if necessary.

see https://developer.apple.com/documentation/appkit/nsmenuitem?language=objc"))

(declaim (inline %ensure-submenu))
(defun %ensure-submenu (menu-item alloc-submenu-p submenu-title)
  (declare (type ns-menu-item menu-item))
  (or (submenu menu-item)
      (if alloc-submenu-p
          (setf (submenu menu-item)
                (alloc-init 'ns-menu
                            :title (or submenu-title
                                       (title menu-item))))
          (error "~S has no submenu to `add-item'. " menu-item))))

(defmethod init ((item ns-menu-item)
                 &key
                   title
                   action
                   target
                   (enabledp nil  enable?)
                   (hiddenp  nil  hidden?)
                   (tag      0    tag?)
                   (state    :off state?)
                   image
                   on-state-image
                   off-state-image
                   mixed-state-image
                   badge
                   submenu
                   menu
                   items
                   tooltip
                   (key-equivalent "")
                   key-equivalent-modifier-mask
                   (section-header-p nil section-header?)
                   service-menu-p
                   windows-menu-p
                   help-menu-p
                 &allow-other-keys)
  "Initialize `ns-menu-item' ITEM.

Parameters:
+ TITLE:
  the menu item's title
+ ACTION:
  the menu item's action-method selector.
+ TARGET:
  the menu item's target.
+ ENABLEDP:
  whether the menu item is enabled.
+ HIDDENP:
  whether the menu item is hidden.
+ TAG:
  The menu item’s tag.
+ STATE: (`ns-control-state-value')
  The state of the menu item.
+ IMAGE
+ ON-STATE-IMAGE
+ OFF-STATE-IMAGE
+ MIXED-STATE-IMAGE
+ BADGE (`as-ns-menu-item-badge')
+ SUBMENU (`as-ns-menu')
+ ITEMS
  a list of `as-ns-menu-items' like things
+ MENU
  `ns-menu' to add `ns-menu-item' ITEM
+ TOOLTIP (`as-ns-string')
+ SECTION-HEADER-P
+ KEY-EQUIVALENT:
  a string of the menu item's unmodified key equivalent
+ KEY-EQUIVALENT-MODIFIER-MASK: (`ns-event-modifier-flags')
  key modifier mask
+ SERVICE-MENU-P
  set menu-item as service menu item if non-nil
+ WINDOWS-MENU-P
  if non-nil, will set MENU as app's windows menu
+ HELP-MENU-P
  if non-nil, will set MENU as app's help menu
+ MAIN-MENU-P
  if non-nil, will set MENU as app's main menu

Note: SERVICE-MENU-P, WINDOWS-MENU-P, HELP-MENU-P, MAIN-MENU-P
cannot be set at same time"
  (declare (type ns-control-state-value state)
           (type (or null ns-menu) menu))
  (when (> (count-if #'identity (list service-menu-p
                                      windows-menu-p
                                      help-menu-p))
           1)
    (error ":service-menu-p, :windows-menu-p, :help-menu-p~
 cannot be set at same time"))
  (let ((item (invoke item "initWithTitle:action:keyEquivalent:"
                      (as-ns-string title)
                      (and action (coerce-to-selector action))
                      (as-ns-string key-equivalent))))
    (when target            (setf (target            item) target))
    (when enable?           (setf (enabledp          item) enabledp))
    (when hidden?           (setf (hiddenp           item) hiddenp))
    (when tag?              (setf (tag               item) tag))
    (when state?            (setf (state             item) state))
    (when image             (setf (image             item) image))
    (when on-state-image    (setf (on-state-image    item) on-state-image))
    (when off-state-image   (setf (off-state-image   item) off-state-image))
    (when mixed-state-image (setf (mixed-state-image item) mixed-state-image))
    (when badge             (setf (badge             item) badge))
    (when section-header?   (setf (section-header-p  item) section-header-p))

    ;; Ensure submenu and items added
    (when submenu (setf (submenu item) (as-ns-menu submenu)))
    (when items
      (let ((submenu (%ensure-submenu item t title)))
        (dolist (item items)
          (add-item submenu item))))

    (when tooltip           (setf (tooltip           item) tooltip))
    (when key-equivalent-modifier-mask
      (setf (key-equivalent-modifier-mask item) key-equivalent-modifier-mask))
    (when menu (add-item menu item))
    (cond (service-menu-p (invoke (ns-app) "setServicesMenu:" (%ensure-submenu item t title)))
          (windows-menu-p (invoke (ns-app) "setWindowsMenu:"  (%ensure-submenu item t title)))
          (help-menu-p    (invoke (ns-app) "setHelpMenu:"     (%ensure-submenu item t title))))))

(defgeneric add-item (parent item &key &allow-other-keys))

(defmethod add-item ((menu ns-menu) (item ns-menu-item)
                     &key
                       (index 0 index?)
                     &allow-other-keys)
  "Adds a menu item to the end of the menu.
Return ITEM.

Parameters:
+ MENU: `ns-menu'
+ ITEM: `ns-menu-item'
  The menu item (an object conforming to the NSMenuItem protocol) to
  add to the menu.
+ INDEX: invokes insertItem:atIndex:

This method invokes insertItem:atIndex:. Thus, the menu does not
accept the menu item if it already belongs to another menu. After
adding the menu item, the menu updates itself.
"
  (declare (type unsigned-byte index))
  (cond (index? (invoke menu "insertItem:atIndex:" item index))
        (t      (invoke menu "addItem:" item)))
  item)

(defmethod add-item ((menu ns-menu) item &rest keys &key)
  "By default, convert ITEM `as-ns-menu-item'. "
  (add-item menu (apply #'as-ns-menu-item item keys)))

(defmethod add-item ((menu-item ns-menu-item) item &rest keys
                     &key (alloc-submenu-p t) submenu-title)
  "Add ITEM to MENU-ITEM.

Parameters:
+ ALLOC-SUBMENU-P: (default `t')
  If MENU-ITEM has no submenu, alloc submenu if ALLOC-SUBMENU-P
  is non-nil; otherwise raise error
+ SUBMENU-TITLE:
  Custom submenu title when alloc submenu

see (add-item ns-menu *) for other possible parameters. "
  (let ((submenu (restart-case
                     (%ensure-submenu menu-item alloc-submenu-p submenu-title)
                   (alloc-submenu ()
                     :report "Alloc submenu with :alloc-submenu-p as t"
                     (setf (getf keys :alloc-submenu-p) t)
                     (apply #'add-item menu-item item keys)))))
    (coca.objc::remove-plist keys :alloc-submenu-p :submenu-title)
    (apply #'add-item submenu item keys)))

(defgeneric as-ns-menu-item (item &key &allow-other-keys)
  (:documentation "Convert ITEM as `ns-menu-item'. ")
  (:method ((item ns-menu-item) &key) item)
  (:method ((title string) &rest keys &key)
    "Create `ns-menu-item' with TITLE. "
    (setf (getf keys :title) title)
    (apply #'alloc-init 'ns-menu-item keys))
  (:method ((separator (eql :separator)) &key)
    (invoke 'ns-menu-item "separatorItem"))
  (:method ((item list) &rest modify &key)
    "Convert ITEM to `ns-menu-item'.

Parameters:
+ ITEM:

     (TITLE &key AS-NS-MENU-ITEM-KEYS)

Example:

    (as-ns-menu-item '(\"Item\" :action \"action:\"))
"
    (destructuring-bind (title &rest keys &key &allow-other-keys) item
      (setf (getf keys :title) title)
      (loop :for (key val) :on modify :by #'cddr
            :do (setf (getf keys key) val))
      (apply #'alloc-init 'ns-menu-item keys))))

(define-objc-enum ns-menu-item-badge-type
  "Constants that define types of badges for display.

The predefined strings that display are localizable and automatically
handle any pluralization of itemCount.

see https://developer.apple.com/documentation/appkit/nsmenuitembadge/badgetype?language=objc"
  (:alerts    3 "A badge representing the number of alerts.")
  (:new-items 2 "A badge representing the number of new items.")
  (:none      0 "A badge with no string portion.")
  (:updates   1 "A badge representing the number of available updates."))

(define-objc-class "NSMenuItemBadge" ()
  (;; Accessing menu item badge attributes
   ("itemCount"
    :reader item-count
    :documentation
    "The number of items the badge displays.

If you create a badge with a custom string, this value is 0.

see https://developer.apple.com/documentation/appkit/nsmenuitembadge/itemcount?language=objc")
   ("stringValue"
    :reader string-value
    :after  ns-string-to-string
    :documentation
    "The string representation of the badge when it displays.
see https://developer.apple.com/documentation/appkit/nsmenuitembadge/stringvalue-fc9f?language=objc")
   ("type"
    :reader badge-type
    :documentation
    "The type of items the badge displays.
see https://developer.apple.com/documentation/appkit/nsmenuitembadge/type?language=objc"))
  (:documentation
   "A control that provides additional quantitative information specific to a menu item, such as the number of available updates.

You create a badge using an initializer or a predefined factory
method, and then you assign it to the badge property of a NSMenuItem
for display.

                   +---------------------+
                   | Message         [6] |
   NSMenuItem -->  | Changes [3 changes] | <-- NSMenuItemBadge
                   | New Items   [3 new] |
                   +---------------------+

For example, to display a badge with a count, use the initWithCount:
initalizer, passing in the value of count as an Int.

    (alloc-init 'ns-menu-item-badge :count 6)

To display a badge with a custom string, use the initWithString:
initializer, passing in the string you want to display.

    (alloc-init 'ns-menu-item-badge :string \"(N) changes\")

To display a badge using a predefined NSMenuItemBadgeType, use a
factory method such as newItemsWithCount:, passing in the count of the
badge to display.

Important
If you use one of the predefined badge types, the system localizes and
pluralizes the string for you. If you create your own custom badge
string, you need to localize and pluralize that string yourself. For
more information on how to localize and pluralize text, see Localizing
and varying text with a string catalog.

see https://developer.apple.com/documentation/appkit/nsmenuitembadge?language=objc"))

(defmethod init ((badge ns-menu-item-badge)
                 &key
                   (count  0     count?)
                   (type   :none type?)
                   (string ""    string?))
  "Initialize `ns-menu-item-badge' with COUNT or TYPE or STRING.

Parameters:
+ COUNT:
  Creates a badge with a count and an empty string.
+ TYPE (`ns-menu-item-badge-type')
+ STRING:
  Creates a badge with the provided custom string."
  (declare (type integer count)
           (type ns-menu-item-badge-type type))
  (cond ((type?   (invoke badge "initWithCount:type:" count
                          (as-ns-menu-item-badge-type type)))
         (string? (invoke badge "initWithString:" (as-ns-string string)))
         (t       (invoke badge "initWithCount:" count)))))

(defgeneric as-ns-menu-item-badge (badge &key &allow-other-keys)
  (:documentation "Convert BADGE as `ns-menu-item-badge'. ")
  (:method ((badge ns-menu-item-badge) &key) badge)
  (:method (default &key)
    "Default converting DEFAULT as string. "
    (alloc-init 'ns-menu-item-badge :string default)))

;;; Menu Validation

;;; Menu Bar Items

(define-objc-class "NSStatusBar" ()
  ()
  (:documentation
   "An object that manages a collection of status items displayed within the system-wide menu bar.
see https://developer.apple.com/documentation/appkit/nsstatusbar?language=objc"))

(define-objc-class "NSStatusItem" ()
  ()
  (:documentation
   "An individual element displayed in the system menu bar.
see https://developer.apple.com/documentation/appkit/nsstatusitem?language=objc"))

(define-objc-class "NSStatusBarButton" ()
  ()
  (:documentation
   "The appearance and behavior of an item in the systemwide menu bar.
see https://developer.apple.com/documentation/appkit/nsstatusbarbutton?language=objc"))

;;; Cursors

(define-objc-class "NSCursor" ()
  ()
  (:documentation
   "A pointer (also called a cursor).
see https://developer.apple.com/documentation/appkit/nscursor?language=objc"))

(define-objc-class "NSTrackingArea" ()
  ()
  (:documentation
   "A region of a view that generates mouse-tracking and cursor-update events when the pointer is over that region.
see https://developer.apple.com/documentation/appkit/nstrackingarea?language=objc"))

;;; The Dock

(define-objc-class "NSDockTile" ()
  ()
  (:documentation
   "The visual representation of your app’s miniaturized windows and app icon as they appear in the Dock.
see https://developer.apple.com/documentation/appkit/nsdocktile?language=objc"))


;;;; Gestures
;; Encapsulate your app’s event-handling logic in gesture recognizers so that you can reuse
;; that code throughout your app.
;; see https://developer.apple.com/documentation/appkit/gestures?language=objc

;;; Standard Gestures

(define-objc-class "NSClickGestureRecognizer" ()
  ()
  (:documentation
   "A discrete gesture recognizer that tracks a specified number of mouse clicks.
see https://developer.apple.com/documentation/appkit/nsclickgesturerecognizer?language=objc"))

(define-objc-class "NSPressGestureRecognizer" ()
  ()
  (:documentation
   "A discrete gesture recognizer that tracks whether the user holds down a mouse button for a minimum amount of time before releasing it.
see https://developer.apple.com/documentation/appkit/nspressgesturerecognizer?language=objc"))

(define-objc-class "NSPanGestureRecognizer" ()
  ()
  (:documentation
   "A continuous gesture recognizer for panning gestures.
see https://developer.apple.com/documentation/appkit/nspangesturerecognizer?language=objc"))

(define-objc-class "NSRotationGestureRecognizer" ()
  ()
  (:documentation
   "A continuous gesture recognizer that tracks two trackpad touches moving opposite each other in a circular motion.
see https://developer.apple.com/documentation/appkit/nsrotationgesturerecognizer?language=objc"))

(define-objc-class "NSMagnificationGestureRecognizer" ()
  ()
  (:documentation
   "A continuous gesture recognizer that tracks a pinch gesture that magnifies content.
see https://developer.apple.com/documentation/appkit/nsmagnificationgesturerecognizer?language=objc"))

;;; Custom Gestures

(define-objc-class "NSGestureRecognizer" ()
  ()
  (:documentation
   "An object that monitors events and calls its action method when a predefined sequence of events occur.
see https://developer.apple.com/documentation/appkit/nsgesturerecognizer?language=objc"))


;;;; Touch Bar
;; Display interactive content and controls in the Touch Bar.
;; see https://developer.apple.com/documentation/appkit/touch-bar?language=objc

;;; Essentials

(define-objc-class "NSTouchBar" ()
  ()
  (:documentation
   "An object that provides dynamic contextual controls in the Touch Bar of supported models of MacBook Pro.
see https://developer.apple.com/documentation/appkit/nstouchbar?language=objc"))

;;; Touch Bar items

(define-objc-class "NSTouchBarItem" ()
  ()
  (:documentation
   "A UI control shown in the Touch Bar on supported models of MacBook Pro.
see https://developer.apple.com/documentation/appkit/nstouchbaritem?language=objc"))

(define-objc-class "NSCandidateListTouchBarItem" ()
  ()
  (:documentation
   "A bar item that, along with its delegate, provides a list of textual suggestions for the current text view.
see https://developer.apple.com/documentation/appkit/nscandidatelisttouchbaritem?language=objc"))

(define-objc-class "NSColorPickerTouchBarItem" ()
  ()
  (:documentation
   "A bar item that provides a system-defined color picker.
see https://developer.apple.com/documentation/appkit/nscolorpickertouchbaritem?language=objc"))

(define-objc-class "NSCustomTouchBarItem" ()
  ()
  (:documentation
   "A bar item that contains a responder of your choice, such as a view, a button, or a scrubber.
see https://developer.apple.com/documentation/appkit/nscustomtouchbaritem?language=objc"))

(define-objc-class "NSGroupTouchBarItem" ()
  ()
  (:documentation
   "A bar item that provides a bar to contain other items.
see https://developer.apple.com/documentation/appkit/nsgrouptouchbaritem?language=objc"))

(define-objc-class "NSPopoverTouchBarItem" ()
  ()
  (:documentation
   "A bar item that provides a two-state control that can expand into its second state, showing the contents of a bar that it owns.
see https://developer.apple.com/documentation/appkit/nspopovertouchbaritem?language=objc"))

(define-objc-class "NSSharingServicePickerTouchBarItem" ()
  ()
  (:documentation
   "A bar item that, along with its delegate, provides a list of objects eligible for sharing.
see https://developer.apple.com/documentation/appkit/nssharingservicepickertouchbaritem?language=objc"))

(define-objc-class "NSSliderTouchBarItem" ()
  ()
  (:documentation
   "A bar item that provides a slider control for choosing a value in a range.
see https://developer.apple.com/documentation/appkit/nsslidertouchbaritem?language=objc"))

(define-objc-class "NSStepperTouchBarItem" ()
  ()
  (:documentation
   "A bar item that provides a stepper control for incrementing or decrementing a value.
see https://developer.apple.com/documentation/appkit/nssteppertouchbaritem?language=objc"))

(define-objc-class "NSUserInterfaceCompressionOptions" ()
  ()
  (:documentation
   "An object that specifies how user interface elements resize themselves when space is constrained.
see https://developer.apple.com/documentation/appkit/nsuserinterfacecompressionoptions?language=objc"))

(define-objc-class "NSButtonTouchBarItem" ()
  ()
  (:documentation
   "A bar item that provides a button.
see https://developer.apple.com/documentation/appkit/nsbuttontouchbaritem?language=objc"))

(define-objc-class "NSPickerTouchBarItem" ()
  ()
  (:documentation
   "A bar item that provides a picker control with multiple options.
see https://developer.apple.com/documentation/appkit/nspickertouchbaritem?language=objc"))

;;; Scrubbers

(define-objc-class "NSScrubber" ()
  ()
  (:documentation
   "A customizable item picker control for the Touch Bar.
see https://developer.apple.com/documentation/appkit/nsscrubber?language=objc"))

;;; Scrubber items

(define-objc-class "NSScrubberItemView" ()
  ()
  (:documentation
   "An item at a specific index position in the scrubber.
see https://developer.apple.com/documentation/appkit/nsscrubberitemview?language=objc"))

(define-objc-class "NSScrubberArrangedView" ()
  ()
  (:documentation
   "An abstract base class for the views whose layout is managed by a scrubber.
see https://developer.apple.com/documentation/appkit/nsscrubberarrangedview?language=objc"))

(define-objc-class "NSScrubberImageItemView" ()
  ()
  (:documentation
   "A concrete view subclass for displaying images in a scrubber items.
see https://developer.apple.com/documentation/appkit/nsscrubberimageitemview?language=objc"))

(define-objc-class "NSScrubberSelectionStyle" ()
  ()
  (:documentation
   "An abstract class that provides decorative accessory views for selected and highlighted items within a scrubber control.
see https://developer.apple.com/documentation/appkit/nsscrubberselectionstyle?language=objc"))

(define-objc-class "NSScrubberSelectionView" ()
  ()
  (:documentation
   "An abstract base class for specifying the appearance of a highlighted or selected item in a scrubber.
see https://developer.apple.com/documentation/appkit/nsscrubberselectionview?language=objc"))

(define-objc-class "NSScrubberTextItemView" ()
  ()
  (:documentation
   "A concrete view subclass for displaying text for an item in a scrubber.
see https://developer.apple.com/documentation/appkit/nsscrubbertextitemview?language=objc"))

;;; Scrubber layouts

(define-objc-class "NSScrubberFlowLayout" ()
  ()
  (:documentation
   "A concrete layout object that arranges items end-to-end in a linear strip.
see https://developer.apple.com/documentation/appkit/nsscrubberflowlayout?language=objc"))

(define-objc-class "NSScrubberProportionalLayout" ()
  ()
  (:documentation
   "A concrete layout object that sizes each item to some fraction of the scrubber’s visible size.
see https://developer.apple.com/documentation/appkit/nsscrubberproportionallayout?language=objc"))

(define-objc-class "NSScrubberLayoutAttributes" ()
  ()
  (:documentation
   "The layout of a scrubber item.
see https://developer.apple.com/documentation/appkit/nsscrubberlayoutattributes?language=objc"))

(define-objc-class "NSScrubberLayout" ()
  ()
  (:documentation
   "An abstract class that describes the layout of items within a scrubber control.
see https://developer.apple.com/documentation/appkit/nsscrubberlayout?language=objc"))


;;;; Drag and Drop
;; Support the direct manipulation of your app’s content using drag and drop.
;; see https://developer.apple.com/documentation/appkit/drag-and-drop?language=objc

;;; Drag Sources

(define-objc-class "NSDraggingItem" ()
  ()
  (:documentation
   "A single dragged item within a dragging session.
see https://developer.apple.com/documentation/appkit/nsdraggingitem?language=objc"))

(define-objc-class "NSDraggingSession" ()
  ()
  (:documentation
   "The encapsulation of a drag-and-drop action that supports modification of the drag while in progress.
see https://developer.apple.com/documentation/appkit/nsdraggingsession?language=objc"))

(define-objc-class "NSDraggingImageComponent" ()
  ()
  (:documentation
   "A single object in a dragging item.
see https://developer.apple.com/documentation/appkit/nsdraggingimagecomponent?language=objc"))

;;; Drop Targets


;;;; Accessibility for AppKit
;; Make your AppKit apps accessible to everyone who uses macOS.
;; see https://developer.apple.com/documentation/appkit/accessibility-for-appkit?language=objc

;;; Essentials

;;; AppKit Elements

;;; Custom View Subclasses

;;; Custom Elements

(define-objc-class "NSAccessibilityElement" ()
  ()
  (:documentation
   "The basic infrastructure necessary for interacting with an assistive app.
see https://developer.apple.com/documentation/appkit/nsaccessibilityelement-swift.class?language=objc"))

;;; Accessibility Types


;;;; Images and PDF
;; Create and manage images, in bitmap, PDF, and other formats.
;; see https://developer.apple.com/documentation/appkit/images-and-pdf?language=objc

;;; Images

(define-objc-class "NSImage" ()
  (("size"
    :accessor size
    :documentation
    "The size of the image.

Defaults to {0.0, 0.0} if no size has been set and the size cannot be
determined from any of the receiver’s image representations. If the
size of the image hasn’t already been set when an image representation
is added, the size is taken from the image representation’s data. For
EPS images, the size is taken from the image’s bounding box. For TIFF
images, the size is taken from the ImageLength and ImageWidth
attributes.

Changing the size of an NSImage after it has been used effectively
resizes the image. Changing the size invalidates all its caches and
frees them. When the image is next composited, the selected
representation will draw itself in an offscreen window to recreate the
cache.

see https://developer.apple.com/documentation/appkit/nsimage/size?language=objc"))
  (:documentation
   "A high-level interface for manipulating image data.

You use instances of NSImage to load existing images, create new
images, and draw the resulting image data into your views. Although
you use this class predominantly for image-related operations, the
class itself knows little about the underlying image data. Instead, it
works in conjunction with one or more image representation objects
(subclasses of NSImageRep) to manage and render the image data. For
the most part, these interactions are transparent.

The class serves many purposes, providing support for the following
tasks:

+ Loading images stored on disk or at a specified URL.
+ Drawing images into a view or graphics context.
+ Providing the contents of a CALayer object.
+ Creating new images based on a series of captured drawing commands.
+ Producing versions of the image in a different format.

The NSImage class itself is capable of managing image data in a
variety of formats. The specific list of formats is dependent on the
version of the operating system but includes many standard formats
such as TIFF, JPEG, GIF, PNG, and PDF among others. AppKit manages
each format using a specific type of image representation object,
whose job is to manage the actual image data. You can get a list of
supported formats using the methods described in Determining Supported
Types of Images.

For more information about how to use image objects in your app, see
Cocoa Drawing Guide.  Using Images with Core Animation Layers

Although you can assign an NSImage object directly to the contents
property of a CALayer object, doing so may not always yield the best
results. Instead of using your image object, you can use the
layerContentsForContentsScale: method to obtain an object that you can
use for your layer’s contents. The image created by that method serves
as the contents of a layer, which also supports all of the layer’s
gravity modes. By contrast, the NSImage class supports only the
kCAGravityResize, kCAGravityResizeAspect, and
kCAGravityResizeAspectFill modes.

Before calling the layerContentsForContentsScale: method, use the
recommendedLayerContentsScale: method to get the recommended scale
factor for the resulting image. The code listing below shows a typical
example that uses the scale factor of a window’s backing store as the
desired scale factor. From that scale factor, the code gets the scale
factor for the specified image object and creates an object that you
assign to the layer. You might use this code for images that fit the
layer bounds precisely or for which you rely on the contentsGravity
property of the layer to position or scale the image.

see https://developer.apple.com/documentation/appkit/nsimage?language=objc"))

(defgeneric as-ns-image (object &key &allow-other-keys)
  (:documentation "Turn OBJECT as `ns-image'. ")
  (:method ((image ns-image) &key)
    image)
  (:method ((pathname pathname) &key)
    "Initializes and returns an image object using the specified file.
Return an initialized NSImage object or nil if the new object cannot
be initialized.

Parameter:
+ PATHNAME: pathname specifying the file with the desired image data.
  Relative paths must be relative to the current working directory.

This method initializes the image object lazily. It does not actually
open the specified file or create any image representations from its
data until an app attempts to draw the image or request information
about it.

The filename parameter should include the file extension that
identifies the type of the image data. The mechanism that actually
creates the image representation for filename looks for an NSImageRep
subclass that handles that data type from among those registered with
NSImage.

Because this method doesn’t actually create image representations for
the image data, your app should do error checking before attempting to
use the image; one way to do so is by accessing the valid property to
check whether the image can be drawn.

This method invokes setDataRetained: with an argument of true, thus
enabling it to hold onto its filename. When archiving an image created
with this method, only the image’s filename is written to the archive.

If the cached version of the image uses less memory than the original
image data, AppKit deletes the original data and uses the cached
image. (This can occur for images whose resolution is greater than 72
dpi.) If you resize the image by less than 50%, AppKit loads the data
in again from the file. If you expect to delete the file or change its
contents, use initWithContentsOfFile: instead.

see https://developer.apple.com/documentation/appkit/nsimage/init(byreferencingfile:)?language=objc"
    (let ((image (invoke (alloc 'ns-image)
                         "initByReferencingFile:"
                         (string-to-ns-string
                          (uiop:native-namestring pathname)))))
      (if image
          image
          (error "~A cannot be initialized as `ns-image'. " pathname))))
  (:method ((url ns-url) &key)
    "Initializes and returns an image object using the specified URL.
Return an initialized NSImage object.

Parameter:
+ URL: the `ns-url' identifying the image

This method initializes the image object lazily. It does not attempt
to retrieve the data from the specified URL or create any image
representations from that data until an app attempts to draw the image
or request information about it.

The url parameter should include a file extension that identifies the
type of the image data. The mechanism that actually creates the image
representation looks for an NSImageRep subclass that handles that data
type from among those registered with NSImage.

Because this method doesn’t actually create image representations for
the image data, your app should do error checking before attempting to
use the image; one way to do so is by accessing the valid property to
check whether the image can be drawn.

This method invokes setDataRetained: with an argument of true, thus
enabling it to hold onto its URL. When archiving an image created with
this method, only the image’s URL is written to the archive.

see https://developer.apple.com/documentation/appkit/nsimage/init(byreferencing:)?language=objc"
    (invoke (alloc 'ns-image) "initByReferencingURL:" url))
  (:method (default &key)
    "Treat DEFAULT as `ns-url'. "
    (as-ns-image (as-ns-url default))))

(define-objc-class "NSImageRep" ()
  ()
  (:documentation
   "A semiabstract superclass that provides subclasses that you use to draw an image from a particular type of source data.
see https://developer.apple.com/documentation/appkit/nsimagerep?language=objc"))

;;; Bitmap Formats

(define-objc-class "NSBitmapImageRep" ()
  ()
  (:documentation
   "An object that renders an image from bitmap data.
see https://developer.apple.com/documentation/appkit/nsbitmapimagerep?language=objc"))

(define-objc-class "NSCIImageRep" ()
  ()
  (:documentation
   "An object that can render an image from a Core Image object.
see https://developer.apple.com/documentation/appkit/nsciimagerep?language=objc"))

(define-objc-class "NSPICTImageRep" ()
  ()
  (:documentation
   "An object that renders an image from a PICT format data stream of version 1, version 2, and extended version 2.
see https://developer.apple.com/documentation/appkit/nspictimagerep?language=objc"))

;;; Vector Formats

(define-objc-class "NSPDFImageRep" ()
  ()
  (:documentation
   "An object that can render an image from a PDF format data stream.
see https://developer.apple.com/documentation/appkit/nspdfimagerep?language=objc"))

(define-objc-class "NSPDFInfo" ()
  ()
  (:documentation
   "An object that stores information associated with the creation of a PDF file, such as its URL, tag names, page orientation, and paper size.
see https://developer.apple.com/documentation/appkit/nspdfinfo?language=objc"))

(define-objc-class "NSEPSImageRep" ()
  ()
  (:documentation
   "An object that can render an image from encapsulated PostScript (EPS) code.
see https://developer.apple.com/documentation/appkit/nsepsimagerep?language=objc"))

;;; Custom Formats

(define-objc-class "NSCustomImageRep" ()
  ()
  (:documentation
   "An object that uses a delegate object to render an image from a custom format.
see https://developer.apple.com/documentation/appkit/nscustomimagerep?language=objc"))


;;;; Drawing
;; Draw shapes, images, and other content on the screen.
;; see https://developer.apple.com/documentation/appkit/drawing?language=objc

;;; Drawing Contexts

(define-objc-class "NSGraphicsContext" ()
  ()
  (:documentation
   "An object that represents a graphics context.
see https://developer.apple.com/documentation/appkit/nsgraphicscontext?language=objc"))

;;; Shapes and Paths

(define-objc-class "NSBezierPath" ()
  ()
  (:documentation
   "An object that can create paths using PostScript-style commands.
see https://developer.apple.com/documentation/appkit/nsbezierpath?language=objc"))

;;; Strings

(define-objc-class "NSStringDrawingContext" ()
  ()
  (:documentation
   "An object that manages metrics for drawing attributed strings.
see https://developer.apple.com/documentation/appkit/nsstringdrawingcontext?language=objc"))

;;; Gradients

(define-objc-class "NSGradient" ()
  ()
  (:documentation
   "An object that can draw gradient fill colors
see https://developer.apple.com/documentation/appkit/nsgradient?language=objc"))

;;; Shadows

(define-objc-class "NSShadow" ()
  ()
  (:documentation
   "An object you use to specify attributes to create and style a drop shadow during drawing operations.
see https://developer.apple.com/documentation/appkit/nsshadow?language=objc"))


;;;; Color
;; Represent colors using built-in or custom formats, and give users options for selecting
;; and applying colors.
;; see https://developer.apple.com/documentation/appkit/color?language=objc

;;; Colors

(define-objc-class "NSColor" ()
  (;; Retrieving individual components
   ("alphaComponent"
    :reader alpha-component
    :documentation
    "The alpha (opacity) component value of the color.")
   ("whiteComponent"
    :reader white-component
    :documentation
    "The white component value of the color.")
   ("redComponent"
    :reader red-component
    :documentation
    "The red component value of the color.")
   ("greenComponent"
    :reader green-component
    :documentation
    "The green component value of the color.")
   ("blueComponent"
    :reader blue-component
    :documentation
    "The blue component value of the color.")
   ("cyanComponent"
    :reader cyan-component
    :documentation
    "The cyan component value of the color.")
   ("magentaComponent"
    :reader magenta-component
    :documentation
    "The magenta component value of the color.")
   ("yellowComponent"
    :reader yellow-component
    :documentation
    "The yellow component value of the color.")
   ("blackComponent"
    :reader black-component
    :documentation
    "The black component value of the color.")
   ("hueComponent"
    :reader hue-component
    :documentation
    "The hue component value of the color.")
   ("saturationComponent"
    :reader saturation-component
    :documentation
    "The saturation component value of the color.")
   ("brightnessComponent"
    :reader brightness-component
    :documentation
    "The brightness component value of the color.")
   ("catalogNameComponent"
    :reader catalog-name-component
    :documentation
    "The catalog containing the color’s name.")
   ("localizedCatalogNameComponent"
    :reader localized-catalog-name-component
    :documentation
    "The localized version of the catalog name containing the color.")
   ("colorNameComponent"
    :reader color-name-component
    :documentation
    "The name of the color.")
   ("localizedColorNameComponent"
    :reader localized-color-name-component
    :documentation
    "The localized version of the color name."))
  (:documentation
   "An object that stores color data and sometimes opacity (alpha value).

Many methods in AppKit require you to specify color data using an
NSColor object; when drawing you use them to set the current fill and
stroke colors. Color objects are immutable and thread-safe. You can
create color objects in many ways:

+ Load colors from an asset catalog. Colors created from assets can
  adapt automatically to system appearance changes.

+ Use the semantic colors for custom UI elements, so that they match
  the appearance of other AppKit views; see UI element colors.

+ Use the adaptable system colors, such as systemBlueColor, when you
  want a specific tint that looks correct in both light and dark
  environments.

+ Create a color object from another object, such as a Core Graphics
  representation of a color, or a Core Image color.

+ Create a color from an NSImage object, and paint a repeating
  pattern instead of using a solid color.

+ Create a color by applying a transform to another NSColor
  object. For example, you might perform a blend operation between
  two colors, or you might create a color that represents the same
  color, but in a different color space.

+ Create custom colors using raw component values, and a variety of
  color spaces, when you need to represent user-specified colors.

For user-specified colors, you can also display a color panel and let
the user specify the color. For information about color panels, see
NSColorPanel.

Color and color spaces
A color object is typically represented internally as a Core Graphics
color (CGColorRef) in a Core Graphics color space
(CGColorSpaceRef). Colors can also be created in extended color
spaces:

    extendedSRGBColorSpace

    extendedGenericGamma22GrayColorSpace

When you need to worry about color spaces, use extended color spaces
as working color spaces. When you need to worry about representing
that color as closely as possible in a specific color space, convert
the color from the extended color space into the target color space.

When working in an extended color space, color values are not clamped
to fit inside the color gamut, meaning that component values may be
less than 0.0 or greater than 1.0. When displayed on an sRGB display,
such colors are outside the gamut and won’t render
accurately. However, extended color spaces are useful as working color
spaces when you want a pixel format and representation that other
color spaces can be easily converted into. For example, a color in the
Display P3 color space can convert to an extended sRGB format, even if
it isn’t within the sRGB color gamut. While some of the converted
color’s values are outside of the 0-1.0 range, the color renders
correctly when viewed on a device with a P3 display gamut.

It is a programmer error to access color components of a color space
that the NSColor object does not support. For example, you cannot
access the redComponent property and getRed:green:blue:alpha: method
on a color that uses the CMYK color space. Further, the getComponents:
method and numberOfComponents property work only in color spaces that
have individual components. As such, they return the components of
color objects as individual floating-point values regardless of
whether they’re based on NSColorSpace objects or named color
spaces. However, older component-fetching methods such as
getRed:green:blue:alpha: are effective only on color objects based on
named color spaces.

If you have a color object in an unknown color space and you want to
extract its components, convert the color object to a known color
space and then use the component accessor methods of that color space.

For design guidance, see Human Interface Guidelines > Color.

see https://developer.apple.com/documentation/appkit/nscolor?language=objc"))

(defun ns-color-srgba (red green blue &optional (alpha 1.0d0))
  "Creates a color object from the specified components in the sRGB
colorspace.

Parameters:
+ RED:   (0-1)
+ GREEN: (0-1)
+ BLUE:  (0-1)
+ ALPHA: (0-1)

see https://developer.apple.com/documentation/appkit/nscolor/init(srgbred:green:blue:alpha:)?language=objc"
  (declare (type double-float red green blue alpha))
  (the ns-color
    (invoke 'ns-color "colorWithSRGBRed:green:blue:alpha:"
            red green blue alpha)))

(defun ns-color-display-p3-rgba (red green blue &optional (alpha 1.0d0))
  "Creates a color object from the specified components in the Display
P3 color space.

Parameters:
+ RED:   (0-1)
+ GREEN: (0-1)
+ BLUE:  (0-1)
+ ALPHA: (0-1)

see https://developer.apple.com/documentation/appkit/nscolor/init(displayp3red:green:blue:alpha:)?language=objc"
  (declare (type double-float red green blue alpha))
  (the ns-color
    (invoke 'ns-color "colorWithDisplayP3Red:green:blue:alpha:"
            red green blue alpha)))

(defun ns-color-rgba (red green blue &optional (alpha 1.0d0))
  "Creates a color object with the specified red, green, blue, and
alpha channel values.

Parameters:
+ RED:   (0-1)
+ GREEN: (0-1)
+ BLUE:  (0-1)
+ ALPHA: (0-1)

This method accepts extended color component values. If the red,
green, blue, or alpha values are outside of the 0-1.0 range, the
method creates a color in the extended range color space. This method
is provided for easier reuse of code that uses UIColor in iOS.

Where possible, it is preferable to specify the colorspace explicitly
using the colorWithSRGBRed:green:blue:alpha: or
colorWithGenericGamma22White:alpha: method.

see https://developer.apple.com/documentation/appkit/nscolor/init(red:green:blue:alpha:)?language=objc"
  (declare (type double-float red green blue alpha))
  (the ns-color
    (invoke 'ns-color "colorWithRed:green:blue:alpha:"
            red green blue alpha)))

(defun ns-color-calibrated-rgba (red green blue &optional (alpha 1.0d0))
  "Creates a color object using the given opacity and RGB components.

Parameters:
+ RED:   (0-1)
+ GREEN: (0-1)
+ BLUE:  (0-1)
+ ALPHA: (0-1)

see https://developer.apple.com/documentation/appkit/nscolor/init(calibratedred:green:blue:alpha:)?language=objc"
  (declare (type double-float red green blue alpha))
  (the ns-color
    (invoke 'ns-color "colorWithCalibratedRed:green:blue:alpha:"
            red green blue alpha)))

(defun ns-color-device-rgba (red green blue &optional (alpha 1.0d0))
  "Creates a color object using the given opacity value and RGB components.

Parameters:
+ RED:   (0-1)
+ GREEN: (0-1)
+ BLUE:  (0-1)
+ ALPHA: (0-1)

see https://developer.apple.com/documentation/appkit/nscolor/init(devicered:green:blue:alpha:)?language=objc"
  (declare (type double-float red green blue alpha))
  (the ns-color
    (invoke 'ns-color "colorWithDeviceRed:green:blue:alpha:"
            red green blue alpha)))

(defun ns-color-calibrated-hsb (hue saturation brightness &optional (alpha 1.0d0))
  "Creates a color object using the given opacity and HSB color space
components.

Parameters:
+ HUE:        (0-1)
+ SATURATION: (0-1)
+ BRIGHTNESS: (0-1)

see https://developer.apple.com/documentation/appkit/nscolor/init(calibratedhue:saturation:brightness:alpha:)?language=objc"
  (declare (type double-float hue saturation brightness alpha))
  (the ns-color
    (invoke 'ns-color "colorWithCalibratedHue:saturation:brightness:alpha:"
            hue saturation brightness alpha)))

(defun ns-color-device-hsb (hue saturation brightness &optional (alpha 1.0d0))
  "Creates a color object using the given opacity value and HSB color
space components.

Parameters:
+ HUE:        (0-1)
+ SATURATION: (0-1)
+ BRIGHTNESS: (0-1)

see https://developer.apple.com/documentation/appkit/nscolor/init(devicehue:saturation:brightness:alpha:)?language=objc"
  (declare (type double-float hue saturation brightness alpha))
  (the ns-color
    (invoke 'ns-color "colorWithDeviceHue:saturation:brightness:alpha:"
            hue saturation brightness alpha)))

(defun ns-color-hsb (hue saturation brightness &optional (alpha 1.0d0))
  "Creates a color object with the specified hue, saturation,
brightness, and alpha channel values.

Parameters:
+ HUE:        (0-1)
+ SATURATION: (0-1)
+ BRIGHTNESS: (0-1)

see https://developer.apple.com/documentation/appkit/nscolor/init(hue:saturation:brightness:alpha:)?language=objc"
  (declare (type double-float hue saturation brightness alpha))
  (the ns-color
    (invoke 'ns-color "colorWithHue:saturation:brightness:alpha:"
            hue saturation brightness alpha)))

(defun ns-color-cmyk (cyan magenta yellow black &optional (alpha 1.0d0))
  "Creates a color object using the given opacity value and CMYK
components.

Parameters:
+ CYAN:    (0-1)
+ MAGENTA: (0-1)
+ YELLOW:  (0-1)
+ BLACK:   (0-1)

see https://developer.apple.com/documentation/appkit/nscolor/init(devicecyan:magenta:yellow:black:alpha:)?language=objc"
  (declare (type double-float cyan magenta yellow black alpha))
  (the ns-color
    (invoke 'ns-color "colorWithDeviceCyan:magenta:yellow:black:alpha:"
             cyan magenta yellow black alpha)))

(defun ns-color-white (white &optional (alpha 1.0d0))
  "Creates a color object with the specified brightness and alpha
channel values.

Parameters:
+ WHITE: (0-1)
+ ALPHA: (0-1)

This method accepts extended color component values. If the alpha or
white values are outside of the 0-1.0 range, the method creates a
color in the extended range or extendedGenericGamma22GrayColorSpace
color space that is compatible with the sRGB colorspace. This method
is provided for easier reuse of code that uses UIColor in iOS.

Where possible, it is preferable to specify the colorspace explicitly
using the colorWithSRGBRed:green:blue:alpha: or
colorWithGenericGamma22White:alpha: method.

see https://developer.apple.com/documentation/appkit/nscolor/init(white:alpha:)?language=objc"
  (declare (type double-float white alpha))
  (the ns-color
    (invoke 'ns-color "colorWithWhite:alpha:" white alpha)))

(defun ns-color-calibrated-white (white &optional (alpha 1.0d0))
  "Creates a color object using the given opacity and grayscale values.

Parameters:
+ WHITE: (0-1)
+ ALPHA: (0-1)

see https://developer.apple.com/documentation/appkit/nscolor/init(calibratedwhite:alpha:)?language=objc"
  (declare (type double-float white alpha))
  (the ns-color
    (invoke 'ns-color "colorWithCalibratedWhite:alpha:" white alpha)))

(defun ns-color-device-white (white &optional (alpha 1.0d0))
  "Creates a color object using the given opacity and grayscale values.

Parameters:
+ WHITE: (0-1)
+ ALPHA: (0-1)

see https://developer.apple.com/documentation/appkit/nscolor/init(devicewhite:alpha:)?language=objc"
  (declare (type real white alpha))
  (the ns-color (invoke 'ns-color "colorWithDeviceWhite:alpha:" white alpha)))

(defun ns-color-generic-gamma-22-white (white &optional (alpha 1.0d0))
  "Returns a color object with the specified white and alpha values in
the GenericGamma22 colorspace.

Parameters:
+ WHITE: (0-1)
+ ALPHA: (0-1)

see https://developer.apple.com/documentation/appkit/nscolor/init(genericgamma22white:alpha:)?language=objc"
  (declare (type real white alpha))
  (the ns-color (invoke 'ns-color "colorWithGenericGamma22White:alpha:" white alpha)))

(defun ns-color-hdr-rgba-linear (red green blue &optional (alpha 1d0) (linear 1d0))
  "Generates an HDR color in the extended sRGB colorspace by applying
an exposure to the SDR color defined by the red, green, and blue
components. The red, green, and blue components have a nominal range
of [0..1], linearExposure is a value >= 1. To produce an HDR color, we
process the given color in a linear color space, multiplying component
values by linearExposure . The produced color will have a
contentHeadroom equal to linearExposure. Each doubling of
linearExposure produces a color that is twice as bright.

Parameters:
+ RED:    (0-1)
+ GREEN:  (0-1)
+ BLUE:   (0-1)
+ ALPHA:  (0-1)
+ LINEAR: (>=1)

see https://developer.apple.com/documentation/appkit/nscolor/init(red:green:blue:alpha:linearexposure:)?language=objc"
  (declare (type double-float red green blue alpha)
           (type (double-float 1d0) linear))
  (the ns-color
    (invoke 'ns-color "colorWithRed:green:blue:alpha:linearExposure:"
            red green blue alpha linear)))

(defun ns-color-hdr-rgba-exposure (red green blue &optional (alpha 1d0) (exposure 1d0))
  "Generates an HDR color in the extended sRGB colorspace by applying
an exposure to the SDR color defined by the red, green, and blue
components. The red, green, and blue components have a nominal range
of [0..1], exposure is a value >= 0. To produce an HDR color, we
process the given color in a linear color space, multiplying component
values by 2^exposure. The produced color will have a contentHeadroom
equal to the linearized exposure value. Each whole value of exposure
produces a color that is twice as bright.

Parameters:
+ RED:    (0-1)
+ GREEN:  (0-1)
+ BLUE:   (0-1)
+ ALPHA:  (0-1)
+ LINEAR: (>=1)

see https://developer.apple.com/documentation/appkit/nscolor/init(red:green:blue:alpha:exposure:)?language=objc"
  (declare (type double-float red green blue alpha)
           (type (double-float 1d0) exposure))
  (the ns-color (invoke 'ns-color
                        "colorWithRed:green:blue:alpha:exposure:"
                        red green blue alpha exposure)))

(defun ns-color-pattern-image (image)
  "Creates a color object that uses the specified image pattern to
paint the target area.

Parameters:
+ IMAGE: `ns-image'
  The image to use as the pattern for the color object. The image is
  tiled starting at the bottom of the window. The image is not scaled.

see https://developer.apple.com/documentation/appkit/nscolor/init(patternimage:)?language=objc"
  (declare (type ns-image image))
  (the ns-color (invoke 'ns-color "colorWithPatternImage:" image)))

(defgeneric as-ns-color (object &key &allow-other-keys)
  (:documentation
   "Convert OBJECT into `ns-color' object.
Return `ns-color' element. ")
  (:method ((color null) &key)     (as-ns-color :clear))
  (:method ((color ns-color) &key) color)
  ;; UI element colors
  ;; Retrieve standard color objects for use with windows, controls,
  ;; labels, text, selections and other content in your app.
  ;; https://developer.apple.com/documentation/appkit/ui-element-colors?language=objc
  ;;
  ;; Label colors
  (:method ((color (eql :label)) &key)
    "The primary color to use for text labels.
see https://developer.apple.com/documentation/appkit/nscolor/labelcolor?language=objc"
    (invoke 'ns-color "labelColor"))
  (:method ((color (eql :secondary-label)) &key)
    "The secondary color to use for text labels.
see https://developer.apple.com/documentation/appkit/nscolor/secondarylabelcolor?language=objc"
    (invoke 'ns-color "secondaryLabelColor"))
  (:method ((color (eql :teriary-label)) &key)
    "The tertiary color to use for text labels.
see https://developer.apple.com/documentation/appkit/nscolor/tertiarylabelcolor?language=objc"
    (invoke 'ns-color "tertiaryLabelColor"))
  (:method ((color (eql :quaternary-label)) &key)
    "The quaternary color to use for text labels and separators.
see https://developer.apple.com/documentation/appkit/nscolor/quaternarylabelcolor?language=objc"
    (invoke 'ns-color "quaternaryLabelColor"))
  ;; Text colors
  (:method ((color (eql :text)) &key)
    "The color to use for text.
see https://developer.apple.com/documentation/appkit/nscolor/textcolor?language=objc"
    (invoke 'ns-color "textColor"))
  (:method ((color (eql :placeholder-text)) &key)
    "The color to use for placeholder text in controls or text views.
see https://developer.apple.com/documentation/appkit/nscolor/placeholdertextcolor?language=objc"
    (invoke 'ns-color "placeholderText"))
  (:method ((color (eql :selected-text)) &key)
    "The color to use for selected text.
see https://developer.apple.com/documentation/appkit/nscolor/selectedtextcolor?language=objc"
    (invoke 'ns-color "selectedTextColor"))
  (:method ((color (eql :text-background)) &key)
    "The color to use for the background area behind text.
see https://developer.apple.com/documentation/appkit/nscolor/textbackgroundcolor?language=objc"
    (invoke 'ns-color "textBackgroundColor"))
  (:method ((color (eql :selected-text-background)) &key)
    "The color to use for the background of selected text.
see https://developer.apple.com/documentation/appkit/nscolor/selectedtextbackgroundcolor?language=objc"
    (invoke 'ns-color "selectedTextBackgroundColor"))
  (:method ((color (eql :keyboard-focus-indicator)) &key)
    "The color to use for the keyboard focus ring around controls.
see https://developer.apple.com/documentation/appkit/nscolor/keyboardfocusindicatorcolor?language=objc"
    (invoke 'ns-color "keyboardFocusIndicatorColor"))
  (:method ((color (eql :unemphasized-selected-text)) &key)
    "The color to use for selected text in an unemphasized context.
see https://developer.apple.com/documentation/appkit/nscolor/unemphasizedselectedtextcolor?language=objc"
    (invoke 'ns-color "unemphasizedSelectedTextColor"))
  (:method ((color (eql :unemphasized-selected-text-background)) &key)
    "The color to use for the text background in an unemphasized context.
see https://developer.apple.com/documentation/appkit/nscolor/unemphasizedselectedtextbackgroundcolor?language=objc"
    (invoke 'ns-color "unemphasizedSelectedTextBackgroundColor"))
  ;; Content colors
  (:method ((color (eql :link)) &key)
    "The color to use for links.
see https://developer.apple.com/documentation/appkit/nscolor/linkcolor?language=objc"
    (invoke 'ns-color "linkColor"))
  (:method ((color (eql :separator)) &key)
    "The color to use for separators between different sections of content.
see https://developer.apple.com/documentation/appkit/nscolor/separatorcolor?language=objc"
    (invoke 'ns-color "separatorColor"))
  (:method ((color (eql :selected-content-background)) &key)
    "The color to use for the background of selected and emphasized content.
see https://developer.apple.com/documentation/appkit/nscolor/selectedcontentbackgroundcolor?language=objc"
    (invoke 'ns-color "selectedContentBackgroundColor"))
  (:method ((color (eql :unemphasized-selected-content-background)) &key)
    "The color to use for selected and unemphasized content.
see https://developer.apple.com/documentation/appkit/nscolor/unemphasizedselectedcontentbackgroundcolor?language=objc"
    (invoke 'ns-color "unemphasizedSelectedContentBackgroundColor"))
  ;; Menu colors
  (:method ((color (eql :selected-menu-item-text)) &key)
    "The color to use for the text in menu items.
see https://developer.apple.com/documentation/appkit/nscolor/selectedmenuitemtextcolor?language=objc"
    (invoke 'ns-color "selectedMenuItemTextColor"))
  ;; Table colors
  (:method ((color (eql :grid)) &key)
    "The color to use for the optional gridlines, such as those in a table view.
see https://developer.apple.com/documentation/appkit/nscolor/gridcolor?language=objc"
    (invoke 'ns-color "gridColor"))
  (:method ((color (eql :header-text)) &key)
    "The color to use for text in header cells in table views and outline views.
see https://developer.apple.com/documentation/appkit/nscolor/headertextcolor?language=objc"
    (invoke 'ns-color "headerTextColor"))
  (:method ((color (eql :alternating-content-background)) &key)
    "The colors to use for alternating content, typically found in
table views and collection views.
see https://developer.apple.com/documentation/appkit/nscolor/alternatingcontentbackgroundcolors?language=objc"
    (invoke 'ns-color "alternatingContentBackgroundColors"))
  ;; Control colors
  (:method ((color (eql :control-accent)) &key)
    "The user’s current accent color preference.
see https://developer.apple.com/documentation/appkit/nscolor/controlaccentcolor?language=objc"
    (invoke 'ns-color "controlAccentColor"))
  (:method ((color (eql :control)) &key)
    "The color to use for the flat surfaces of a control.
see https://developer.apple.com/documentation/appkit/nscolor/controlcolor?language=objc"
    (invoke 'ns-color "controlColor"))
  (:method ((color (eql :control-background)) &key)
    "The color to use for the background of large controls, such as
scroll views or table views.
see https://developer.apple.com/documentation/appkit/nscolor/controlbackgroundcolor?language=objc"
    (invoke 'ns-color "controlBackgroundColor"))
  (:method ((color (eql :control-text)) &key)
    "The color to use for text on enabled controls.
see https://developer.apple.com/documentation/appkit/nscolor/controltextcolor?language=objc"
    (invoke 'ns-color "controlTextColor"))
  (:method ((color (eql :disabled-control-text)) &key)
    "The color to use for text on disabled controls.
see https://developer.apple.com/documentation/appkit/nscolor/disabledcontroltextcolor?language=objc"
    (invoke 'ns-color "disabledControlTextColor"))
  (:method ((color (eql :current-control-tint)) &key)
    "The current system control tint color.
see https://developer.apple.com/documentation/appkit/nscolor/currentcontroltint?language=objc"
    (invoke 'ns-color "currentControlTint"))
  (:method ((color (eql :selected-control)) &key)
    "The color to use for the face of a selected control—that is, a
control that has been clicked or is being dragged.
see https://developer.apple.com/documentation/appkit/nscolor/selectedcontrolcolor?language=objc"
    (invoke 'ns-color "selectedControlColor"))
  (:method ((color (eql :selected-control-text)) &key)
    "The color to use for text in a selected control—that is, a
control being clicked or dragged.
see https://developer.apple.com/documentation/appkit/nscolor/selectedcontroltextcolor?language=objc"
    (invoke 'ns-color "selectedControlTextColor"))
  (:method ((color (eql :alternate-selected-control-text)) &key)
    "The color to use for text in a selected control."
    (invoke 'ns-color "alternateSelectedControlTextColor"))
  (:method ((color (eql :scrubber-textured-background)) &key)
    "The patterned color to use for the background of a scrubber control.
see https://developer.apple.com/documentation/appkit/nscolor/scrubbertexturedbackground?language=objc"
    (invoke 'ns-color "scrubberTexturedBackgroundColor"))
  ;; Window colors
  (:method ((color (eql :window-background)) &key)
    "The color to use for the window background.
see https://developer.apple.com/documentation/appkit/nscolor/windowbackgroundcolor?language=objc"
    (invoke 'ns-color "windowBackgroundColor"))
  (:method ((color (eql :window-frame-text)) &key)
    "The color to use for text in a window’s frame."
    (invoke 'ns-color "windowFrameTextColor"))
  (:method ((color (eql :under-page-background)) &key)
    "The color to use in the area beneath your window’s views."
    (invoke 'ns-color "underPageBackgroundColor"))
  ;; Highlights and shadows
  (:method ((color (eql :find-highlight)) &key)
    "The highlight color to use for the bubble that shows inline
search result values.
see https://developer.apple.com/documentation/appkit/nscolor/findhighlightcolor?language=objc"
    (invoke 'ns-color "findHighlightColor"))
  (:method ((color (eql :highlight-color)) &key)
    "The color to use as a virtual light source on the screen."
    (invoke 'ns-color "highlightColor"))
  (:method ((color (eql :shadow)) &key)
    "The color to use for virtual shadows cast by raised objects on the screen.
see https://developer.apple.com/documentation/appkit/nscolor/shadowcolor?language=objc"
    (invoke 'ns-color "shadowColor"))
  ;; Fill colors
  (:method ((color (eql :quaternary-system-fill)) &key)
    (invoke 'ns-color "quaternarySystemFillColor"))
  (:method ((color (eql :quinary-label)) &key)
    (invoke 'ns-color "quinaryLabelColor"))
  (:method ((color (eql :quinary-system-fill)) &key)
    (invoke 'ns-color "quinarySystemFillColor"))
  (:method ((color (eql :secondary-system-fill)) &key)
    (invoke 'ns-color "secondarySystemFillColor"))
  (:method ((color (eql :system-fill)) &key)
    (invoke 'ns-color "systemFillColor"))
  (:method ((color (eql :teriary-system-fill)) &key)
    (invoke 'ns-color "tertiarySystemFillColor"))
  (:method ((color (eql :text-insertion-point)) &key)
    (invoke 'ns-color "textInsertionPointColor"))

  ;; Standard colors
  ;; Retrieve the standard color objects for common colors like red,
  ;; blue, green, black, white, and more.
  ;;
  ;; For design guidance, see Human Interface Guidelines.
  ;; https://developer.apple.com/design/human-interface-guidelines/macos/visual-design/color#system-colors
  ;;
  ;; see https://developer.apple.com/documentation/appkit/standard-colors?language=objc

  ;; Adaptable system colors
  (:method ((color (eql :system-blue)) &key)
    "Returns a color object for blue that automatically adapts to
vibrancy and accessibility settings.
see https://developer.apple.com/documentation/appkit/nscolor/systemblue?language=objc"
    (invoke 'ns-color "systemBlueColor"))
  (:method ((color (eql :system-brown)) &key)
    "Returns a color object for brown that automatically adapts to
vibrancy and accessibility settings.
see https://developer.apple.com/documentation/appkit/nscolor/systembrown?language=objc"
    (invoke 'ns-color "systemBrownColor"))
  (:method ((color (eql :system-cyan)) &key)
    "Returns a color object for cyan that automatically adapts to
vibrancy and accessibility settings.
see "
    (invoke 'ns-color "systemCyanColor"))
  (:method ((color (eql :system-gray)) &key)
    "Returns a color object for gray that automatically adapts to
vibrancy and accessibility settings.
see "
    (invoke 'ns-color "systemGrayColor"))
  (:method ((color (eql :system-green)) &key)
    "Returns a color object for green that automatically adapts to
vibrancy and accessibility settings.
see "
    (invoke 'ns-color "systemGreenColor"))
  (:method ((color (eql :system-indigo-color)) &key)
    "Returns a color object for indigo that automatically adapts to
vibrancy and accessibility settings.
see "
    (invoke 'ns-color "systemIndigoColor"))
  (:method ((color (eql :system-mint)) &key)
    "Returns a color object for mint that automatically adapts to
vibrancy and accessibility settings.
see "
    (invoke 'ns-color "systemMintColor"))
  (:method ((color (eql :system-orange)) &key)
    "Returns a color object for orange that automatically adapts to
vibrancy and accessibility settings.
see "
    (invoke 'ns-color "systemOrangeColor"))
  (:method ((color (eql :system-pink)) &key)
    "Returns a color object for pink that automatically adapts to
vibrancy and accessibility settings.
see "
    (invoke 'ns-color "systemPinkColor"))
  (:method ((color (eql :system-purple)) &key)
    "Returns a color object for purple that automatically adapts to
vibrancy and accessibility settings.
see "
    (invoke 'ns-color "systemPurpleColor"))
  (:method ((color (eql :system-red)) &key)
    "Returns a color object for red that automatically adapts to
vibrancy and accessibility settings.
see "
    (invoke 'ns-color "systemRedColor"))
  (:method ((color (eql :system-teal)) &key)
    "Returns a color object for teal that automatically adapts to
vibrancy and accessibility settings.
see "
    (invoke 'ns-color "systemTealColor"))
  (:method ((color (eql :system-yellow)) &key)
    "Returns a color object for yellow that automatically adapts to
vibrancy and accessibility settings.
see "
    (invoke 'ns-color "systemYellowColor"))
  ;; Transparent color
  (:method ((color (eql :clear)) &key)
    "Returns a color object whose grayscale and alpha values are both
0.0.
see "
    (invoke 'ns-color "clearColor"))
  ;; Fixed colors
  (:method ((color (eql :black)) &key)
    "Returns a color object whose grayscale value is 0.0 and whose
alpha value is 1.0."
    (invoke 'ns-color "blackColor"))
  (:method ((color (eql :blue)) &key)
    "Returns a color object whose RGB value is 0.0, 0.0, 1.0 and whose
alpha value is 1.0."
    (invoke 'ns-color "blueColor"))
  (:method ((color (eql :brown)) &key)
    "Returns a color object whose RGB value is 0.6, 0.4, 0.2 and whose
alpha value is 1.0."
    (invoke 'ns-color "brownColor"))
  (:method ((color (eql :cyan)) &key)
    "Returns a color object whose RGB value is 0.0, 1.0, 1.0 and whose
alpha value is 1.0."
    (invoke 'ns-color "cyanColor"))
  (:method ((color (eql :dark-gray)) &key)
    "Returns a color object whose grayscale value is 1/3 and whose
alpha value is 1.0."
    (invoke 'ns-color "darkGrayColor"))
  (:method ((color (eql :gray)) &key)
    "Returns a color object whose grayscale value is 0.5 and whose
alpha value is 1.0."
    (invoke 'ns-color "grayColor"))
  (:method ((color (eql :green)) &key)
    "Returns a color object whose RGB value is 0.0, 1.0, 0.0 and whose
alpha value is 1.0."
    (invoke 'ns-color "greenColor"))
  (:method ((color (eql :light-gray)) &key)
    "Returns a color object whose grayscale value is 2/3 and whose
alpha value is 1.0."
    (invoke 'ns-color "lightGrayColor"))
  (:method ((color (eql :magenta)) &key)
    "Returns a color object whose RGB value is 1.0, 0.0, 1.0 and whose
alpha value is 1.0."
    (invoke 'ns-color "magentaColor"))
  (:method ((color (eql :orange)) &key)
    "Returns a color object whose RGB value is 1.0, 0.5, 0.0 and whose
alpha value is 1.0."
    (invoke 'ns-color "orangeColor"))
  (:method ((color (eql :purple)) &key)
    "Returns a color object whose RGB value is 0.5, 0.0, 0.5 and whose
alpha value is 1.0.  "
    (invoke 'ns-color "purpleColor"))
  (:method ((color (eql :red)) &key)
    "Returns a color object whose RGB value is 1.0, 0.0, 0.0 and whose
alpha value is 1.0."
    (invoke 'ns-color "redColor"))
  (:method ((color (eql :white)) &key)
    "Returns a color object whose grayscale and alpha values are both
1.0."
    (invoke 'ns-color "whiteColor"))
  (:method ((color (eql :yellow)) &key)
    "Returns a color object whose RGB value is 1.0, 1.0, 0.0 and whose
alpha value is 1.0."
    (invoke 'ns-color "yellowColor"))

  ;; Color creation
  ;; Load colors from asset catalogs, and create colors from raw
  ;; component values, such as those used by grayscale, RGB, HSB, and
  ;; CMYK colors.
  ;; see https://developer.apple.com/documentation/appkit/color-creation?language=objc

  ;; Loading color objects from asset catalogs
  (:method ((name ns-string) &key catalog bundle)
    "Creates a color object from the provided name, which corresponds
to a color in the default asset catalog of the app’s main bundle.

Parameters:
+ NAME:    name of the color in the asset catalog.
+ CATALOG: name of the asset catalog in which to find the specified color
  this may be a standard color catalog. (see `as-ns-string')
+ BUNDLE:  the app bundle (see `as-ns-bundle')

Dev Note:
+ if given CATALOG: invoke colorWithCatalogName:colorName:
+ if given BUNDLE:  invoke colorNamed:bundle:
+ if given both CATALOG and BUNDLE, raise error
"
    (declare (type (or null string ns-string) catalog)
             (type (or null ns-bundle)        bundle))
    (cond ((and catalog bundle)
           (error "Cannot setting CATALOG and BUNDLE at same time. "))
          (catalog
           (invoke 'ns-color
                   "colorWithCatalogName:colorName:"
                   catalog
                   name))
          (bundle
           (invoke 'ns-color
                   "colorNamed:bundle:"
                   name
                   bundle))
          (t (invoke 'ns-color "colorNamed:" name))))
  (:method ((name string) &key catalog bundle)
    "See (as-ns-color ns-string &key catalog bundle)"
    (as-ns-color (string-to-ns-string name) :catalog catalog :bundle bundle))

  ;; Creating a color using RGB components
  ;; Creating a color using HSB components
  ;; Creating a color using CMYK components
  ;; Creating a color using white components
  ;; Creating a high dynamic range (HDR) color
  ;; Creating a pattern-based color
  ;; Creating a color dynamically
  ;; Creating a color in an arbitrary color space
  ;; Creating a system tint color
  ;; Converting other types of color objects
  ;; Creating color objects
  (:method ((args sequence) &key (color-space :rgba))
    "COLOR-SPACE could be `:rgb' `:rgba' `:cmyk' `:white', `:hsb' or
function to apply ARGS (default :rgba).

see
+ `ns-color-srgba' (:srgba :srgb)
+ `ns-color-rgba'  (:rgba :rgb)
+ `ns-color-display-p3-rgba'
+ `ns-color-calibrated-rgba'
+ `ns-color-device-rgba'
+ `ns-color-hsb'   (:hsb)
+ `ns-color-cmyk'  (:cmyk)
+ `ns-color-white' (:white)
+ `ns-color-calibrated-white'
+ `ns-color-device-white'
+ `ns-color-generic-gamma-22-white'
+ `ns-color-hdr-rgba-linear'
+ `ns-color-hdr-rgba-exposure'
"
    (apply (etypecase color-space
             (function color-space)
             (keyword (ecase color-space
                        ((:rgba  :rgb)  #'ns-color-rgba)
                        ((:srgba :srgb) #'ns-color-srgba)
                        (:hsb           #'ns-color-hsb)
                        (:cmyk          #'ns-color-cmyk)
                        (:white         #'ns-color-white))))
           (map 'list (lambda (val) (coerce val 'double-float)) args)))
  (:method ((white real) &key)
    "ns-color-white"
    (ns-color-white (coerce white 'double-float)))
  (:method ((image ns-image) &key)
    "ns-color-pattern-image"
    (ns-color-pattern-image image)))

(define-objc-class "NSColorList" ()
  ()
  (:documentation
   "An ordered list of color objects, identified by keys.
see https://developer.apple.com/documentation/appkit/nscolorlist?language=objc"))

(define-objc-class "NSColorSpace" ()
  ()
  (:documentation
   "An object that represents a custom color space.
see https://developer.apple.com/documentation/appkit/nscolorspace?language=objc"))

;;; Color Selection

(define-objc-class "NSColorPicker" ()
  ()
  (:documentation
   "An abstract superclass that implements the default color picking protocol.
see https://developer.apple.com/documentation/appkit/nscolorpicker?language=objc"))

(define-objc-class "NSColorWell" ()
  ()
  (:documentation
   "A control that displays a color value and lets the user change that color value.
see https://developer.apple.com/documentation/appkit/nscolorwell?language=objc"))

(define-objc-class "NSColorPickerTouchBarItem" ()
  ()
  (:documentation
   "A bar item that provides a system-defined color picker.
see https://developer.apple.com/documentation/appkit/nscolorpickertouchbaritem?language=objc"))

;;; Color Sampler

(define-objc-class "NSColorSampler" ()
  ()
  (:documentation
   "An object that displays the system’s color-sampling interface and
returns the selected color to your app.
see https://developer.apple.com/documentation/appkit/nscolorsampler?language=objc"))


;;;; Printing
;; Display the system print panels and manage the printing process.
;; see https://developer.apple.com/documentation/appkit/printing?language=objc

;;; Print Panels

(define-objc-class "NSPrintPanel" ()
  ()
  (:documentation
   "The Print panel that queries the user for information about a print job.
see https://developer.apple.com/documentation/appkit/nsprintpanel?language=objc"))

(define-objc-class "NSPageLayout" ()
  ()
  (:documentation
   "A panel that queries the user for information such as paper type and orientation.
see https://developer.apple.com/documentation/appkit/nspagelayout?language=objc"))

;;; Print Information

(define-objc-class "NSPrinter" ()
  ()
  (:documentation
   "An object that describes a printer’s capabilities.
see https://developer.apple.com/documentation/appkit/nsprinter?language=objc"))

(define-objc-class "NSPrintInfo" ()
  ()
  (:documentation
   "An object that stores information that’s used to generate printed output.
see https://developer.apple.com/documentation/appkit/nsprintinfo?language=objc"))

(define-objc-class "NSPrintOperation" ()
  ()
  (:documentation
   "An object that controls operations that generate Encapsulated PostScript (EPS) code, Portable Document Format (PDF) code, or print jobs.
see https://developer.apple.com/documentation/appkit/nsprintoperation?language=objc"))


;;;; Text Display
;; Display text and check spelling.
;; see https://developer.apple.com/documentation/appkit/text-display?language=objc

;;; Text views

(define-objc-mask ns-line-break-strategy
  "Constants that specify how the text system breaks lines while laying out paragraphs.
see https://developer.apple.com/documentation/appkit/nsparagraphstyle/linebreakstrategy-swift.struct?language=objc"
  (:push-out              1
                          "The text system pushes out individual lines to avoid an orphan"
                          "word on the last line of the paragraph.")
  (:hangul-word-priority  2
                          "The text system prohibits breaking between Hangul characters.")
  (:standard              65525
                          "The text system uses the same configuration of line-break"
                          "strategies that it uses for standard UI labels.")
  (:none                  0
                          "The text system doesn’t use any line-break strategies."))

(define-objc-enum ns-text-field-bezel-style
  "The style of bezel the text field displays.
see https://developer.apple.com/documentation/appkit/nstextfield/bezelstyle-swift.enum?language=objc"
  (:square  0 "A style that draws a bezel with square corners around a text field.")
  (:rounded 1 "A style that draws a bezel with rounded corners around a single-line text field."))

(define-objc-class "NSTextField" ()
  (;; Controlling Selection and Editing
   ("selectable"
    :accessor selectablep
    :documentation
    "A Boolean value that determines whether the user can select the
content of the text field.

If `t', the text field becomes selectable but not editable.
Use `editable' to make the text field selectable and editable.

If `nil', the text is neither editable nor selectable.

see https://developer.apple.com/documentation/appkit/nstextfield/isselectable?language=objc")
   ("editable"
    :accessor editablep
    :before   as-boolean
    :documentation
    "A Boolean value that controls whether the user can edit the value
in the text field.

If `t', the user can select and edit text.

If `nil', the user can’t edit text, and the ability to select the text
field’s content is dependent on the value of selectable.

For example, if an NSTextField object is selectable but uneditable,
becomes editable for a time, and then becomes uneditable again, it
remains selectable. To ensure that text is neither editable nor
selectable, use `selectable' to disable text selection.

see https://developer.apple.com/documentation/appkit/nstextfield/iseditable?language=objc")
   ;; Controlling Rich Text Behavior
   ("allowsEditingTextAttributes"
    :accessor allows-editing-text-attributes-p
    :documentation
    "A Boolean value that controls whether the user can change font
attributes of the text field’s string.

If `t' and the text value is an attributed string, the text field’s
content displays using the attributed string’s visual settings. The
user can modify the text field’s style attributes in the font panel.

If `nil' and the text is an attributed string, the text field ignores
style attributes, such as font and color. The text field’s content
displays according to the text field’s settings. The text field
ignores changes to the attributed string’s attributes when displaying
the string and when the text field is in edit mode.

see https://developer.apple.com/documentation/appkit/nstextfield/allowseditingtextattributes?language=objc")
   ("importsGraphics"
    :accessor imports-graphics-p
    :documentation
    "A Boolean value that controls whether the user can drag image
files into the text field.

see https://developer.apple.com/documentation/appkit/nstextfield/importsgraphics?language=objc")
   ;; Setting Placeholder Text
   ("placeholderString"
    :accessor placeholder-string
    :documentation
    "The string the text field displays when empty to help the user
understand the text field’s purpose.
see https://developer.apple.com/documentation/appkit/nstextfield/placeholderstring?language=objc")
   ("placeholderAttributedString"
    :accessor placeholder-attributed-string
    :documentation
    "The attributed string the text field displays when empty to help
the user understand the text field’s purpose.
see https://developer.apple.com/documentation/appkit/nstextfield/placeholderattributedstring?language=objc")
   ;; Configuring Line Wrapping
   ("lineBreakStrategy"
    :accessor line-break-strategy
    :before   ns-line-break-strategy
    :after    decode-ns-line-break-strategy
    :documentation
    "The strategy that the system uses to break lines when laying out
multiple lines of text.

The default value for editable text fields is `:none' to match the
field editor’s behavior. The default value for selectable, uneditable
text fields is `:standard'.

Note
When the text field has an attributed string value, the system ignores
the `text-color', `font', `alignment', `line-break-mode', and
`line-break-strategy' properties. Set the `foreground-color', `font',
`alignment', `line-break-mode', and `line-break-strategy' properties
in the attributed string instead.

see https://developer.apple.com/documentation/appkit/nstextfield/linebreakstrategy?language=objc")
   ("allowsDefaultTighteningForTruncation"
    :accessor allows-default-tightening-for-truncation-p
    :before   as-boolean
    :documentation
    "A Boolean value that controls whether single-line text fields
tighten intercharacter spacing before truncating the text.
see https://developer.apple.com/documentation/appkit/nstextfield/allowsdefaulttighteningfortruncation?language=objc")
   ("maximumNumberOfLines"
    :accessor maximum-number-of-lines
    :documentation
    "The maximum number of lines a wrapping text field displays before
clipping or truncating the text.

see https://developer.apple.com/documentation/appkit/nstextfield/maximumnumberoflines?language=objc")
   ;; Sizing with Auto Layout
   ("preferredMaxLayoutWidth"
    :accessor preferred-max-layout-width
    :documentation
    "The maximum width of the text field’s intrinsic content size.

see https://developer.apple.com/documentation/appkit/nstextfield/preferredmaxlayoutwidth?language=objc")
   ;; Setting the Text Color
   ("textColor"
    :accessor text-color
    :before   as-ns-color
    :documentation
    "The color of the text field’s content.
see https://developer.apple.com/documentation/appkit/nstextfield/textcolor?language=objc")
   ;; Controlling the Background
   ("backgroundColor"
    :accessor background-color
    :documentation
    "The color of the background the text field’s cell draws behind the text.
see https://developer.apple.com/documentation/appkit/nstextfield/backgroundcolor?language=objc")
   ("drawsBackground"
    :accessor draws-background-p
    :before   as-boolean
    :documentation
    "A Boolean value that controls whether the text field’s cell draws
a background color behind the text.

see https://developer.apple.com/documentation/appkit/nstextfield/drawsbackground?language=objc")
   ("bezeled"
    :accessor bezeledp
    :before   as-boolean
    :documentation
    "A Boolean value that controls whether the text field draws a
bezeled background around its contents.

see https://developer.apple.com/documentation/appkit/nstextfield/isbezeled?language=objc")
   ("bezelStyle"
    :accessor bezel-style
    :before   ns-text-field-bezel-style
    :after    decode-ns-text-field-bezel-style
    :documentation
    "The text field’s bezel style, `:square' or `:rounded'.

To set the bezel style, you must have already set the the text field’s
`bezeled' method with an argument of true. For a list of bezel styles,
see `ns-text-field-bezel-style'.

see https://developer.apple.com/documentation/appkit/nstextfieldcell/bezelstyle?language=objc")
   ;; Setting a Border
   ("bordered"
    :accessor borderedp
    :before   as-boolean
    :documentation
    "A Boolean value that controls whether the text field draws a
solid black border around its contents.

see https://developer.apple.com/documentation/appkit/nstextfield/isbordered?language=objc")
   ;; Working with the Responder Chain
   ;; Using Keyboard Interface Control
   ;; Supporting Text Completion and Suggestions
   )
  (:documentation
   "Text the user can select or edit to send an action message to a target
when the user presses the Return key.

The NSTextField class uses the NSTextFieldCell class to implement its
user interface. Text fields display text either as a static label or
as an editable input field. The content of a text field is either
plain text or a rich-text attributed string. Text fields also support
line wrapping to display multiline text, and a variety of truncation
styles if the content doesn’t fit the available space.

The parent class, NSControl, provides the methods for setting the
values of the text field, such as stringValue and doubleValue. There
are corresponding methods to retrieve values.

In macOS 12 and later, if you explicitly call the layoutManager
property on your text field, the framework will revert to a
compatibility mode that uses NSLayoutManager. The text view also
switches to this compatibility mode when it encounters text content
that’s not yet supported.

see https://developer.apple.com/documentation/appkit/nstextfield?language=objc"))

(defmethod init ((field ns-text-field)
                 &key
                 frame
                 text
                 (draws-background-p nil draws-background?)
                 (background-color   nil background-color?)
                 (text-color         nil text-color?)
                 (editablep          nil editable?)
                 (selectablep        nil selectable?)
                 (font               nil font?)
                 (bezeledp           nil bezeled?)
                 &allow-other-keys)
  "Initialize NSTextField.

Parameters:
+ FRAME: `ns-rect'
  The frame rectangle for the created view object.
  If setting, will use initWithFrame: method to initialize the
  `ns-text-field'.
+ TEXT:
  The string value of `ns-text-field' (see `string-value')

Styles Parameters:
+ DRAWS-BACKGROUND-P:
  if or not draws background (see `draws-background-p')
+ BACKGROUND-COLOR:
  set the text field background color (see `background-color')
  if setting, will force DRAWS-BACKGROUND-P as `t'
+ TEXT-COLOR:  set the text color (see `text-color')
+ EDITABLEP:   set the text field is editable or not (see `editablep')
+ SELECTABLEP: set the text field is selectable or not (see `selectablep')
+ FONT:        set the text field (see `as-ns-font')
+ BEZELEDP:    set if draws bezeled background (boarder) (see `bezeledp')
"
  (declare (type (or null ns-rect) frame))
  ;; TODO: initWithFrame: should be moved to NSControl init method
  (cond (frame (invoke field "initWithFrame:" frame)
               (when text (setf (string-value field) text)))
        (t     (error "Does not know how to init ~S. "
                      (class-name (class-of field)))))
  (if background-color?
      ;; Ignore `draws-background-p'
      (if (null background-color)
          (setf (draws-background-p field) nil)
          (setf (draws-background-p field) t
                (background-color   field) background-color))
      ;; if not setting `background-color',
      ;; `draws-background-p' will modify the `text' attributes
      (when draws-background?
        (setf (draws-background-p field) draws-background-p)))
  (when bezeled?    (setf (bezeledp    field) bezeledp))
  (when text-color? (setf (text-color  field) text-color))
  (when editable?   (setf (editablep   field) editablep))
  (when selectable? (setf (selectablep field) selectablep))
  (when font?       (setf (font        field) font)))

(define-objc-class "NSTextView" ()
  ()
  (:documentation
   "A view that draws text and handles user interactions with that text.
see https://developer.apple.com/documentation/appkit/nstextview?language=objc"))

(define-objc-class "NSText" ()
  ()
  (:documentation
   "The most general programmatic interface for objects that manage text.
see https://developer.apple.com/documentation/appkit/nstext?language=objc"))

;;; Text input

(define-objc-class "NSTextInputContext" ()
  ()
  (:documentation
   "An object that represents the Cocoa text input system.
see https://developer.apple.com/documentation/appkit/nstextinputcontext?language=objc"))

(define-objc-class "NSTextInsertionIndicator" ()
  ()
  (:documentation
   "A view that represents the insertion indicator in text.
see https://developer.apple.com/documentation/appkit/nstextinsertionindicator?language=objc"))

;;; Text-checking

;;; Spell-checking

(define-objc-class "NSSpellChecker" ()
  ()
  (:documentation
   "An interface to the Cocoa spell-checking service.
see https://developer.apple.com/documentation/appkit/nsspellchecker?language=objc"))

;;; Deprecated


;;;; TextKit
;; Manage text storage and perform custom layout of text-based content in your app’s views.
;; see https://developer.apple.com/documentation/appkit/textkit?language=objc

;;; Text management

(define-objc-class "NSTextContentStorage" ()
  ()
  (:documentation
   "A concrete object for managing your view’s text content and generating the
text elements necessary for layout.
see https://developer.apple.com/documentation/appkit/nstextcontentstorage?language=objc"))

(define-objc-class "NSTextContentManager" ()
  ()
  (:documentation
   "An abstract class that defines the interface and a default implementation for managing the text document contents.
see https://developer.apple.com/documentation/appkit/nstextcontentmanager?language=objc"))

(define-objc-class "NSAttributedString" ()
  ()
  (:documentation
   "A string of text that manages data, layout, and stylistic information for ranges of characters to support rendering.
see https://developer.apple.com/documentation/Foundation/NSAttributedString?language=objc"))

(define-objc-class "NSMutableAttributedString" ()
  ()
  (:documentation
   "A mutable string with associated attributes (such as visual style, hyperlinks, or accessibility data) for portions of its text.
see https://developer.apple.com/documentation/Foundation/NSMutableAttributedString?language=objc"))

;;; Formatting and attributes

(define-objc-class "NSParagraphStyle" ()
  ()
  (:documentation
   "see https://developer.apple.com/documentation/appkit/nsparagraphstyle?language=objc"))

(define-objc-class "NSMutableParagraphStyle" ()
  ()
  (:documentation
   "An object for changing the values of the subattributes in a paragraph style attribute.
see https://developer.apple.com/documentation/appkit/nsmutableparagraphstyle?language=objc"))

(define-objc-class "NSTextTab" ()
  ()
  (:documentation
   "A tab in a paragraph.
see https://developer.apple.com/documentation/appkit/nstexttab?language=objc"))

(define-objc-class "NSTextList" ()
  ()
  (:documentation
   "A section of text that forms a single list.
see https://developer.apple.com/documentation/appkit/nstextlist?language=objc"))

(define-objc-class "NSTextTable" ()
  ()
  (:documentation
   "An object that represents a text table as a whole.
see https://developer.apple.com/documentation/appkit/nstexttable?language=objc"))

(define-objc-class "NSTextTableBlock" ()
  ()
  (:documentation
   "A text block that appears as a cell in a text table.
see https://developer.apple.com/documentation/appkit/nstexttableblock?language=objc"))

(define-objc-class "NSTextBlock" ()
  ()
  (:documentation
   "A block of text laid out in a subregion of the text container.
see https://developer.apple.com/documentation/appkit/nstextblock?language=objc"))

;;; Content elements

(define-objc-class "NSTextParagraph" ()
  ()
  (:documentation
   "A class that represents a single paragraph backed by an attributed string as the contents.
see https://developer.apple.com/documentation/appkit/nstextparagraph?language=objc"))

(define-objc-class "NSTextListElement" ()
  ()
  (:documentation
   "A class that represents a text list node.
see https://developer.apple.com/documentation/appkit/nstextlistelement?language=objc"))

(define-objc-class "NSTextElement" ()
  ()
  (:documentation
   "An abstract base class that represents the smallest units of text layout such as paragraphs or attachments.
see https://developer.apple.com/documentation/appkit/nstextelement?language=objc"))

;;; Location and selection

(define-objc-class "NSTextRange" ()
  ()
  (:documentation
   "A class that represents a contiguous range between two locations inside document contents.
see https://developer.apple.com/documentation/appkit/nstextrange?language=objc"))

(define-objc-class "NSTextSelection" ()
  ()
  (:documentation
   "A class that represents a single logical selection context that corresponds to an insertion point.
see https://developer.apple.com/documentation/appkit/nstextselection?language=objc"))

(define-objc-class "NSTextSelectionNavigation" ()
  ()
  (:documentation
   "An interface you use to expose methods for obtaining results from actions performed on text selections.
see https://developer.apple.com/documentation/appkit/nstextselectionnavigation?language=objc"))

;;; Layout

(define-objc-class "NSTextLayoutManager" ()
  ()
  (:documentation
   "The primary class that you use to manage text layout and presentation for custom text displays.
see https://developer.apple.com/documentation/appkit/nstextlayoutmanager?language=objc"))

(define-objc-class "NSTextContainer" ()
  ()
  (:documentation
   "A region where text layout occurs.
see https://developer.apple.com/documentation/appkit/nstextcontainer?language=objc"))

(define-objc-class "NSTextLayoutFragment" ()
  ()
  (:documentation
   "A class that represents the layout fragment typically corresponding to a rendering surface, such as a layer or view subclass.
see https://developer.apple.com/documentation/appkit/nstextlayoutfragment?language=objc"))

(define-objc-class "NSTextLineFragment" ()
  ()
  (:documentation
   "A class that represents a line fragment as a single textual layout and rendering unit inside a text layout fragment.
see https://developer.apple.com/documentation/appkit/nstextlinefragment?language=objc"))

(define-objc-class "NSTextViewportLayoutController" ()
  ()
  (:documentation
   "Manages the layout process inside the viewport interacting with its delegate.
see https://developer.apple.com/documentation/appkit/nstextviewportlayoutcontroller?language=objc"))

;;; Attachments

(define-objc-class "NSTextAttachment" ()
  ()
  (:documentation
   "The values for the attachment characteristics of attributed strings and related objects.
see https://developer.apple.com/documentation/appkit/nstextattachment?language=objc"))

(define-objc-class "NSTextAttachmentViewProvider" ()
  ()
  (:documentation
   "A container object that associates a text attachment at a particular document location with a view object.
see https://developer.apple.com/documentation/appkit/nstextattachmentviewprovider?language=objc"))

(define-objc-class "NSAdaptiveImageGlyph" ()
  ()
  (:documentation
   "A data object for an emoji-like image that can appear in attributed text.
see https://developer.apple.com/documentation/appkit/nsadaptiveimageglyph?language=objc"))

(define-objc-class "NSCell" ()
  ()
  (:documentation
   "A mechanism for displaying text or images in a view object
without the overhead of a full `ns-view' subclass.
see https://developer.apple.com/documentation/appkit/nscell?language=objc"))

(define-objc-class "NSTextAttachmentCell" ()
  ()
  (:documentation
   "An object that implements the functionality of the text attachment cell protocol.
see https://developer.apple.com/documentation/appkit/nstextattachmentcell-swift.class?language=objc"))

;;; Glyphs

(define-objc-class "NSGlyphGenerator" ()
  ()
  (:documentation
   "An object that performs the initial, nominal glyph generation phase in the layout process.
see https://developer.apple.com/documentation/appkit/nsglyphgenerator?language=objc"))

(define-objc-class "NSGlyphInfo" ()
  ()
  (:documentation
   "A glyph attribute in an attributed string.
see https://developer.apple.com/documentation/appkit/nsglyphinfo?language=objc"))

;;; TextKit 1

(define-objc-class "NSTextStorage" ()
  ()
  (:documentation
   "The fundamental storage mechanism of TextKit that contains the text managed by the system.
see https://developer.apple.com/documentation/appkit/nstextstorage?language=objc"))

(define-objc-class "NSLayoutManager" ()
  ()
  (:documentation
   "An object that coordinates the layout and display of text characters.
see https://developer.apple.com/documentation/appkit/nslayoutmanager?language=objc"))

(define-objc-class "NSATSTypesetter" ()
  ()
  (:documentation
   "A concrete typesetter object that places glyphs during the text layout process.
see https://developer.apple.com/documentation/appkit/nsatstypesetter?language=objc"))

(define-objc-class "NSTypesetter" ()
  ()
  (:documentation
   "An abstract class that performs various type layout tasks.
see https://developer.apple.com/documentation/appkit/nstypesetter?language=objc"))

;;; Text


;;;; Fonts
;; Manage the fonts used to display text.
;; see https://developer.apple.com/documentation/appkit/fonts?language=objc

;;; Font Data

(define-objc-class "NSFont" ()
  (("fontDescriptor"
    :reader font-descriptor
    :documentation
    "The font descriptor object for the font.

The font descriptor contains a mutable dictionary of optional
attributes for creating an `ns-font' object. For more information about
font descriptors, see `ns-font-descriptor'.

see https://developer.apple.com/documentation/appkit/nsfont/fontdescriptor?language=objc")
   ("familyName"
    :reader family-name
    :documentation
    "The family name of the font.
For example, \"Times\" or \"Helvetica\".

This name is the one that NSFontManager uses and may differ slightly
from the AFM name.

The value in this property is intended for an application’s internal
usage and not for display. To get a name that you can display to the
user, use the displayName property instead.

see https://developer.apple.com/documentation/appkit/nsfont/familyname?language=objc")
   ("fontName"
    :reader font-name
    :documentation
    "The full name of the font, as used in PostScript language code.
For example, \"Times-Roman\" or \"Helvetica-Oblique\".

The value in this property is intended for an application’s internal
usage and not for display. To get a name that you can display to the
user, use the displayName property instead.

see https://developer.apple.com/documentation/appkit/nsfont/fontname?language=objc"))
  (:documentation
   "The representation of a font in an app.

`ns-font' objects represent fonts to an app, providing access to
characteristics of the font and assistance in laying out glyphs
relative to one another. Font objects are also used to establish the
current font for drawing text directly into a graphics context, using
the set method.

You don’t create `ns-font' objects using the `alloc' and `init'
methods. Instead, you use either fontWithDescriptor:size: or
fontWithName:size: to look up an available font and alter its size or
matrix to your needs. These methods check for an existing font object
with the specified characteristics, returning it if there is
one. Otherwise, they look up the font data requested and create the
appropriate object. NSFont also defines a number of methods for
getting standard system fonts, such as systemFontOfSize:,
userFontOfSize:, and messageFontOfSize:. To request the default size
for these standard fonts, pass a negative number or 0 as the font
size. See macOS Human Interface Guidelines for more information about
system fonts.

see https://developer.apple.com/documentation/appkit/nsfont?language=objc"))

(defgeneric as-ns-font-size (size)
  (:documentation
   "Convert SIZE into `ns-font' size.
Return value is double float in points. ")
  (:method ((size number))       (coerce size 'double-float))
  (:method ((size double-float)) size)
  (:method ((size null))         (as-ns-font-size :system))
  (:method ((label (eql :system)))
    "Returns the size of the standard system font."
    (invoke 'ns-font "systemFontSize"))
  (:method ((label (eql :system-small)))
    "Returns the size of the standard small system font."
    (invoke 'ns-font "smallSystemFontSize"))
  (:method ((label (eql :label)))
    "Returns the size of the standard label font."
    (invoke 'ns-font "labelFontSize")))

(define-objc-const +ns-font-weight-ultra-light+
    ("NSFontWeightUltraLight" :double)
  "The font weight for system ultra light font.")

(define-objc-const +ns-font-weight-thin+
    ("NSFontWeightThin"       :double)
  "The font weight for system thin font.")

(define-objc-const +ns-font-weight-light+
    ("NSFontWeightLight"      :double)
  "The font weight for system light font.")

(define-objc-const +ns-font-weight-regular+
    ("NSFontWeightRegular"    :double)
  "The font weight for system regular font.")

(define-objc-const +ns-font-weight-medium+
  ("NSFontWeightMedium"       :double)
  "The font weight for system medium font.")

(define-objc-const +ns-font-weight-semibold+
    ("NSFontWeightSemibold"   :double)
  "The font weight for system semibold font.")

(define-objc-const +ns-font-weight-bold+
    ("NSFontWeightBold"       :double)
  "The font weight for system bold font.")

(define-objc-const +ns-font-weight-heavy+
    ("NSFontWeightHeavy"      :double)
  "The font weight for system heavy font.")

(define-objc-const +ns-font-weight-black+
    ("NSFontWeightBlack"      :double)
  "The font weight for system black font.")

(defgeneric as-ns-font-weight (size)
  (:documentation
   "Convert SIZE into `ns-font' weight. ")
  (:method ((size number))       (coerce size 'double-float))
  (:method ((size double-float)) size)
  (:method ((size null))         (as-ns-font-weight :regular))
  (:method ((size (eql :ultra-light))) +ns-font-weight-ultra-light+)
  (:method ((size (eql :thin)))        +ns-font-weight-thin+)
  (:method ((size (eql :light)))       +ns-font-weight-light+)
  (:method ((size (eql :regular)))     +ns-font-regular+)
  (:method ((size (eql :medium)))      +ns-font-weight-medium+)
  (:method ((size (eql :semibold)))    +ns-font-weight-semibold+)
  (:method ((size (eql :bold)))        +ns-font-weight-bold+)
  (:method ((size (eql :heavy)))       +ns-font-weight-heavy+)
  (:method ((size (eql :black)))       +ns-font-weight-black+))

(defgeneric as-ns-font (object &key size weight &allow-other-keys)
  (:documentation
   "Convert OBJECT into `ns-font'.
Return `ns-font'.

Parameters:
+ SIZE: font size in points (see `as-ns-font-size')
+ WEIGHT: font weight (see `as-ns-font-weight')
")
  (:method ((font ns-font) &key size)
    (let ((font font))
      (when size
        (setf font (invoke (ns-font-manager)
                           "convertFont:toSize:"
                           font
                           (coerce size 'double-float))))
      font))
  (:method ((font (eql :system)) &key (size :system) (weight :regular weight?))
    "Returns the standard system font with the specified size.

Parameters:
+ WEIGHT:
  if `:bold', will call ObjC method boldSystemFontOfSize:
  otherwise, call systemFontOfSize:weight:

  if not set, call systemFontOfSize: by default
"
    (if weight?
        (if (eql weight :bold)
            (invoke 'ns-font "boldSystemFontOfSize:"
                    (as-ns-font-size size))
            (invoke 'ns-font "systemFontOfSize:weight:"
                    (as-ns-font-size   size)
                    (as-ns-font-weight weight)))
        (invoke 'ns-font "systemFontOfSize:"
                (as-ns-font-size size))))
  (:method ((font (eql :system-monospace)) &key (size :system) (weight :regular))
    "Returns a monospace version of the system font with the specified
size and weight.

This invokes ObjC method monospacedSystemFontOfSize:weight:.

Use the returned font for interface items that require monospaced
glyphs.  The returned font includes monospaced glyphs for the Latin
characters and the symbols commonly found in source code. Glyphs for
other symbols are usually wider or narrower than the monospaced
characters. To ensure the font uses fixed spacing for all characters,
apply the NSFontFixedAdvanceAttribute attribute to the any strings you
render.

see https://developer.apple.com/documentation/appkit/nsfont/monospacedsystemfont(ofsize:weight:)?language=objc"
    (invoke 'ns-font "monospacedSystemFontOfSize:weight:"
            (as-ns-font-size   size)
            (as-ns-font-weight weight)))
  (:method ((font (eql :system-monospace-digit)) &key (size :system) (weight :regular))
    "Returns a version of the standard system font that contains
monospaced digit glyphs.

This invokes ObjC method monospacedDigitSystemFontOfSize:weight:

The font returned by this method has monospaced digit glyphs. Glyphs
for other characters and symbols may be wider or narrower than the
monospaced characters. To ensure the font uses fixed spacing for all
characters, apply the NSFontFixedAdvanceAttribute attribute to the any
strings you render.

see https://developer.apple.com/documentation/appkit/nsfont/monospaceddigitsystemfont(ofsize:weight:)?language=objc"
    (invoke 'ns-font "monospacedDigitSystemFontOfSize:weight:"
            (as-ns-font-size   size)
            (as-ns-font-weight weight)))
  (:method ((font (eql :label)) &key (size :system))
    "Returns the font used for standard interface labels in the
specified size.

This invokes ObjC method labelFontOfSize:

The label font (Lucida Grande Regular 10 point) is used for the labels
on toolbar buttons and to label tick marks on full-size sliders. See
The macOS Environment in macOS Human Interface Guidelines for more
information about system fonts.

see https://developer.apple.com/documentation/appkit/nsfont/labelfont(ofsize:)?language=objc"
    (invoke 'ns-font "labelFontOfSize:" (as-ns-font-size size)))
  (:method ((font (eql :message)) &key (size :system))
    "Returns the font used for standard interface items, such as
button labels, menu items, and so on, in the specified size.

This invokes ObjC method messageFontOfSize:

see https://developer.apple.com/documentation/appkit/nsfont/messagefont(ofsize:)?language=objc"
    (invoke 'ns-font "messageFontOfSize:" (as-ns-font-size size)))
  (:method ((font (eql :menu-bar)) &key (size :system))
    "Returns the font used for menu bar items, in the specified size.

This invokes ObjC method menuBarFontOfSize:

see https://developer.apple.com/documentation/appkit/nsfont/menubarfont(ofsize:)?language=objc"
    (invoke 'ns-font "menuBarFontOfSize:" (as-ns-font-size size)))
  (:method ((font (eql :menu)) &key (size :system))
    "Returns the font used for menu items, in the specified size.

This invokes ObjC method menuFontOfSize:

see https://developer.apple.com/documentation/appkit/nsfont/menufont(ofsize:)?language=objc"
    (invoke 'ns-font "menuFontOfSize:" (as-ns-font-size size)))
  (:method ((font (eql :control-content)) &key (size :system))
    "Returns the font used for the content of controls in the
specified size.

This invokes ObjC method controlContentFontOfSize:

see https://developer.apple.com/documentation/appkit/nsfont/controlcontentfont(ofsize:)?language=objc"
    (invoke 'ns-font "controlContentFontOfSize:" (as-ns-font-size size)))
  (:method ((font (eql :title-bar)) &key (size :system))
    "Returns the font used for window title bars, in the specified size.

This invokes ObjC method titleBarFontOfSize:

see https://developer.apple.com/documentation/appkit/nsfont/titlebarfont(ofsize:)?language=objc"
    (invoke 'ns-font "titleBarFontOfSize:" (as-ns-font-size size)))
  (:method ((font (eql :palette)) &key (size :system))
    "Returns the font used for palette window title bars, in the
specified size.

This invokes ObjC method paletteFontOfSize:

see https://developer.apple.com/documentation/appkit/nsfont/palettefont(ofsize:)?language=objc"
    (invoke 'ns-font "paletteFontOfSize:" (as-ns-font-size size)))
  (:method ((font (eql :tool-tips)) &key (size :system))
    "Returns the font used for tool tips labels, in the specified size.

This invokes ObjC method toolTipsFontOfSize:

see https://developer.apple.com/documentation/appkit/nsfont/tooltipsfont(ofsize:)?language=objc"
    (invoke 'ns-font "toolTipsFontOfSize:" (as-ns-font-size size))))

(define-objc-class "NSFontDescriptor" ()
  ()
  (:documentation
   "A dictionary of attributes that describe a font.

A font descriptor can be used to create or modify an `ns-font'
object. The system provides a font matching capability, so that you
can partially describe a font by creating a font descriptor with, for
example, just a family name. You can then find all the available fonts
on the system with a matching family name using
matchingFontDescriptorsWithMandatoryKeys:.

There are several ways to create a new `ns-font-descriptor' object.
You can use alloc and initWithFontAttributes:,
fontDescriptorWithFontAttributes:, fontDescriptorWithName:matrix:, or
fontDescriptorWithName:size:. to create a font descriptor based on
either your custom attributes dictionary or on a specific font’s name
and size. Alternatively you can use one of the fontDescriptor…
instance methods (such as fontDescriptorWithFace:) to create a
modified version of an existing descriptor. The latter methods are
useful if you have an existing descriptor and simply want to change
one aspect.

All attributes in the attributes dictionary are optional.

see https://developer.apple.com/documentation/appkit/nsfontdescriptor?language=objc"))

;;; Management

(define-objc-class "NSFontManager" ()
  (("selectedFont"
    :reader selected-font
    :documentation
    "The currently selected font object.

The value of this property is the last font recorded with a
setSelectedFont:isMultiple: message.

While fonts are being converted in response to a convertFont: message,
you can determine the font selected in the Font panel like this:

    (let ((manager (ns-font-manager)))
      (convert-font manager (selected-font manager)))

"))
  (:documentation
   "The center of activity for the font-conversion system.

The font manager records the currently selected font, updates the Font
panel and Font menu to reflect the selected font, initiates font
changes, and converts fonts in response to requests from text-bearing
objects. In a more prosaic role, `ns-font-manager' can be queried for the
fonts available to the application and for the particular attributes
of a font, such as whether it’s condensed or extended.

You typically set up a font manager and the Font menu using Interface
Builder. However, you can also do so programmatically by getting the
shared font manager instance and having it create the standard Font
menu at runtime:

  (ns-font-manager)                          ; NSFontManager
  (invoke (ns-font-manager) \"fontMenu:\" t) ; NSMenu

You can then add the Font menu to your app’s main menu. After the Font
menu is installed, your app automatically gains the functionality of
both the Font menu and the Font panel.

Font collections are managed by NSFontManager.

see https://developer.apple.com/documentation/appkit/nsfontmanager?language=objc"))

;; Getting the Shared Font Manager

(declaim (type (or null ns-font-manager) *ns-font-manager*))
(defparameter *ns-font-manager* nil
  "")

(defmethod initialize-instance :after ((manager ns-font-manager) &key)
  "After [NSFont sharedFontManager], set global `*ns-font-manager*'. "
  (setf *ns-font-manager* manager))

(defun ns-font-manager ()
  "Returns the shared instance of the font manager for the
application, creating it if necessary."
  (invoke 'ns-font-manager "sharedFontManager"))

;; `*ns-font-manager*' should be cleared when `coca-init'
(define-coca-init :pre (setf *ns-font-manager* nil))

;; Changing the Default Font Conversion Classes

;; Getting Available Fonts

;; TODO: should it be cached?
(defmethod available-fonts ((manager ns-font-manager))
  "The names of the fonts available in the system (not the NSFont objects themselves).
Note that these fonts are in various system font directories.
Return a list of font names string.
see https://developer.apple.com/documentation/appkit/nsfontmanager/availablefonts?language=objc"
  (mapcar #'ns-string-to-string (ns-array-to-list (invoke manager "availableFonts"))))

(defmethod available-font-families ((manager ns-font-manager))
  "The names of the font families available in the system.
Note that these fonts are in various system font directories.
see https://developer.apple.com/documentation/appkit/nsfontmanager/availablefontfamilies?language=objc"
  (mapcar #'ns-string-to-string (ns-array-to-list (invoke manager "availableFontFamilies"))))

;; Setting and Examining the Selected Font

;; Sending Action Methods

;; Converting Fonts Automatically

(defmethod convert-font ((manager ns-font-manager) font)
  "Converts the given font according to the object that initiated a
font change, typically the Font panel or Font menu.
Return converted font, or aFont itself if the conversion isn’t possible.

Parameters:
+ FONT: the font to convert

This method is invoked in response to an action message such as
addFontTrait: or modifyFontViaPanel:. These initiating methods cause
the font manager to query the sender for the action to take and the
traits to change. See Converting Fonts Manually for more information."
  (invoke manager "convertFont:" (as-ns-font font)))

;; Converting Fonts Manually

(define-objc-mask ns-font-trait-mask
  "Constants for isolating specific traits of a font.

NSFontManager categorizes fonts according to a small set of
traits. You can convert fonts by adding and removing individual
traits, and you can get a font with a specific combination of traits.

These pairs of traits are mutually exclusive:

    :condensed and :expanded

    :bold and :unbold

    :italic and :unitalic
"
  "Trait Masks"
  (:bold                       2        "A mask that specifies a bold font.")
  (:compressed                 512      "A mask that specifies a compressed font.")
  (:condensed                  64       "A mask that specifies a condensed font.")
  (:expanded                   32       "A mask that specifies an expanded font.")
  (:fixed-pitch                1024     "A mask that specifies a fixed pitch font.")
  (:italic                     1        "A mask that specifies an italic font.")
  (:narrow                     16       "A mask that specifies a narrow font.")
  (:non-standard-character-set 8        "A mask that specifies a font containing a non-standard character set.")
  (:poster                     256      "A mask that specifies a poster-style font.")
  (:small-caps                 128      "A mask that specifies a small-caps font.")
  (:unbold                     4        "A mask that specifies a font that is not bold.")
  (:unitalic                   16777216 "A mask that specifies a font that is not italic."))

(defmethod as-ns-font :around (font &rest modification
                               &key face family traits no-traits)
  (declare (type (or null string) face family))
  (let ((font (the ns-font (call-next-method))))
    (when modification
      (when face
        (setf font (invoke (ns-font-manager)
                           "convertFont:toFace:"
                           font
                           (string-to-ns-string face))))
      (when family
        (setf font (invoke (ns-font-manager)
                           "convertFont:toFamily:"
                           font
                           (string-to-ns-string family))))
      (when traits
        (setf font (invoke (ns-font-manager)
                           "convertFont:toHaveTrait:"
                           font
                           (as-ns-font-trait-mask traits))))
      (when no-traits
        (setf font (invoke (ns-font-manager)
                           "convertFont:toNotHaveTrait:"
                           font
                           (as-ns-font-trait-mask no-traits)))))
    font))

;; Getting a Particular Font

;; Examining Fonts

;; Managing the Font Panel and Font Menu

;; Accessing the Action Property

;; Setting Attributes

(define-objc-class "NSFontCollection" ()
  ()
  (:documentation
   "A font collection, which is a group of font descriptors taken together as a single object.
see https://developer.apple.com/documentation/appkit/nsfontcollection?language=objc"))

(define-objc-class "NSMutableFontCollection" ()
  ()
  (:documentation
   "A mutable collection of font descriptors taken together as a single object.
see https://developer.apple.com/documentation/appkit/nsmutablefontcollection?language=objc"))


;;;; Writing Tools
;; Add support for Writing Tools to your app’s text views.
;; see https://developer.apple.com/documentation/appkit/writing-tools?language=objc

;;; Configuration

;;; Writing Tools for custom views

(define-objc-class "NSWritingToolsCoordinator" ()
  ()
  (:documentation
   "An object that manages interactions between Writing Tools and your custom text view.
see https://developer.apple.com/documentation/appkit/nswritingtoolscoordinator?language=objc"))

(define-objc-class "NSWritingToolsCoordinatorContext" ()
  ()
  (:documentation
   "A data object that you use to share your custom view’s text with Writing Tools.
see https://developer.apple.com/documentation/appkit/nswritingtoolscoordinator/context?language=objc"))

(define-objc-class "NSWritingToolsCoordinatorAnimationParameters" ()
  ()
  (:documentation
   "An object you use to configure additional tasks or animations to run alongside the Writing Tools animations.
see https://developer.apple.com/documentation/appkit/nswritingtoolscoordinator/animationparameters?language=objc"))

;;; Text previews

(define-objc-class "NSTextPreview" ()
  ()
  (:documentation
   "A snapshot of the text in your view, which the system uses to create user-visible effects.
see https://developer.apple.com/documentation/appkit/nstextpreview?language=objc"))

;;; Toolbar configuration


;;;; App and Environment
;; Learn about the objects that you use to interact with the system.
;; see https://developer.apple.com/documentation/appkit/app-and-environment?language=objc

;;; Life Cycle

(define-objc-class "NSApplication" ()
  (;; Managing the event loop
   ("currentEvent"
    :reader current-event
    :documentation
    "The last event object that the app retrieved from the event queue.
see https://developer.apple.com/documentation/appkit/nsapplication/currentevent?language=objc")
   ("running"
    :reader running-p
    :documentation
    "Test if the main event loop is running.
see https://developer.apple.com/documentation/appkit/nsapplication/isrunning?language=objc")
   ;; Activating and deactivating the app
   ("active"
    :reader activep
    :documentation
    "Test if this is the active app.
see https://developer.apple.com/documentation/appkit/nsapplication/isactive?language=objc")
   ;; Accessing the dock tile
   ("dockTile"
    :reader dock-tile
    :documentation
    "The app’s Dock tile.
see https://developer.apple.com/documentation/appkit/nsapplication/docktile?language=objc")
   ("windows"
    :reader windows
    :after  ns-array-to-list
    :documentation
    "A list of the app’s window objects.

This property contains an array of NSWindow objects corresponding to
all currently existing windows for the app. The array includes all
onscreen and offscreen windows, whether or not they are visible on any
space. There is no guarantee of the order of the windows in the array.
see https://developer.apple.com/documentation/appkit/nsapplication/windows?language=objc")
   ("applicationIconImage"
    :accessor application-icon-image
    :before   as-ns-image
    :documentation
    "The image used for the app’s icon.
see https://developer.apple.com/documentation/appkit/nsapplication/applicationiconimage?language=objc")
   ("mainMenu"
    :accessor main-menu
    :before   as-ns-menu
    :documentation
    "The app’s main menu bar.

Use this property to assign a new menu bar for your app or to access
the current menu bar.

see https://developer.apple.com/documentation/appkit/nsapplication/mainmenu?language=objc"))
  (:documentation
   "An object that manages an app’s main event loop and resources used by all of that app's objects.
Every app uses a single instance of `ns-application' to control the main
event loop, keep track of the app's windows and menus, distribute
events to the appropriate objects (that’s, itself or one of its windows),
set up autorelease pools, and receive notification of app-level events.

An `ns-application' object has a delegate (an object that you assign)
that’s notified when the app starts or terminates, is hidden or
activated, should open a file selected by the user, and so forth. By
setting the delegate and implementing the delegate methods, you
customize the behavior of your app without having to subclass
`ns-application'.

In Lisp main thread, create the `ns-application' instance by calling
function `ns-app'. An example of main thread function:

    (defun ns-application-main ()
      (let ((app (ns-app)))
        ;; load nib or else
        ;; set up window or else
        (run app)))

The `ns-app' function initializes the display environment
and connects your program to the window server and the display
server. The `ns-application' object maintains a list of all the
`ns-window' objects the app uses, so it can retrieve any of the app’s
`ns-view' objects.

Dev Note:
Use `ns-app' to retrive the global variable `coca.appkit::*ns-app*'.
The shared `ns-application' object performs the important task of
receiving events from the window server and distributing them to the
proper `ns-responder' objects. `ns-app' translates an event into an
`ns-event' object, then forwards the event object to the affected
`ns-window' object. All keyboard and mouse events go directly to the
`ns-window' object associated with the event. The only exception to
this rule is if the Command key is pressed when a key-down event
occurs; in this case, every `ns-window' object has an opportunity to
respond to the event. When a window object receives an `ns-window'
object from `ns-app', it distributes it to the objects in its view
hierarchy.

`ns-application' is also responsible for dispatching certain Apple
events received by the app. For example, macOS sends Apple events to
your app at various times, such as when the app is launched or
reopened. `ns-application' installs Apple event handlers to handle
these events by sending a message to the appropriate object. You can
also use the `ns-apple-event-manager' class to register your own Apple
event handlers. The applicationWillFinishLaunching: method is
generally the best place to do so. For more information on how events
are handled and how you can modify the default behavior, including
information on working with Apple events in scriptable apps, see
``How Cocoa Applications Handle Apple Events'' in ``Cocoa Scripting Guide''.

The `ns-application' class sets up @autorelease block during
initialization and inside the event loop-specifically, within its
initialization and run methods. Similarly, the
methods `Coca.AppKit' adds to `ns-bundle' employ @autorelease blocks
during the loading of nib files. These @autorelease blocks aren’t
accessible outside the scope of the respective `ns-application' and
`ns-bundle' methods. Typically, an app creates objects either while
the event loop is running or by loading objects from nib files, so
this lack of access usually isn’t a problem. However, if you do need
to use Cocoa classes within the main() function itself
(other than to load nib files or to instantiate `ns-application'),
you should create an @autorelease block to contain the code using
the classes.

The delegate and notifications
=====================================
You can assign a delegate to your `ns-application' object. The delegate
responds to certain messages on behalf of the object. Some of these
messages, such as application:openFile:, ask the delegate to perform
an action. Another message, applicationShouldTerminate:, lets the
delegate determine whether the app should be allowed to quit. The
`ns-application' class sends these messages directly to its delegate.

`ns-application' also posts notifications to the app’s default
notification center. Any object may register to receive one or more of
the notifications posted by NSApplication by sending the message
addObserver:selector:name:object: to the default notification center
(an instance of the NSNotificationCenter class). The delegate of
NSApplication is automatically registered to receive these
notifications if it implements certain delegate methods. For example,
NSApplication posts notifications when it’s about to be done launching
the app and when it’s done launching the app
(NSApplicationWillFinishLaunchingNotification and
NSApplicationDidFinishLaunchingNotification). The delegate has an
opportunity to respond to these notifications by implementing the
methods applicationWillFinishLaunching: and
applicationDidFinishLaunching:. If the delegate wants to be informed
of both events, it implements both methods. If it needs to know only
when the app is finished launching, it implements only
applicationDidFinishLaunching:.

System services
======================
`ns-application' interacts with the system services architecture to
provide services to your app through the Services menu.

Subclassing notes
======================
You rarely should find a real need to create a custom `ns-application'
subclass. Unlike some object-oriented libraries, Cocoa doesn’t require
you to subclass `ns-application' to customize app behavior. Instead it
gives you many other ways to customize an app. This section discusses
both some of the possible reasons to subclass `ns-application' and some
of the reasons not to subclass `ns-application'.

To use a custom subclass of `ns-application', send sharedApplication to
your subclass rather than directly to `ns-application'. If you create
your app in Xcode, you can accomplish this by setting your custom app
class to be the principal class. In Xcode, double-click the app target
in the Groups and Files list to open the Info window for the
target. Then display the Properties pane of the window and replace
“NSApplication” in the Principal Class field with the name of your
custom class. The NSApplicationMain function sends sharedApplication
to the principal class to obtain the global app instance (NSApp)—which
in this case will be an instance of your custom subclass of
`ns-application'.

Important: Many AppKit classes rely on the `ns-application' class and may
not work properly until this class is fully initialized. As a result,
you should not, for example, attempt to invoke methods of other AppKit
classes from an initialization method of an `ns-application' subclass.

Methods to override:
Generally, you subclass NSApplication to provide your own special
responses to messages that are routinely sent to the global app object
(NSApp). NSApplication doesn’t have primitive methods in the sense of
methods that you must override in your subclass. Here are four methods
that are possible candidates for overriding:
+ Override `run' if you want the app to manage the main event loop
  differently than it does by default. (This a critical and complex
  task, however, that you should only attempt with good reason).
+ Override `sendEvent:' if you want to change how events are dispatched
  or perform some special event processing.
+ Override `requestUserAttention:' if you want to modify how your app
  attracts the attention of the user (for example, offering an
  alternative to the bouncing app icon in the Dock).
+ Override `targetForAction:' to substitute another object for the target
  of an action message.

Special considerations:
The global app object uses @autorelease blocks in its run method; if
you override this method, you’ll need to create your own @autorelease
blocks.  Do not override sharedApplication. The default
implementation, which is essential to app behavior, is too complex to
duplicate on your own.

Alternatives to subclassing:
NSApplication defines numerous Delegation methods that offer
opportunities for modifying specific aspects of app behavior. Instead
of making a custom subclass of NSApplication, your app delegate may be
able to implement one or more of these methods to accomplish your
design goals. In general, a better design than subclassing
NSApplication is to put the code that expresses your app’s special
behavior into one or more custom objects called controllers. Methods
defined in your controllers can be invoked from a small dispatcher
object without being closely tied to the global app object.
see https://developer.apple.com/documentation/appkit/nsapplication?language=objc"))

;; Getting the shared app object

(declaim (type (or null ns-application) *ns-app*))
(defparameter *ns-app* nil
  "Global `ns-application' object after calling sharedApplication. ")

(defmethod initialize-instance :after ((ns-app ns-application) &key)
  "After [NSApplication sharedApplication], set global `*ns-app*'. "
  (setf *ns-app* ns-app))

(defun ns-app ()
  "Returns the shared `ns-application'. "
  (or *ns-app* (invoke 'ns-application "sharedApplication")))

(define-coca-init :pre
  (setf *ns-app* nil))

;; Managing the event loop

(define-objc-const +ns-event-tracking-run-loop-mode+
    ("NSEventTrackingRunLoopMode" :object appkit)
  "The mode set when tracking events modally, such as a mouse-dragging loop.
see https://developer.apple.com/documentation/appkit/nseventtrackingrunloopmode?language=objc")

(define-objc-const +ns-modal-panel-run-loop-mode+
    ("NSModalPanelRunLoopMode" :object appkit)
  "The mode set when waiting for input from a modal panel, such as a save or open panel.
see https://developer.apple.com/documentation/appkit/nsmodalpanelrunloopmode?language=objc")

(defmethod next-event ((app ns-application)
                       &key
                         (mask  :any)
                         (util  nil)
                         (mode  +ns-default-run-loop-mode+)
                         (deque t)
                       &allow-other-keys)
  "Returns the next event matching a given mask,
or nil if no such event is found before a specified expiration date.

Parameters:
+ MASK: `ns-event-mask'
+ UTIL: The expiration date for the current event request (default `ns-date-distant-future')
+ MODE: The run loop mode in which to run while looking for events.
  could be:
  + `+ns-default-run-loop-mode+'
  + `+ns-run-loop-common-modes+'
  + `+ns-event-tracking-run-loop-mode+'
  + `+ns-modal-panel-run-loop-mode+'
  + `+ns-connnection-reply-mode+'
+ DEQUEUE: Specify `t' (default) if you want the event removed from the queue.

Dev Note:
Invokes ObjC method nextEventMatchingMask:untilDate:inMode:dequeue:

see https://developer.apple.com/documentation/appkit/nsapplication/nextevent(matching:until:inmode:dequeue:)?language=objc"
  (declare (type (or list ns-event-mask) mask)
           (type (or null ns-date)       util)
           (type (or ns-string string)   mode))
  (invoke app
          "nextEventMatchingMask:untilDate:inMode:dequeue:"
          (apply #'as-ns-event-mask (coca.objc::listfy mask))
          (or util (ns-date-distant-future))
          mode
          (and deque t)))

(defmethod finish-launching ((app ns-application))
  "Activates the app,
opens any files specified by the `ns-open' user default,
and unhighlights the app’s icon.

Parameter:
+ APP: `ns-application'

The run method calls this method before it starts the event loop.
When this method begins, it posts an
NSApplicationWillFinishLaunchingNotification to the default
notification center. If you override finishLaunching, the subclass
method should invoke the superclass method.
"
  (invoke app "finishLaunching"))

(defmethod run ((app ns-application))
  "Starts the main event loop. "
  (invoke app "run"))

(defmethod stop ((app ns-application) (sender standard-objc-object))
  "Stops the main event loop.

Parameters:
+ SENDER: the Object that sent this message

This method notifies the app that you want to exit the current run
loop as soon as it finishes processing the current NSEvent
object. This method doesn’t forcibly exit the current run
loop. Instead it sets a flag that the app checks only after it
finishes dispatching an actual event object. For example, you could
call this method from an action method responding to a button click or
from one of the many methods defined by the NSResponder
class. However, calling this method from a timer or run-loop observer
routine wouldn’t stop the run loop because they don’t result in the
posting of an NSEvent object.

If you call this method from an event handler running in your main run
loop, the app object exits out of the run method, thereby returning
control to the main() function. If you call this method from within a
modal event loop, it will exit the modal loop instead of the main
event loop.

see https://developer.apple.com/documentation/appkit/nsapplication/stop(_:)?language=objc"
  (invoke app "stop:" sender))

(defmethod terminate ((app ns-application) (sender standard-objc-object))
  "Terminates the receiver.

Parameters:
+ NS-APP: NSApplication
+ SENDER:
  Typically, this parameter contains the object that initiated the
  termination request.

This method is typically invoked when the user chooses Quit or Exit
from the app’s menu.

When invoked, this method performs several steps to process the
termination request. First, it asks the app’s document controller (if
one exists) to save any unsaved changes in its documents. During this
process, the document controller can cancel termination in response to
input from the user. If the document controller doesn’t cancel the
operation, this method then calls the delegate’s
applicationShouldTerminate: method. If applicationShouldTerminate:
returns NSTerminateCancel, the termination process is aborted and
control is handed back to the main event loop. If the method returns
NSTerminateLater, the app runs its run loop in the
NSModalPanelRunLoopMode mode until the
replyToApplicationShouldTerminate: method is called with the value
true or false. If the applicationShouldTerminate: method returns
NSTerminateNow, this method posts a
NSApplicationWillTerminateNotification notification to the default
notification center.

Don’t bother to put final cleanup code in your app’s main()
function—it will never be executed. If cleanup is necessary, perform
that cleanup in the delegate’s applicationWillTerminate: method.

see https://developer.apple.com/documentation/appkit/nsapplication/terminate(_:)?language=objc"
  (invoke app "terminate:" sender))

(defmethod send-event ((app ns-application) (event ns-event))
  "Dispatches an event to other objects.

Parameter:
+ APP: `ns-application'
+ EVENT: `ns-event' to dispatch

You rarely invoke sendEvent: directly, although you might want to
override this method to perform some action on every event. sendEvent:
messages are sent from the main event loop (the run
method). sendEvent: is the method that dispatches events to the
appropriate responders—NSApp handles app events, the NSWindow object
indicated in the event record handles window-related events, and mouse
and key events are forwarded to the appropriate NSWindow object for
further dispatching."
  (invoke app "sendEvent:" event))

;; Positing actions

;; Terminating the app

;; Activating and deactivating the app

;; Managing relaunch on login

;; Managing remote notifications

;; Managing the app's apperance

;; Managing windows, panels, and menus

;; Modal Windows and Panels

(define-objc-enum ns-modal-response
  "A set of button return values for modal dialogs.

The response value that a button returns can depend on which method is
used to present the dialog. See
alertWithMessageText:defaultButton:alternateButton:otherButton:informativeTextWithFormat:,
runModal, and addButtonWithTitle: for examples.

see https://developer.apple.com/documentation/appkit/nsapplication/modalresponse?language=objc"
  (:ok                   1
                        "The presentation or dismissal of the sheet"
                        "has finished.")
  (:cancel               0
                        "The presentation or dismissal of the sheet"
                        "has been canceled.")
  (:continue             18446744073709550614
                        "Modal session is continuing (returned by"
                        "runModalSession: only).")
  (:stop                 18446744073709550616
                        "Modal session was broken with stopModal.")
  (:abort                18446744073709550615
                        "Modal session was broken with abortModal.")
  (:first-button-return  1000
                         "The user clicked the first (rightmost)"
                         "button on the dialog or sheet.")
  (:second-button-return 1001
                         "The user clicked the second button from the"
                         "right edge of the dialog or sheet.")
  (:third-button-return  1002
                         "The user clicked the third button from the"
                         "right edge of the dialog or sheet."))

;; - App Windows

(defmethod update-windows ((app ns-application))
  "Sends an update message to each onscreen window. "
  (invoke app "updateWindows"))

;; User interface layout direction

;; Accessing the dock tile

;; Customizing the Touch Bar

;; Managing user attension requests

;; Providing help information

;; Providing services

;; Determining access to the keyboard

;; Hiding apps

;; Managing threads

;; Logging exceptions

;; Configuring the activation policy

(define-objc-enum ns-application-activation-policy
  "Activation policies (used by activationPolicy) that control whether
and how an app may be activated.
see https://developer.apple.com/documentation/appkit/nsapplication/activationpolicy-swift.enum?language=objc"
  "Activation Policies"
  (:regular    0
              "The application is an ordinary app that appears in the"
              "Dock and may have a user interface.")
  (:accessory  1
              "The application doesn’t appear in the Dock and doesn’t "
              "have a menu bar, but it may be activated programmatically"
              "or by clicking on one of its windows.")
  (:prohibited 2
               "The application doesn’t appear in the Dock and may not "
               "create windows or be activated."))

(defmethod activation-policy ((app ns-application))
  "Returns the app’s activation policy.
see https://developer.apple.com/documentation/appkit/nsapplication/activationpolicy()?language=objc"
  (decode-ns-application-activation-policy
   (invoke app "activationPolicy")))

(defmethod (setf activation-policy) (policy (app ns-application))
  "Attempts to modify the app’s activation policy.

You can set any activation policy in macOS 10.9 and later; in macOS
10.8 and earlier, you can only set the activation policy to
`:prohibited' or `:regular'.

see https://developer.apple.com/documentation/appkit/nsapplication/setactivationpolicy(_:)?language=objc"
  (let ((res (invoke app
                     "setActivationPolicy:"
                     (as-ns-application-activation-policy policy))))
    (unless res
      (warn "Failed to switch ~S policy with ~S. " app policy))
    policy))

;; Scripting your app

;; Notifications

;; Loading Cocoa bundles

;; Displaying high dynamic resolution (HDR) content

(define-objc-class "NSRunningApplication" ()
  ()
  (:documentation
   "An object that can manipulate and provide information for a single instance of an app.
see https://developer.apple.com/documentation/appkit/nsrunningapplication?language=objc"))

;;; Environment

(define-objc-class "NSWorkspace" ()
  ()
  (:documentation
   "A workspace that can launch other apps and perform a variety of file-handling services.
There is one shared NSWorkspace object per app.
You use the class method sharedWorkspace to access it.
For example, the following statement uses an `ns-workspace' object to request that
a file be opened in the TextEdit app:

    (invoke (invoke \"NSWorkspace\" \"sharedWorkspace\")
            \"openFile:withApplication:\"
            file
            application)

You can use the workspace object to:
+ Open, manipulate, and get information about files and devices.
+ Track changes to the file system, devices, and the user database.
+ Get and set Finder information for files.
+ Launch apps.
see https://developer.apple.com/documentation/appkit/nsworkspace?language=objc"))

(define-objc-class "NSWorkspaceOpenConfiguration" ()
  ()
  (:documentation
   "The configuration options for opening URLs or launching apps.
Create an `ns-workspace-open-configuration' object before launching an app
or opening a URL using the shared `ns-workspace' object.
Use the properties of this object to customize the behavior of the launched app
or the handling of the URLs.

For example, you might tell the app to hide itself immediately after launch.
see https://developer.apple.com/documentation/appkit/nsworkspace/openconfiguration?language=objc"))

;;; Handoff

(define-objc-class "NSUserActivity" ()
  ()
  (:documentation
   "A representation of the state of your app at a moment in time.
see https://developer.apple.com/documentation/Foundation/NSUserActivity?language=objc"))

;;; App Services

(define-objc-class "NSSharingService" ()
  ()
  (:documentation
   "An object that facilitates the sharing of content with social media services, or with apps like Mail or Safari.
see https://developer.apple.com/documentation/appkit/nssharingservice?language=objc"))

(define-objc-class "NSToolbarItem" ()
  ()
  (:documentation
   "A single item that appears in a window’s toolbar.
see https://developer.apple.com/documentation/appkit/nstoolbaritem?language=objc"))

(define-objc-class "NSSharingServicePickerToolbarItem" ()
  ()
  (:documentation
   "A toolbar item that displays the macOS share sheet.
see https://developer.apple.com/documentation/appkit/nssharingservicepickertoolbaritem?language=objc"))

;;; App Help

(define-objc-class "NSHelpManager" ()
  ()
  (:documentation
   "An object for displaying online help for an app.
see https://developer.apple.com/documentation/appkit/nshelpmanager?language=objc"))

;;; Errors

;;; App Structure

;;;; appkit.lisp ends here
