;;;; appkit.lisp --- ObjC bindings for AppKit Framework

(uiop:define-package #:coca.appkit
  (:use :cl :coca.objc :coca.foundation)
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
   #:running-p
   #:activep
   #:current-event
   #:ns-app
   #:+ns-event-tracking-run-loop-mode+
   #:+ns-modal-panel-run-loop-mode+
   #:next-event
   #:run
   #:finish-launching
   #:send-event
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
   #:coca
   #:ns-control
   #:ns-cell
   #:ns-action-cell
   #:ns-split-view
   #:ns-stack-view
   #:ns-tab-view
   #:ns-text-view
   #:ns-button
   #:ns-color-well
   #:ns-combo-button
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
   #:ns-box

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
   #:ns-window
   #:ns-window-style-mask
   #:ns-window-style-mask-p
   #:decode-ns-window-style-mask
   #:ns-backing-store-type
   #:ns-backing-store-type-p
   #:decode-backing-store-type
   #:ns-window-init
   #:content-view-controller
   #:content-view
   #:style
   #:alpha-value
   #:background-color
   #:color-space
   #:can-hide
   #:on-active-space
   #:on-active-space
   #:hides-on-deactivate
   #:collection-behavior
   #:opaque
   #:has-shadow
   #:visible
   #:ns-panel
   #:ns-window-tab
   #:ns-window-tab-group
   #:ns-screen
   #:ns-popover
   #:ns-alert
   #:ns-open-panel
   #:ns-save-panel
   #:ns-sharing-service-picker
   #:ns-preview-representing-activity-item
   #:ns-pdf-panel
   #:ns-color-panel
   #:ns-color-picker
   #:ns-font-panel

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
   #:ns-event-mask-p
   #:decode-ns-event-mask
   #:ns-event
   #:location-in-window
   #:event-type
   #:event-subtype
   #:modifier-flags

   ;; Menus, Cursors, and the Dock
   #:ns-menu
   #:ns-menu-item
   #:ns-menu-item-badge
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
   #:ns-text-field
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
   #:ns-font-descriptor
   #:ns-font-manager
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

(doc-objc-class "NSDocument"            ; ns-document
  "An abstract class that defines the interface for macOS documents."
  "A document is an object that can internally represent data
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
see https://developer.apple.com/documentation/appkit/developing-a-document-based-app?language=objc"
  "see https://developer.apple.com/documentation/appkit/nsdocument?language=objc")

(doc-objc-class "NSDocumentController"  ; ns-document-controller
  "An object that manages an app’s documents."
  "As the first-responder target of New and Open menu commands,
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
in apps that don’t use subclasses of `ns-document'."
  "see https://developer.apple.com/documentation/appkit/nsdocumentcontroller?language=objc")

(doc-objc-class "NSPersistentDocument"  ; ns-persistent-document
  "A document object that can integrate with Core Data."
  "see https://developer.apple.com/documentation/appkit/nspersistentdocument?language=objc")

;;; User Preferences

;;; Pasteboard

(doc-objc-class "NSPasteboard"          ; ns-pasteboard
  "An object that transfers data to and from the pasteboard server."
  "see https://developer.apple.com/documentation/appkit/nspasteboard?language=objc")

;;; File Promises

(doc-objc-class "NSFilePromiseProvider" ; ns-file-promise-provider
  "An object that provides a promise for the pasteboard."
  "see https://developer.apple.com/documentation/appkit/nsfilepromiseprovider?language=objc")

(doc-objc-class "NSFilePromiseReceiver" ; ns-file-promise-receiver
  "An object that receives a file promise from the pasteboard."
  "see https://developer.apple.com/documentation/appkit/nsfilepromisereceiver?language=objc")

;;; Object Editing


;;;; Cocoa Bindings
;; Automatically synchronize your data model with your app’s interface using Cocoa Bindings.
;; see https://developer.apple.com/documentation/appkit/cocoa-bindings?language=objc

;;; Core Controllers

(doc-objc-class "NSObjectController"    ; ns-object-controller
  "A controller that can manage an object’s properties referenced by key-value paths."
  "see https://developer.apple.com/documentation/appkit/nsobjectcontroller?language=objc")

(doc-objc-class "NSController"          ; ns-controller
  "An abstract class that implements the NSEditor and NSEditorRegistration informal protocols required for controller classes."
  "see https://developer.apple.com/documentation/appkit/nscontroller?language=objc")

;;; Tree-Based Data

(doc-objc-class "NSTreeController"      ; ns-tree-controller
  "A bindings-compatible controller that manages a tree of objects."
  "see https://developer.apple.com/documentation/appkit/nstreecontroller?language=objc")

(doc-objc-class "NSTreeNode"            ; ns-tree-node
  "A node in a tree of nodes."
  "see https://developer.apple.com/documentation/appkit/nstreenode?language=objc")

;;; Array-Based Data

(doc-objc-class "NSArrayController"     ; ns-array-controller
  "A bindings-compatible controller that manages a collection of objects."
  "see https://developer.apple.com/documentation/appkit/nsarraycontroller?language=objc")

;;; Key-Value Data

(doc-objc-class "NSDictionaryController" ; ns-dictionary-controller
  "A bindings-compatible controller that manages the display and editing of
a dictionary of key-value pairs."
  "see https://developer.apple.com/documentation/appkit/nsdictionarycontroller?language=objc")

(doc-objc-class "NSDictionaryControllerKeyValuePair" ; ns-dictionary-controller-key-value-pair
  "A set of methods implemented by arranged objects to give access to information about those objects."
  "see https://developer.apple.com/documentation/appkit/nsdictionarycontrollerkeyvaluepair?language=objc")

;;; Data Placeholders


;;;; Resource Management
;; Manage the storyboards and nib files containing your app’s user interface,
;; and learn how to load data that is stored in resource files.
;; see https://developer.apple.com/documentation/appkit/resource-management?language=objc

;;; Storyboard

(doc-objc-class "NSStoryboard"          ; ns-storyboard
  "An encapsulation of the design-time view controller and window controller
graph represented in an Interface Builder storyboard resource file."
  "see https://developer.apple.com/documentation/appkit/nsstoryboard?language=objc")

(doc-objc-class "NSStoryboardSegue"     ; ns-storyboard-segue
  "A transition or containment relationship between two scenes in a storyboard."
  "see https://developer.apple.com/documentation/appkit/nsstoryboardsegue?language=objc")

;;; Assets

(doc-objc-class "NSDataAsset"           ; ns-data-asset
  "An object from a data set type stored in an asset catalog."
  "see https://developer.apple.com/documentation/appkit/nsdataasset?language=objc")

;;; Nib Files

(doc-objc-class "NSNib"                 ; ns-nib
  "An object wrapper, or container, for an Interface Builder nib file."
  "see https://developer.apple.com/documentation/appkit/nsnib?language=objc")

(doc-objc-class "NSNibConnector"        ; ns-nib-connector
  "A connection between two nibs."
  "see https://developer.apple.com/documentation/appkit/nsnibconnector?language=objc")

(doc-objc-class "NSNibControlConnector" ; ns-nib-control-connector
  "A control connection between two Interface Builder objects."
  "see https://developer.apple.com/documentation/appkit/nsnibcontrolconnector?language=objc")

(doc-objc-class "NSNibOutletConnector"  ; ns-nib-outlet-connector
  "An outlet connection between Interface Builder objects."
  "see https://developer.apple.com/documentation/appkit/nsniboutletconnector?language=objc")


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

(doc-objc-class "NSView"                ; ns-view
  "The infrastructure for drawing, printing, and handling events in an app."
  "see https://developer.apple.com/documentation/appkit/nsview?language=objc")

(doc-objc-class "NSControl"             ; ns-control
  "A specialized view, such as a button or text field, that notifies your app of relevant events using the target-action design pattern."
  "see https://developer.apple.com/documentation/appkit/nscontrol?language=objc")

(doc-objc-class "NSCell"                ; ns-cell
  "A mechanism for displaying text or images in a view object without
the overhead of a full NSView subclass."
  "see https://developer.apple.com/documentation/appkit/nscell?language=objc")

(doc-objc-class "NSActionCell"          ; ns-action-cell
  "An active area inside a control."
  "see https://developer.apple.com/documentation/appkit/nsactioncell?language=objc")

;;; Container views

(doc-objc-class "NSSplitView"           ; ns-split-view
  "A view that arranges two or more views in a linear stack running horizontally or vertically."
  "see https://developer.apple.com/documentation/appkit/nssplitview?language=objc")

(doc-objc-class "NSStackView"           ; ns-stack-view
  "A view that arranges an array of views horizontally or vertically and updates their placement and sizing when the window size changes."
  "see https://developer.apple.com/documentation/appkit/nsstackview?language=objc")

(doc-objc-class "NSTabView"             ; ns-tab-view
  "A multipage interface that displays one page at a time."
  "see https://developer.apple.com/documentation/appkit/nstabview?language=objc")

;;; Content views

(doc-objc-class "NSTextView"            ; ns-text-view
  "A view that draws text and handles user interactions with that text."
  "see https://developer.apple.com/documentation/appkit/nstextview?language=objc")

;;; Controls

(doc-objc-class "NSButton"              ; ns-button
  "A control that defines an area on the screen that a user clicks to trigger an action."
  "see https://developer.apple.com/documentation/appkit/nsbutton?language=objc")

(doc-objc-class "NSColorWell"           ; ns-color-well
  "A control that displays a color value and lets the user change that color value."
  "see https://developer.apple.com/documentation/appkit/nscolorwell?language=objc")

(doc-objc-class "NSComboButton"         ; ns-combo-button
  "A button with a pull-down menu and a default action."
  "see https://developer.apple.com/documentation/appkit/nscombobutton?language=objc")

(doc-objc-class "NSImageView"           ; ns-image-view
  "A display of image data in a frame."
  "see https://developer.apple.com/documentation/appkit/nsimageview?language=objc")

(doc-objc-class "NSLevelIndicator"      ; ns-level-indicator
  "A visual representation of a level or quantity, using discrete values."
  "see https://developer.apple.com/documentation/appkit/nslevelindicator?language=objc")

(doc-objc-class "NSPopUpButton"         ; ns-pop-up-button
  "A control for selecting an item from a list."
  "see https://developer.apple.com/documentation/appkit/nspopupbutton?language=objc")

(doc-objc-class "NSProgressIndicator"   ; ns-progress-indicator
  "An interface that provides visual feedback to the user about the status of an ongoing task."
  "see https://developer.apple.com/documentation/appkit/nsprogressindicator?language=objc")

(doc-objc-class "NSRuleEditor"          ; ns-rule-editor
  "An interface for configuring a rule-based list of options."
  "see https://developer.apple.com/documentation/appkit/nsruleeditor?language=objc")

(doc-objc-class "NSPredicateEditor"     ; ns-predicate-editor
  "A defined set of rules that allows the editing of predicate objects."
  "see https://developer.apple.com/documentation/appkit/nspredicateeditor?language=objc")

(doc-objc-class "NSSegmentedControl"    ; ns-segmented-control
  "Display one or more buttons in a single horizontal group."
  "see https://developer.apple.com/documentation/appkit/nssegmentedcontrol?language=objc")

(doc-objc-class "NSStepper"             ; ns-stepper
  "An interface with up and down arrow buttons for incrementing or decrementing a value."
  "see https://developer.apple.com/documentation/appkit/nsstepper?language=objc")

(doc-objc-class "NSSwitch"              ; ns-switch
  "A control that offers a binary choice."
  "see https://developer.apple.com/documentation/appkit/nsswitch?language=objc")

(doc-objc-class "NSMatrix"              ; ns-matrix
  "A legacy interface for grouping radio buttons or other types of cells together."
  "see https://developer.apple.com/documentation/appkit/nsmatrix?language=objc")

;;; Liquid Glass effects

(doc-objc-class "NSGlassEffectView"     ; ns-glass-effect-view
  "A view that embeds its content view in a dynamic glass effect."
  "see https://developer.apple.com/documentation/appkit/nsglasseffectview?language=objc")

(doc-objc-class "NSGlassEffectContainerView" ; ns-glass-effect-container-view
  "A view that efficiently merges descendant glass effect views together when they are within a specified proximity to each other."
  "see https://developer.apple.com/documentation/appkit/nsglasseffectcontainerview?language=objc")

;;; Interacting with adjacent views

(doc-objc-class "NSBackgroundExtensionView" ; ns-background-extension-view
  "A view that extends content to fill its own bounds."
  "see https://developer.apple.com/documentation/appkit/nsbackgroundextensionview?language=objc")

;;; Visual adornments

(doc-objc-class "NSVisualEffectView"    ; ns-visual-effect-view
  "A view that adds translucency and vibrancy effects to the views in your interface."
  "see https://developer.apple.com/documentation/appkit/nsvisualeffectview?language=objc")

(doc-objc-class "NSBox"                 ; ns-box
  "A stylized rectangular box with an optional title."
  "see https://developer.apple.com/documentation/appkit/nsbox?language=objc")

;;; UI validation

;;; Tool tips

;;; Related types



;;;; View Management
;; Manage your user interface, including the size and position of views in a window.
;; see https://developer.apple.com/documentation/appkit/view-management?language=objc

;;; Content Controllers

(doc-objc-class "NSWindowController"    ; ns-window-controller
  "A controller that manages a window, usually a window stored in a nib file."
  "see https://developer.apple.com/documentation/appkit/nswindowcontroller?language=objc")

(doc-objc-class "NSViewController"      ; ns-view-controller
  "A controller that manages a view, typically loaded from a nib file."
  "see https://developer.apple.com/documentation/appkit/nsviewcontroller?language=objc")

(doc-objc-class "NSTitlebarAccessoryViewController" ; ns-titlebar-accessory-view-controller
  "An object that manages a custom view—known as an accessory view-in
the title bar–toolbar area of a window."
  "see https://developer.apple.com/documentation/appkit/nstitlebaraccessoryviewcontroller?language=objc")

;;; Split View Interface

(doc-objc-class "NSSplitViewController" ; ns-split-view-controller
  "An object that manages an array of adjacent child views, and has a split view object for managing dividers between those views."
  "see https://developer.apple.com/documentation/appkit/nssplitviewcontroller?language=objc")

(doc-objc-class "NSSplitView"           ; ns-split-view
  "A view that arranges two or more views in a linear stack running horizontally or vertically."
  "see https://developer.apple.com/documentation/appkit/nssplitview?language=objc")

(doc-objc-class "NSSplitViewItem"       ; ns-split-view-item
  "An item in a split view controller."
  "see https://developer.apple.com/documentation/appkit/nssplitviewitem?language=objc")

;;; Stack View Interface

(doc-objc-class "NSStackView"           ; ns-stack-view
  "A view that arranges an array of views horizontally or vertically
and updates their placement and sizing when the window size changes."
  "see https://developer.apple.com/documentation/appkit/nsstackview?language=objc")

;;; Tab View Interface

(doc-objc-class "NSTabViewController"   ; ns-tab-view-controller
  "A container view controller that manages a tab view interface,
which organizes multiple pages of content but displays only one page at a time."
  "see https://developer.apple.com/documentation/appkit/nstabviewcontroller?language=objc")

(doc-objc-class "NSTabView"             ; ns-tab-view
  "A multipage interface that displays one page at a time."
  "see https://developer.apple.com/documentation/appkit/nstabview?language=objc")

(doc-objc-class "NSTabViewItem"         ; ns-tab-view-item
  "An item in a tab view."
  "see https://developer.apple.com/documentation/appkit/nstabviewitem?language=objc")

;;; Paged Interface

(doc-objc-class "NSPageController"      ; ns-page-controller
  "An object that controls swipe navigation and animations between views or view content."
  "see https://developer.apple.com/documentation/appkit/nspagecontroller?language=objc")

;;; Media Library Interface

(doc-objc-class "NSMediaLibraryBrowserController" ; ns-media-library-browser-controller
  "An object that configures and displays a Media Library Browser panel."
  "see https://developer.apple.com/documentation/appkit/nsmedialibrarybrowsercontroller?language=objc")


;;;; View Layout
;; Position and size views using a stack view or Auto Layout constraints.
;; see https://developer.apple.com/documentation/appkit/view-layout?language=objc

;;; Stack View

(doc-objc-class "NSStackView"           ; ns-stack-view
  "A view that arranges an array of views horizontally or vertically
and updates their placement and sizing when the window size changes."
  "see https://developer.apple.com/documentation/appkit/nsstackview?language=objc")

;;; Auto Layout Constraints

(doc-objc-class "NSLayoutConstraint"    ; ns-layout-constraint
  "The relationship between two user interface objects that must be satisfied by
the constraint-based layout system."
  "see https://developer.apple.com/documentation/appkit/nslayoutconstraint?language=objc")

;;; Layout Guides

(doc-objc-class "NSLayoutGuide"         ; ns-layout-guide
  "A rectangular area that can interact with Auto Layout."
  "see https://developer.apple.com/documentation/appkit/nslayoutguide?language=objc")

(doc-objc-class "NSLayoutDimension"     ; ns-layout-dimension
  "A factory class for creating size-based layout constraint objects using a fluent API."
  "see https://developer.apple.com/documentation/appkit/nslayoutdimension?language=objc")

;;; Anchors

(doc-objc-class "NSLayoutAnchor"        ; ns-layout-anchor
  "A factory class for creating layout constraint objects using a fluent API."
  "see https://developer.apple.com/documentation/appkit/nslayoutanchor?language=objc")

(doc-objc-class "NSLayoutXAxisAnchor"   ; ns-layout-x-axis-anchor
  "A factory class for creating horizontal layout constraint objects using a fluent API."
  "see https://developer.apple.com/documentation/appkit/nslayoutxaxisanchor?language=objc")

(doc-objc-class "NSLayoutYAxisAnchor"   ; ns-layout-y-axis-anchor
  "A factory class for creating vertical layout constraint objects using a fluent API."
  "see https://developer.apple.com/documentation/appkit/nslayoutyaxisanchor?language=objc")

;;; View Compression


;;;; Appearance Customization
;; Add Dark Mode support to your app, and use appearance proxies to modify your UI.
;; see https://developer.apple.com/documentation/appkit/appearance-customization?language=objc

;;; Dark Mode

;;; Appearance System

(doc-objc-class "NSAppearance"          ; ns-appearance
  "An object that manages standard appearance attributes for UI elements in an app."
  "see https://developer.apple.com/documentation/appkit/nsappearance?language=objc")


;;;; Animation
;; Animate your views and other content to create a more engaging experience for users.
;; see https://developer.apple.com/documentation/appkit/animation?language=objc

;;; View-Based Animations

(doc-objc-class "NSViewAnimation"       ; ns-view-animation
  "An animation of an app’s views, limited to changes in frame location and size,
and to fade-in and fade-out effects."
  "see https://developer.apple.com/documentation/appkit/nsviewanimation?language=objc")

(doc-objc-class "NSAnimationContext"    ; ns-animation-context
  "An animation context, which contains information about environment and state."
  "see https://developer.apple.com/documentation/appkit/nsanimationcontext?language=objc")

;;; Presentations

;;; Custom Animations

(doc-objc-class "NSAnimation"           ; ns-animation
  "An object that manages the timing and progress of animations in the user interface."
  "see https://developer.apple.com/documentation/appkit/nsanimation?language=objc")

;;; System Animations


;;;; Windows, Panels, and Screens
;; Organize your view hierarchies and facilitate their display onscreen.
;; see https://developer.apple.com/documentation/appkit/windows-panels-and-screens?language=objc

;;; Windows

(define-objc-enum ns-window-style-mask
  "Constants that specify the style of a window,
and that you can combine with the C bitwise OR operator.

See https://developer.apple.com/documentation/appkit/nswindow/stylemask-swift.struct?language=objc"
  (:borderless                        0  "The window displays none of the usual peripheral elements. ")
  (:titled                    (ash 1  0) "The window displays a title bar. ")
  (:closable                  (ash 1  1) "The window displays a close button. ")
  (:miniaturizable            (ash 1  2) "The window displays a minimize button. ")
  (:resizable                 (ash 1  3) "The window can be resized by the user. ")
  (:textured-background       (ash 1  8) "Deprecated"
                              "The window uses a textured background that darkens"
                              "when the window is key or main and lightens when it is inactive,"
                              "and may have a second gradient in the section below the window content.")
  (:unified-title-and-toolbar (ash 1 12) "This constant has no effect,"
                              "because all windows that include a toolbar use the unified style. ")
  (:full-screen               (ash 1 14) "The window can appear full screen. "
                              "A fullscreen window does not draw its title bar, "
                              "and may have special handling for its toolbar. "
                              "(This mask is automatically toggled when toggleFullScreen: is called.)")
  (:full-size-content-view    (ash 1 15)
                              "When set, the window's contentView consumes the full size of the window."
                              "Although you can combine this constant with other window style masks, "
                              "it is respected only for windows with a title bar. "
                              "Note that using this mask opts in to layer-backing. "
                              "Use the contentLayoutRect or the contentLayoutGuide to lay out views "
                              "underneath the title bar–toolbar area.")
  (:utility-window            (ash 1  4) "The window is a panel or a subclass of `ns-panel'.")
  (:doc-modal-window          (ash 1  6) "The window is a document-modal panel (or a subclass of `ns-panel').")
  (:nonactivating-panel       (ash 1  7) "The window is a panel or a subclass of NSPanel"
                              "that does not activate the owning app.")
  (:hud-window                (ash 1 13) "The window is a HUD panel. "))

(doc-objc-class "NSWindow"              ; ns-window
  (("contentViewController"
    :reader content-view-controller
    :documentation "Get the main content view controller for the window.

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
    :documentation "Get the window’s content view,
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
   ("styleMask"
    :accessor style-mask
    :after  decode-ns-window-style-mask
    :before ns-window-style-mask
    :documentation "Flags that describe the window’s current style,
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
    :documentation "The window's alpha value.
see https://developer.apple.com/documentation/appkit/nswindow/alphavalue?language=objc")
   ("backgroundColor"
    :accessor background-color
    :documentation "The color of window's background.
see https://developer.apple.com/documentation/appkit/nswindow/backgroundcolor?language=objc")
   ("colorSpace"
    :accessor color-space
    :documentation "The window’s color space.

The value of this property is nil if the window does not have a
backing store, and is off-screen.

see https://developer.apple.com/documentation/appkit/nswindow/colorspace?language=objc")
   ("canHide"
    :accessor can-hide
    :documentation "A Boolean value that indicates whether the window can hide
when its application becomes hidden.

The value of this property is true if the window can hide when its
application becomes hidden (during execution of the `ns-application'
`ns-application' `hide' method); otherwise, `nil'.
By default, the value of the property is `t'.

see https://developer.apple.com/documentation/appkit/nswindow/canhide?language=objc")
   ("onActiveSpace"
    :accessor on-active-space
    :documentation "Whether the window is on the currently active space.

The value of this property is true if the window is on the currently
active space; otherwise, false. For visible windows, this property
indicates whether the window is currently visible on the active
space. For nonvisible windows, it indicates whether ordering the
window onscreen would cause it to be on the active space.

see https://developer.apple.com/documentation/appkit/nswindow/isonactivespace?language=objc")
   ("hidesOnDeactivate"
    :reader hides-on-deactivate
    :documentation "Whether the window is removed from the screen
when its application becomes inactive.

The value of this property is true if the window is removed from the
screen when its application is deactivated; false if it remains
onscreen. The default value for NSWindow is false; the default value
for NSPanel is true.

see https://developer.apple.com/documentation/appkit/nswindow/hidesondeactivate?language=objc")
   ("collectionBehavior"
    :accessor collection-behavior
    :documentation "A value that identifies the window’s behavior in window collections.

The possible values for this property are listed in `ns-window-collection-behavior'.

see https://developer.apple.com/documentation/appkit/nswindow/collectionbehavior-swift.property?language=objc")
   ("opaque"
    :accessor opaque
    :documentation "Whether the window is opaque.
see https://developer.apple.com/documentation/appkit/nswindow/isopaque?language=objc")
   ("hasShadow"
    :accessor has-shadow
    :documentation "Whether the window has a shadow.
see https://developer.apple.com/documentation/appkit/nswindow/hasshadow?language=objc")
   ("visible"
    :accessor visible
    :setter  "setIsVisible:"
    :documentation "If window is visible on screen.

Dev Note:
Using (setf visible-p) would invoke setIsVisible: method.

see https://developer.apple.com/documentation/appkit/nswindow/isvisible?language=objc"))
  "A window that an app displays on the screen. "
  "A single `ns-window' object corresponds to, at most, one on-screen window.
Windows perform two principal functions:
+ To place views in a provided area
+ To accept and distribute mouse and keyboard events the user generates to the appropriate views"
  "Notes:
Although the `ns-window' class inherits the `ns-coding' protocol from `ns-responder',
the class doesn't support coding. Legacy support for archivers exists, but its use is
deprecated and may not work. Any attempt to archive or unarchive a window object using a
keyed coding object raises an `ns-invalid-argument-exception' exception.
For details about window restoration, see restorationClass."
  "see https://developer.apple.com/documentation/appkit/nswindow?language=objc")

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

(defun ns-window-init (ns-rect &key
                                 (style   *ns-window-style*)
                                 (backing *ns-backing-store*)
                                 (defer   t)
                                 screen
                       &allow-other-keys)
  "Initializes the window with the specified values.
Return the initialized `ns-window'.

Parameters:
+ NS-RECT: `ns-rect'
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

Dev Note:
this invokes
+ initWithContentRect:styleMask:backing:defer:screen:
+ initWithContentRect:styleMask:backing:defer:

see https://developer.apple.com/documentation/appkit/nswindow/init(contentrect:stylemask:backing:defer:)?language=objc
see https://developer.apple.com/documentation/appkit/nswindow/init(contentrect:stylemask:backing:defer:screen:)?language=objc"
  (declare (type ns-rect ns-rect)
           (type (satisfies ns-window-style-mask-p)  style)
           (type (satisfies ns-backing-store-type-p) backing)
           (type (or null ns-screen)                 screen))
  (if screen
      (invoke (alloc 'ns-window)
              "initWithContentRect:styleMask:backing:defer:screen:"
              ns-rect
              (ns-window-style-mask style)
              (ns-backing-store-type backing)
              (and defer t)
              screen)
      (invoke (alloc 'ns-window)
              "initWithContentRect:styleMask:backing:defer:"
              ns-rect
              (ns-window-style-mask style)
              (ns-backing-store-type backing)
              (and defer t))))

;; Managing the Window's Behavior

;; Configuring the Window's Content

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

;; (defmethod main-window-p ((window ns-window))
;;   "Test if WINDOW is application's main window.
;; see https://developer.apple.com/documentation/appkit/nswindow/ismainwindow?language=objc"
;;   (invoke window "mainWindow"))

;; (defmethod can-become-main-window-p ((window ns-window))
;;   "Test if WINDOW can become the application's main window.
;; see https://developer.apple.com/documentation/appkit/nswindow/canbecomemain?language=objc"
;;   (invoke window "canBecomeMainWindow"))

;; (defmethod make-main-window ((window ns-window))
;;   "Makes the window the main window.
;; see https://developer.apple.com/documentation/appkit/nswindow/makemain()?language=objc"
;;   (invoke window "makeMainWindow"))

;; Managing Toolbars

;; (defmethod toolbar ((window ns-window))
;;   "Get the WINDOW's toolbar
;; see https://developer.apple.com/documentation/appkit/nswindow/toolbar?language=objc"
;;   (invoke window "toolbar"))

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

;; Notifications

(doc-objc-class "NSPanel"               ; ns-panel
  "A special kind of window that typically performs a function that is auxiliary to the main window. "
  "For details about how panels work,
especially to find out how their behavior differs from window behavior, see How Panels Work:
https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/WinPanel/Concepts/UsingPanels.html#//apple_ref/doc/uid/20000224"
  "see https://developer.apple.com/documentation/appkit/nspanel?language=objc")

;; Configuring Panels

;; Constants

(doc-objc-class "NSWindowTab"           ; ns-window-tab
  "A tab associated with a window that is part of a tabbing group. "
  "see https://developer.apple.com/documentation/appkit/nswindowtab?language=objc")

;; Customizing the Title

;; Customizing the Tooltip

;; Adding an Accessory View

;; Relationships

(doc-objc-class "NSWindowTabGroup"      ; ns-window-tab-group
  "A group of windows that display together as a single tabbed window."
  "see https://developer.apple.com/documentation/appkit/nswindowtabgroup?language=objc")

;; Checking the Group Identifier

;; Configuring the Tab User Interface

;; Managing Tabbed Windows

;;; Window Restoration

;;; Screens

(doc-objc-class "NSScreen"              ; ns-screen
  "An object that describes the attributes of a computer’s monitor or screen. "
  "An app may use an NSScreen object to retrieve information about a
screen and use this information to decide what to display on that
screen. For example, an app may use the deepestScreen method to find
out which of the available screens can best represent color and then
might choose to display all of its windows on that screen."
  "Create the application object before you use the methods in this
class, so that the application object can make the necessary
connection to the window system. You can make sure the application
object exists by invoking the sharedApplication method of
NSApplication. If you created your app with Xcode, the application
object is automatically created for you during initialization."
  "Note
The NSScreen class is only for getting information about the available
displays. If you need additional information or want to change the
attributes relating to a display, you must use Quartz Services. For
more information, see Quartz Display Services."
  "see https://developer.apple.com/documentation/appkit/nsscreen?language=objc")

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

(doc-objc-class "NSPopover"             ; ns-popover
  "A means to display additional content related to existing content on the screen. "
  "The popover is positioned relative to the existing content and an
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
dragged by implementing the appropriate delegate method."
  "see https://developer.apple.com/documentation/appkit/nspopover?language=objc")

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

(doc-objc-class "NSAlert"               ; ns-alert
  "A modal dialog or sheet attached to a document window. "
  "see https://developer.apple.com/documentation/appkit/nsalert?language=objc")

;;; Open and Save Panels

(doc-objc-class "NSOpenPanel"           ; ns-open-panel
  "A panel that prompts the user to select a file to open. "
  "see https://developer.apple.com/documentation/appkit/nsopenpanel?language=objc")

(doc-objc-class "NSSavePanel"           ; ns-save-panel
  "A panel that prompts the user for information about where to save a file. "
  "see https://developer.apple.com/documentation/appkit/nssavepanel?language=objc")

;;; Share Panel

(doc-objc-class "NSSharingServicePicker" ; ns-sharing-service-picker
  "A list of sharing services that the user can choose from. "
  "see https://developer.apple.com/documentation/appkit/nssharingservicepicker?language=objc")

(doc-objc-class "NSPreviewRepresentingActivityItem" ; ns-preview-representing-activity-item
  "A type that adds metadata to an item you share using the macOS share sheet. "
  "see https://developer.apple.com/documentation/appkit/nspreviewrepresentingactivityitem?language=objc")

;;; Print and PDF Panels

(doc-objc-class "NSPDFPanel"            ; ns-pdf-panel
  "A Save or Export as PDF panel that’s consistent with the macOS user interface."
  "see https://developer.apple.com/documentation/appkit/nspdfpanel?language=objc")

;;; Color Panels

(doc-objc-class "NSColorPanel"          ; ns-color-panel
  "A standard user interface for selecting color in an app. "
  "see https://developer.apple.com/documentation/appkit/nscolorpanel?language=objc")

(doc-objc-class "NSColorPicker"         ; ns-color-picker
  "An abstract superclass that implements the default color picking protocol."
  "see https://developer.apple.com/documentation/appkit/nscolorpicker?language=objc")

;;; Font Panels

(doc-objc-class "NSFontPanel"           ; ns-font-panel
  "The Font panel a user interface object that displays a list of available fonts,
letting the user preview them and change the font used to display text. "
  "see https://developer.apple.com/documentation/appkit/nsfontpanel?language=objc")

;;; User Interface



;;;; Sound, Speech, and Haptics
;; Play sounds and haptic feedback, and incorporate speech recognition and synthesis into your interface.
;; see https://developer.apple.com/documentation/appkit/sound-speech-and-haptics?language=objc

;;; Sounds

(doc-objc-class "NSSound"               ; ns-sound
  "A simple interface for loading and playing audio files."
  "see https://developer.apple.com/documentation/appkit/nssound?language=objc")

;;; Speech

(doc-objc-class "NSSpeechRecognizer"    ; ns-speech-recognizer
  "The Cocoa interface to speech recognition in macOS."
  "see https://developer.apple.com/documentation/appkit/nsspeechrecognizer?language=objc")

(doc-objc-class "NSSpeechSynthesizer"   ; ns-speech-synthesizer
  "The Cocoa interface to speech synthesis in macOS."
  "see https://developer.apple.com/documentation/appkit/nsspeechsynthesizer?language=objc")

;;; Haptics

(doc-objc-class "NSHapticFeedbackManager" ; ns-haptic-feedback-manager
  "An object that provides access to the haptic feedback management attributes on a system with a Force Touch trackpad."
  "see https://developer.apple.com/documentation/appkit/nshapticfeedbackmanager?language=objc")

(doc-objc-class "NSAlignmentFeedbackFilter" ; ns-alignment-feedback-filter
  "An object that can filter the movement of an object and provides haptic feedback when alignment occurs."
  "see https://developer.apple.com/documentation/appkit/nsalignmentfeedbackfilter?language=objc")


;;;; Supporting Continuity Camera in Your Mac App
;; Incorporate scanned documents and pictures from a user’s iPhone, iPad, or iPod touch
;; into your Mac app using Continuity Camera.
;; see https://developer.apple.com/documentation/appkit/supporting-continuity-camera-in-your-mac-app?language=objc


;;;; Mouse, Keyboard, and Trackpad
;; Handle events related to mouse, keyboard, and trackpad input.
;; see https://developer.apple.com/documentation/appkit/mouse-keyboard-and-trackpad?language=objc

(define-objc-enum ns-event-mask
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

(define-objc-enum ns-event-modifier-flags
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

(doc-objc-class "NSEvent"               ; ns-event
  (("type"
    :reader event-type
    :after  decode-ns-event-type
    :documentation
    "The event’s type.
See https://developer.apple.com/documentation/appkit/nsevent/type?language=objc")
   ("subtype"
    :reader event-subtype
    :after  decode-ns-event-subtype
    :documentation
    "The event’s subtype.
See https://developer.apple.com/documentation/appkit/nsevent/subtype?language=objc")
   ("modifierFlags"
    :reader modifier-flags
    :after  decode-ns-event-modifier-flags
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
  "An object that contains information about an input action,
such as a mouse click or a key press."
  "AppKit reports events that occur in a window to the app that created
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
events systemwide, although without the ability to modify them."
  "see https://developer.apple.com/documentation/appkit/nsevent?language=objc")


;;;; Menus, Cursors, and the Dock
;; Implement menus and cursors to facilitate interactions with your app, and use your
;; app’s Dock tile to convey updated information.
;; see https://developer.apple.com/documentation/appkit/menus-cursors-and-the-dock?language=objc

;;; Menus

(doc-objc-class "NSMenu"                ; ns-menu
  "An object that manages an app’s menus."
  "see https://developer.apple.com/documentation/appkit/nsmenu?language=objc")

(doc-objc-class "NSMenuItem"            ; ns-menu-item
  "A command item in an app menu."
  "see https://developer.apple.com/documentation/appkit/nsmenuitem?language=objc")

(doc-objc-class "NSMenuItemBadge"       ; ns-menu-item-badge
  "A control that provides additional quantitative information specific to a menu item, such as the number of available updates."
  "see https://developer.apple.com/documentation/appkit/nsmenuitembadge?language=objc")

;;; Menu Validation

;;; Menu Bar Items

(doc-objc-class "NSStatusBar"           ; ns-status-bar
  "An object that manages a collection of status items displayed within the system-wide menu bar."
  "see https://developer.apple.com/documentation/appkit/nsstatusbar?language=objc")

(doc-objc-class "NSStatusItem"          ; ns-status-item
  "An individual element displayed in the system menu bar."
  "see https://developer.apple.com/documentation/appkit/nsstatusitem?language=objc")

(doc-objc-class "NSStatusBarButton"     ; ns-status-bar-button
  "The appearance and behavior of an item in the systemwide menu bar."
  "see https://developer.apple.com/documentation/appkit/nsstatusbarbutton?language=objc")

;;; Cursors

(doc-objc-class "NSCursor"              ; ns-cursor
  "A pointer (also called a cursor)."
  "see https://developer.apple.com/documentation/appkit/nscursor?language=objc")

(doc-objc-class "NSTrackingArea"        ; ns-tracking-area
  "A region of a view that generates mouse-tracking and cursor-update events when the pointer is over that region."
  "see https://developer.apple.com/documentation/appkit/nstrackingarea?language=objc")

;;; The Dock

(doc-objc-class "NSDockTile"            ; ns-dock-tile
  "The visual representation of your app’s miniaturized windows and app icon as they appear in the Dock."
  "see https://developer.apple.com/documentation/appkit/nsdocktile?language=objc")


;;;; Gestures
;; Encapsulate your app’s event-handling logic in gesture recognizers so that you can reuse
;; that code throughout your app.
;; see https://developer.apple.com/documentation/appkit/gestures?language=objc

;;; Standard Gestures

(doc-objc-class "NSClickGestureRecognizer" ; ns-click-gesture-recognizer
  "A discrete gesture recognizer that tracks a specified number of mouse clicks."
  "see https://developer.apple.com/documentation/appkit/nsclickgesturerecognizer?language=objc")

(doc-objc-class "NSPressGestureRecognizer" ; ns-press-gesture-recognizer
  "A discrete gesture recognizer that tracks whether the user holds down a mouse button for a minimum amount of time before releasing it."
  "see https://developer.apple.com/documentation/appkit/nspressgesturerecognizer?language=objc")

(doc-objc-class "NSPanGestureRecognizer" ; ns-pan-gesture-recognizer
  "A continuous gesture recognizer for panning gestures."
  "see https://developer.apple.com/documentation/appkit/nspangesturerecognizer?language=objc")

(doc-objc-class "NSRotationGestureRecognizer" ; ns-rotation-gesture-recognizer
  "A continuous gesture recognizer that tracks two trackpad touches moving opposite each other in a circular motion."
  "see https://developer.apple.com/documentation/appkit/nsrotationgesturerecognizer?language=objc")

(doc-objc-class "NSMagnificationGestureRecognizer" ; ns-magnification-gesture-recognizer
  "A continuous gesture recognizer that tracks a pinch gesture that magnifies content."
  "see https://developer.apple.com/documentation/appkit/nsmagnificationgesturerecognizer?language=objc")

;;; Custom Gestures

(doc-objc-class "NSGestureRecognizer"   ; ns-gesture-recognizer
  "An object that monitors events and calls its action method when a predefined sequence of events occur."
  "see https://developer.apple.com/documentation/appkit/nsgesturerecognizer?language=objc")


;;;; Touch Bar
;; Display interactive content and controls in the Touch Bar.
;; see https://developer.apple.com/documentation/appkit/touch-bar?language=objc

;;; Essentials

(doc-objc-class "NSTouchBar"            ; ns-touch-bar
  "An object that provides dynamic contextual controls in the Touch Bar of supported models of MacBook Pro."
  "see https://developer.apple.com/documentation/appkit/nstouchbar?language=objc")

;;; Touch Bar items

(doc-objc-class "NSTouchBarItem"        ; ns-touch-bar-item
  "A UI control shown in the Touch Bar on supported models of MacBook Pro."
  "see https://developer.apple.com/documentation/appkit/nstouchbaritem?language=objc")

(doc-objc-class "NSCandidateListTouchBarItem" ; ns-candidate-list-touch-bar-item
  "A bar item that, along with its delegate, provides a list of textual suggestions for the current text view."
  "see https://developer.apple.com/documentation/appkit/nscandidatelisttouchbaritem?language=objc")

(doc-objc-class "NSColorPickerTouchBarItem" ; ns-color-picker-touch-bar-item
  "A bar item that provides a system-defined color picker."
  "see https://developer.apple.com/documentation/appkit/nscolorpickertouchbaritem?language=objc")

(doc-objc-class "NSCustomTouchBarItem"  ; ns-custom-touch-bar-item
  "A bar item that contains a responder of your choice, such as a view, a button, or a scrubber."
  "see https://developer.apple.com/documentation/appkit/nscustomtouchbaritem?language=objc")

(doc-objc-class "NSGroupTouchBarItem"   ; ns-group-touch-bar-item
  "A bar item that provides a bar to contain other items."
  "see https://developer.apple.com/documentation/appkit/nsgrouptouchbaritem?language=objc")

(doc-objc-class "NSPopoverTouchBarItem" ; ns-popover-touch-bar-item
  "A bar item that provides a two-state control that can expand into its second state, showing the contents of a bar that it owns."
  "see https://developer.apple.com/documentation/appkit/nspopovertouchbaritem?language=objc")

(doc-objc-class "NSSharingServicePickerTouchBarItem" ; ns-sharing-service-picker-touch-bar-item
  "A bar item that, along with its delegate, provides a list of objects eligible for sharing."
  "see https://developer.apple.com/documentation/appkit/nssharingservicepickertouchbaritem?language=objc")

(doc-objc-class "NSSliderTouchBarItem"  ; ns-slider-touch-bar-item
  "A bar item that provides a slider control for choosing a value in a range."
  "see https://developer.apple.com/documentation/appkit/nsslidertouchbaritem?language=objc")

(doc-objc-class "NSStepperTouchBarItem" ; ns-stepper-touch-bar-item
  "A bar item that provides a stepper control for incrementing or decrementing a value."
  "see https://developer.apple.com/documentation/appkit/nssteppertouchbaritem?language=objc")

(doc-objc-class "NSUserInterfaceCompressionOptions" ; ns-user-interface-compression-options
  "An object that specifies how user interface elements resize themselves when space is constrained."
  "see https://developer.apple.com/documentation/appkit/nsuserinterfacecompressionoptions?language=objc")

(doc-objc-class "NSButtonTouchBarItem"  ; ns-button-touch-bar-item
  "A bar item that provides a button."
  "see https://developer.apple.com/documentation/appkit/nsbuttontouchbaritem?language=objc")

(doc-objc-class "NSPickerTouchBarItem"  ; ns-picker-touch-bar-item
  "A bar item that provides a picker control with multiple options."
  "see https://developer.apple.com/documentation/appkit/nspickertouchbaritem?language=objc")

;;; Scrubbers

(doc-objc-class "NSScrubber"            ; ns-scrubber
  "A customizable item picker control for the Touch Bar."
  "see https://developer.apple.com/documentation/appkit/nsscrubber?language=objc")

;;; Scrubber items

(doc-objc-class "NSScrubberItemView"    ; ns-scrubber-item-view
  "An item at a specific index position in the scrubber."
  "see https://developer.apple.com/documentation/appkit/nsscrubberitemview?language=objc")

(doc-objc-class "NSScrubberArrangedView" ; ns-scrubber-arranged-view
  "An abstract base class for the views whose layout is managed by a scrubber."
  "see https://developer.apple.com/documentation/appkit/nsscrubberarrangedview?language=objc")

(doc-objc-class "NSScrubberImageItemView" ; ns-scrubber-image-item-view
  "A concrete view subclass for displaying images in a scrubber items."
  "see https://developer.apple.com/documentation/appkit/nsscrubberimageitemview?language=objc")

(doc-objc-class "NSScrubberSelectionStyle" ; ns-scrubber-selection-style
  "An abstract class that provides decorative accessory views for selected and highlighted items within a scrubber control."
  "see https://developer.apple.com/documentation/appkit/nsscrubberselectionstyle?language=objc")

(doc-objc-class "NSScrubberSelectionView" ; ns-scrubber-selection-view
  "An abstract base class for specifying the appearance of a highlighted or selected item in a scrubber."
  "see https://developer.apple.com/documentation/appkit/nsscrubberselectionview?language=objc")

(doc-objc-class "NSScrubberTextItemView" ; ns-scrubber-text-item-view
  "A concrete view subclass for displaying text for an item in a scrubber."
  "see https://developer.apple.com/documentation/appkit/nsscrubbertextitemview?language=objc")

;;; Scrubber layouts

(doc-objc-class "NSScrubberFlowLayout"  ; ns-scrubber-flow-layout
  "A concrete layout object that arranges items end-to-end in a linear strip."
  "see https://developer.apple.com/documentation/appkit/nsscrubberflowlayout?language=objc")

(doc-objc-class "NSScrubberProportionalLayout" ; ns-scrubber-proportional-layout
  "A concrete layout object that sizes each item to some fraction of the scrubber’s visible size."
  "see https://developer.apple.com/documentation/appkit/nsscrubberproportionallayout?language=objc")

(doc-objc-class "NSScrubberLayoutAttributes" ; ns-scrubber-layout-attributes
  "The layout of a scrubber item."
  "see https://developer.apple.com/documentation/appkit/nsscrubberlayoutattributes?language=objc")

(doc-objc-class "NSScrubberLayout"      ; ns-scrubber-layout
  "An abstract class that describes the layout of items within a scrubber control."
  "see https://developer.apple.com/documentation/appkit/nsscrubberlayout?language=objc")


;;;; Drag and Drop
;; Support the direct manipulation of your app’s content using drag and drop.
;; see https://developer.apple.com/documentation/appkit/drag-and-drop?language=objc

;;; Drag Sources

(doc-objc-class "NSDraggingItem"        ; ns-dragging-item
  "A single dragged item within a dragging session."
  "see https://developer.apple.com/documentation/appkit/nsdraggingitem?language=objc")

(doc-objc-class "NSDraggingSession"     ; ns-dragging-session
  "The encapsulation of a drag-and-drop action that supports modification of the drag while in progress."
  "see https://developer.apple.com/documentation/appkit/nsdraggingsession?language=objc")

(doc-objc-class "NSDraggingImageComponent" ; ns-dragging-image-component
  "A single object in a dragging item."
  "see https://developer.apple.com/documentation/appkit/nsdraggingimagecomponent?language=objc")

;;; Drop Targets


;;;; Accessibility for AppKit
;; Make your AppKit apps accessible to everyone who uses macOS.
;; see https://developer.apple.com/documentation/appkit/accessibility-for-appkit?language=objc

;;; Essentials

;;; AppKit Elements

;;; Custom View Subclasses

;;; Custom Elements

(doc-objc-class "NSAccessibilityElement" ; ns-accessibility-element
  "The basic infrastructure necessary for interacting with an assistive app."
  "see https://developer.apple.com/documentation/appkit/nsaccessibilityelement-swift.class?language=objc")

;;; Accessibility Types


;;;; Images and PDF
;; Create and manage images, in bitmap, PDF, and other formats.
;; see https://developer.apple.com/documentation/appkit/images-and-pdf?language=objc

;;; Images

(doc-objc-class "NSImage"               ; ns-image
  "A high-level interface for manipulating image data."
  "see https://developer.apple.com/documentation/appkit/nsimage?language=objc")

(doc-objc-class "NSImageRep"            ; ns-image-rep
  "A semiabstract superclass that provides subclasses that you use to draw an image from a particular type of source data."
  "see https://developer.apple.com/documentation/appkit/nsimagerep?language=objc")

;;; Bitmap Formats

(doc-objc-class "NSBitmapImageRep"      ; ns-bitmap-image-rep
  "An object that renders an image from bitmap data."
  "see https://developer.apple.com/documentation/appkit/nsbitmapimagerep?language=objc")

(doc-objc-class "NSCIImageRep"          ; ns-ci-image-rep
  "An object that can render an image from a Core Image object."
  "see https://developer.apple.com/documentation/appkit/nsciimagerep?language=objc")

(doc-objc-class "NSPICTImageRep"        ; ns-pict-image-rep
  "An object that renders an image from a PICT format data stream of version 1, version 2, and extended version 2."
  "see https://developer.apple.com/documentation/appkit/nspictimagerep?language=objc")

;;; Vector Formats

(doc-objc-class "NSPDFImageRep"         ; ns-pdf-image-rep
  "An object that can render an image from a PDF format data stream."
  "see https://developer.apple.com/documentation/appkit/nspdfimagerep?language=objc")

(doc-objc-class "NSPDFInfo"             ; ns-pdf-info
  "An object that stores information associated with the creation of a PDF file, such as its URL, tag names, page orientation, and paper size."
  "see https://developer.apple.com/documentation/appkit/nspdfinfo?language=objc")

(doc-objc-class "NSEPSImageRep"         ; ns-eps-image-rep
  "An object that can render an image from encapsulated PostScript (EPS) code."
  "see https://developer.apple.com/documentation/appkit/nsepsimagerep?language=objc")

;;; Custom Formats

(doc-objc-class "NSCustomImageRep"      ; ns-custom-image-rep
  "An object that uses a delegate object to render an image from a custom format."
  "see https://developer.apple.com/documentation/appkit/nscustomimagerep?language=objc")


;;;; Drawing
;; Draw shapes, images, and other content on the screen.
;; see https://developer.apple.com/documentation/appkit/drawing?language=objc

;;; Drawing Contexts

(doc-objc-class "NSGraphicsContext"     ; ns-graphics-context
  "An object that represents a graphics context."
  "see https://developer.apple.com/documentation/appkit/nsgraphicscontext?language=objc")

;;; Shapes and Paths

(doc-objc-class "NSBezierPath"          ; ns-bezier-path
  "An object that can create paths using PostScript-style commands."
  "see https://developer.apple.com/documentation/appkit/nsbezierpath?language=objc")

;;; Strings

(doc-objc-class "NSStringDrawingContext" ; ns-string-drawing-context
  "An object that manages metrics for drawing attributed strings."
  "see https://developer.apple.com/documentation/appkit/nsstringdrawingcontext?language=objc")

;;; Gradients

(doc-objc-class "NSGradient"            ; ns-gradient
  "An object that can draw gradient fill colors"
  "see https://developer.apple.com/documentation/appkit/nsgradient?language=objc")

;;; Shadows

(doc-objc-class "NSShadow"              ; ns-shadow
  "An object you use to specify attributes to create and style a drop shadow during drawing operations."
  "see https://developer.apple.com/documentation/appkit/nsshadow?language=objc")


;;;; Color
;; Represent colors using built-in or custom formats, and give users options for selecting
;; and applying colors.
;; see https://developer.apple.com/documentation/appkit/color?language=objc

;;; Colors

(doc-objc-class "NSColor"               ; ns-color
  "An object that stores color data and sometimes opacity (alpha value)."
  "see https://developer.apple.com/documentation/appkit/nscolor?language=objc")

(doc-objc-class "NSColorList"           ; ns-color-list
  "An ordered list of color objects, identified by keys."
  "see https://developer.apple.com/documentation/appkit/nscolorlist?language=objc")

(doc-objc-class "NSColorSpace"          ; ns-color-space
  "An object that represents a custom color space."
  "see https://developer.apple.com/documentation/appkit/nscolorspace?language=objc")

;;; Color Selection

(doc-objc-class "NSColorPicker"         ; ns-color-picker
  "An abstract superclass that implements the default color picking protocol."
  "see https://developer.apple.com/documentation/appkit/nscolorpicker?language=objc")

(doc-objc-class "NSColorWell"           ; ns-color-well
  "A control that displays a color value and lets the user change that color value."
  "see https://developer.apple.com/documentation/appkit/nscolorwell?language=objc")

(doc-objc-class "NSColorPickerTouchBarItem" ; ns-color-picker-touch-bar-item
  "A bar item that provides a system-defined color picker."
  "see https://developer.apple.com/documentation/appkit/nscolorpickertouchbaritem?language=objc")

;;; Color Sampler

(doc-objc-class "NSColorSampler"        ; ns-color-sampler
  "An object that displays the system’s color-sampling interface and
returns the selected color to your app."
  "see https://developer.apple.com/documentation/appkit/nscolorsampler?language=objc")


;;;; Printing
;; Display the system print panels and manage the printing process.
;; see https://developer.apple.com/documentation/appkit/printing?language=objc

;;; Print Panels

(doc-objc-class "NSPrintPanel"          ; ns-print-panel
  "The Print panel that queries the user for information about a print job."
  "see https://developer.apple.com/documentation/appkit/nsprintpanel?language=objc")

(doc-objc-class "NSPageLayout"          ; ns-page-layout
  "A panel that queries the user for information such as paper type and orientation."
  "see https://developer.apple.com/documentation/appkit/nspagelayout?language=objc")

;;; Print Information

(doc-objc-class "NSPrinter"             ; ns-printer
  "An object that describes a printer’s capabilities."
  "see https://developer.apple.com/documentation/appkit/nsprinter?language=objc")

(doc-objc-class "NSPrintInfo"           ; ns-print-info
  "An object that stores information that’s used to generate printed output."
  "see https://developer.apple.com/documentation/appkit/nsprintinfo?language=objc")

(doc-objc-class "NSPrintOperation"      ; ns-print-operation
  "An object that controls operations that generate Encapsulated PostScript (EPS) code, Portable Document Format (PDF) code, or print jobs."
  "see https://developer.apple.com/documentation/appkit/nsprintoperation?language=objc")


;;;; Text Display
;; Display text and check spelling.
;; see https://developer.apple.com/documentation/appkit/text-display?language=objc

;;; Text views

(doc-objc-class "NSTextField"           ; ns-text-field
  "Text the user can select or edit to send an action message to a target
when the user presses the Return key."
  "see https://developer.apple.com/documentation/appkit/nstextfield?language=objc")

(doc-objc-class "NSTextView"            ; ns-text-view
  "A view that draws text and handles user interactions with that text."
  "see https://developer.apple.com/documentation/appkit/nstextview?language=objc")

(doc-objc-class "NSText"                ; ns-text
  "The most general programmatic interface for objects that manage text."
  "see https://developer.apple.com/documentation/appkit/nstext?language=objc")

;;; Text input

(doc-objc-class "NSTextInputContext"    ; ns-text-input-context
  "An object that represents the Cocoa text input system."
  "see https://developer.apple.com/documentation/appkit/nstextinputcontext?language=objc")

(doc-objc-class "NSTextInsertionIndicator" ; ns-text-insertion-indicator
  "A view that represents the insertion indicator in text."
  "see https://developer.apple.com/documentation/appkit/nstextinsertionindicator?language=objc")

;;; Text-checking

;;; Spell-checking

(doc-objc-class "NSSpellChecker"        ; ns-spell-checker
  "An interface to the Cocoa spell-checking service."
  "see https://developer.apple.com/documentation/appkit/nsspellchecker?language=objc")

;;; Deprecated


;;;; TextKit
;; Manage text storage and perform custom layout of text-based content in your app’s views.
;; see https://developer.apple.com/documentation/appkit/textkit?language=objc

;;; Text management

(doc-objc-class "NSTextContentStorage"  ; ns-text-content-storage
  "A concrete object for managing your view’s text content and generating the
text elements necessary for layout."
  "see https://developer.apple.com/documentation/appkit/nstextcontentstorage?language=objc")

(doc-objc-class "NSTextContentManager"  ; ns-text-content-manager
  "An abstract class that defines the interface and a default implementation for managing the text document contents."
  "see https://developer.apple.com/documentation/appkit/nstextcontentmanager?language=objc")

(doc-objc-class "NSAttributedString"    ; ns-attributed-string
  "A string of text that manages data, layout, and stylistic information for ranges of characters to support rendering."
  "see https://developer.apple.com/documentation/Foundation/NSAttributedString?language=objc")

(doc-objc-class "NSMutableAttributedString" ; ns-mutable-attributed-string
  "A mutable string with associated attributes (such as visual style, hyperlinks, or accessibility data) for portions of its text."
  "see https://developer.apple.com/documentation/Foundation/NSMutableAttributedString?language=objc")

;;; Formatting and attributes

(doc-objc-class "NSParagraphStyle"      ; ns-paragraph-style
  "see https://developer.apple.com/documentation/appkit/nsparagraphstyle?language=objc")

(doc-objc-class "NSMutableParagraphStyle" ; ns-mutable-paragraph-style
  "An object for changing the values of the subattributes in a paragraph style attribute."
  "see https://developer.apple.com/documentation/appkit/nsmutableparagraphstyle?language=objc")

(doc-objc-class "NSTextTab"             ; ns-text-tab
  "A tab in a paragraph."
  "see https://developer.apple.com/documentation/appkit/nstexttab?language=objc")

(doc-objc-class "NSTextList"            ; ns-text-list
  "A section of text that forms a single list."
  "see https://developer.apple.com/documentation/appkit/nstextlist?language=objc")

(doc-objc-class "NSTextTable"           ; ns-text-table
  "An object that represents a text table as a whole."
  "see https://developer.apple.com/documentation/appkit/nstexttable?language=objc")

(doc-objc-class "NSTextTableBlock"      ; ns-text-table-block
  "A text block that appears as a cell in a text table."
  "see https://developer.apple.com/documentation/appkit/nstexttableblock?language=objc")

(doc-objc-class "NSTextBlock"           ; ns-text-block
  "A block of text laid out in a subregion of the text container."
  "see https://developer.apple.com/documentation/appkit/nstextblock?language=objc")

;;; Content elements

(doc-objc-class "NSTextParagraph"       ; ns-text-paragraph
  "A class that represents a single paragraph backed by an attributed string as the contents."
  "see https://developer.apple.com/documentation/appkit/nstextparagraph?language=objc")

(doc-objc-class "NSTextListElement"     ; ns-text-list-element
  "A class that represents a text list node."
  "see https://developer.apple.com/documentation/appkit/nstextlistelement?language=objc")

(doc-objc-class "NSTextElement"         ; ns-text-element
  "An abstract base class that represents the smallest units of text layout such as paragraphs or attachments."
  "see https://developer.apple.com/documentation/appkit/nstextelement?language=objc")

;;; Location and selection

(doc-objc-class "NSTextRange"           ; ns-text-range
  "A class that represents a contiguous range between two locations inside document contents."
  "see https://developer.apple.com/documentation/appkit/nstextrange?language=objc")

(doc-objc-class "NSTextSelection"       ; ns-text-selection
  "A class that represents a single logical selection context that corresponds to an insertion point."
  "see https://developer.apple.com/documentation/appkit/nstextselection?language=objc")

(doc-objc-class "NSTextSelectionNavigation" ; ns-text-selection-navigation
  "An interface you use to expose methods for obtaining results from actions performed on text selections."
  "see https://developer.apple.com/documentation/appkit/nstextselectionnavigation?language=objc")

;;; Layout

(doc-objc-class "NSTextLayoutManager"   ; ns-text-layout-manager
  "The primary class that you use to manage text layout and presentation for custom text displays."
  "see https://developer.apple.com/documentation/appkit/nstextlayoutmanager?language=objc")

(doc-objc-class "NSTextContainer"       ; ns-text-container
  "A region where text layout occurs."
  "see https://developer.apple.com/documentation/appkit/nstextcontainer?language=objc")

(doc-objc-class "NSTextLayoutFragment"  ; ns-text-layout-fragment
  "A class that represents the layout fragment typically corresponding to a rendering surface, such as a layer or view subclass."
  "see https://developer.apple.com/documentation/appkit/nstextlayoutfragment?language=objc")

(doc-objc-class "NSTextLineFragment"    ; ns-text-line-fragment
  "A class that represents a line fragment as a single textual layout and rendering unit inside a text layout fragment."
  "see https://developer.apple.com/documentation/appkit/nstextlinefragment?language=objc")

(doc-objc-class "NSTextViewportLayoutController" ; ns-text-viewport-layout-controller
  "Manages the layout process inside the viewport interacting with its delegate."
  "see https://developer.apple.com/documentation/appkit/nstextviewportlayoutcontroller?language=objc")

;;; Attachments

(doc-objc-class "NSTextAttachment"      ; ns-text-attachment
  "The values for the attachment characteristics of attributed strings and related objects."
  "see https://developer.apple.com/documentation/appkit/nstextattachment?language=objc")

(doc-objc-class "NSTextAttachmentViewProvider" ; ns-text-attachment-view-provider
  "A container object that associates a text attachment at a particular document location with a view object."
  "see https://developer.apple.com/documentation/appkit/nstextattachmentviewprovider?language=objc")

(doc-objc-class "NSAdaptiveImageGlyph"  ; ns-adaptive-image-glyph
  "A data object for an emoji-like image that can appear in attributed text."
  "see https://developer.apple.com/documentation/appkit/nsadaptiveimageglyph?language=objc")

(doc-objc-class "NSCell"                ; ns-cell
  "A mechanism for displaying text or images in a view object
without the overhead of a full `ns-view' subclass."
  "see https://developer.apple.com/documentation/appkit/nscell?language=objc")

(doc-objc-class "NSTextAttachmentCell"  ; ns-text-attachment-cell
  "An object that implements the functionality of the text attachment cell protocol."
  "see https://developer.apple.com/documentation/appkit/nstextattachmentcell-swift.class?language=objc")

;;; Glyphs

(doc-objc-class "NSGlyphGenerator"      ; ns-glyph-generator
  "An object that performs the initial, nominal glyph generation phase in the layout process."
  "see https://developer.apple.com/documentation/appkit/nsglyphgenerator?language=objc")

(doc-objc-class "NSGlyphInfo"           ; ns-glyph-info
  "A glyph attribute in an attributed string."
  "see https://developer.apple.com/documentation/appkit/nsglyphinfo?language=objc")

;;; TextKit 1

(doc-objc-class "NSTextStorage"         ; ns-text-storage
  "The fundamental storage mechanism of TextKit that contains the text managed by the system."
  "see https://developer.apple.com/documentation/appkit/nstextstorage?language=objc")

(doc-objc-class "NSLayoutManager"       ; ns-layout-manager
  "An object that coordinates the layout and display of text characters."
  "see https://developer.apple.com/documentation/appkit/nslayoutmanager?language=objc")

(doc-objc-class "NSATSTypesetter"       ; ns-ats-typesetter
  "A concrete typesetter object that places glyphs during the text layout process."
  "see https://developer.apple.com/documentation/appkit/nsatstypesetter?language=objc")

(doc-objc-class "NSTypesetter"          ; ns-typesetter
  "An abstract class that performs various type layout tasks."
  "see https://developer.apple.com/documentation/appkit/nstypesetter?language=objc")

;;; Text


;;;; Fonts
;; Manage the fonts used to display text.
;; see https://developer.apple.com/documentation/appkit/fonts?language=objc

;;; Font Data

(doc-objc-class "NSFont"                ; ns-font
  "The representation of a font in an app."
  "see https://developer.apple.com/documentation/appkit/nsfont?language=objc")

(doc-objc-class "NSFontDescriptor"      ; ns-font-descriptor
  "A dictionary of attributes that describe a font."
  "see https://developer.apple.com/documentation/appkit/nsfontdescriptor?language=objc")

;;; Management

(doc-objc-class "NSFontManager"         ; ns-font-manager
  "The center of activity for the font-conversion system."
  "see https://developer.apple.com/documentation/appkit/nsfontmanager?language=objc")

(doc-objc-class "NSFontCollection"      ; ns-font-collection
  "A font collection, which is a group of font descriptors taken together as a single object."
  "see https://developer.apple.com/documentation/appkit/nsfontcollection?language=objc")

(doc-objc-class "NSMutableFontCollection" ; ns-mutable-font-collection
  "A mutable collection of font descriptors taken together as a single object."
  "see https://developer.apple.com/documentation/appkit/nsmutablefontcollection?language=objc")


;;;; Writing Tools
;; Add support for Writing Tools to your app’s text views.
;; see https://developer.apple.com/documentation/appkit/writing-tools?language=objc

;;; Configuration

;;; Writing Tools for custom views

(doc-objc-class "NSWritingToolsCoordinator" ; ns-writing-tools-coordinator
  "An object that manages interactions between Writing Tools and your custom text view."
  "see https://developer.apple.com/documentation/appkit/nswritingtoolscoordinator?language=objc")

(doc-objc-class "NSWritingToolsCoordinatorContext" ; ns-writing-tools-coordinator-context
  "A data object that you use to share your custom view’s text with Writing Tools."
  "see https://developer.apple.com/documentation/appkit/nswritingtoolscoordinator/context?language=objc")

(doc-objc-class "NSWritingToolsCoordinatorAnimationParameters" ; ns-writing-tools-coordinator-animation-parameters
  "An object you use to configure additional tasks or animations to run alongside the Writing Tools animations."
  "see https://developer.apple.com/documentation/appkit/nswritingtoolscoordinator/animationparameters?language=objc")

;;; Text previews

(doc-objc-class "NSTextPreview"         ; ns-text-preview
  "A snapshot of the text in your view, which the system uses to create user-visible effects."
  "see https://developer.apple.com/documentation/appkit/nstextpreview?language=objc")

;;; Toolbar configuration


;;;; App and Environment
;; Learn about the objects that you use to interact with the system.
;; see https://developer.apple.com/documentation/appkit/app-and-environment?language=objc

;;; Life Cycle

(doc-objc-class "NSApplication"         ; ns-application
  (("currentEvent"
    :reader current-event
    :documentation "The last event object that the app retrieved from the event queue.
See https://developer.apple.com/documentation/appkit/nsapplication/currentevent?language=objc")
   ("running"
    :reader running-p
    :documentation "Test if the main event loop is running.
See https://developer.apple.com/documentation/appkit/nsapplication/isrunning?language=objc")
   ("active"
    :reader activep
    :documentation "Test if this is the active app.
See https://developer.apple.com/documentation/appkit/nsapplication/isactive?language=objc"))
  "An object that manages an app’s main event loop and resources used by all of that app's objects."
  "Every app uses a single instance of `ns-application' to control the main
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
Use `ns-app' to retrive the global variable `coca.appkit::*ns-app*'."
  "The shared `ns-application' object performs the important task of
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
the classes."
  "The delegate and notifications
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
applicationDidFinishLaunching:."
  "System services
======================
`ns-application' interacts with the system services architecture to
provide services to your app through the Services menu."
  "Subclassing notes
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
object without being closely tied to the global app object."
  "see https://developer.apple.com/documentation/appkit/nsapplication?language=objc")

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
          (apply #'ns-event-mask (coca.objc::listfy mask))
          (or util (ns-date-distant-future))
          mode
          (and deque t)))

(defmethod run ((app ns-application))
  "Starts the main event loop. "
  (invoke app "run"))

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

;; Scripting your app

;; Notifications

;; Loading Cocoa bundles

;; Displaying high dynamic resolution (HDR) content

(doc-objc-class "NSRunningApplication"  ; ns-running-application
  "An object that can manipulate and provide information for a single instance of an app."
  "see https://developer.apple.com/documentation/appkit/nsrunningapplication?language=objc")

;;; Environment

(doc-objc-class "NSWorkspace"           ; ns-workspace
  "A workspace that can launch other apps and perform a variety of file-handling services."
  "There is one shared NSWorkspace object per app.
You use the class method sharedWorkspace to access it.
For example, the following statement uses an `ns-workspace' object to request that
a file be opened in the TextEdit app:

    (invoke (invoke \"NSWorkspace\" \"sharedWorkspace\") \"openFile:withApplication:\" file application)

You can use the workspace object to:
+ Open, manipulate, and get information about files and devices.
+ Track changes to the file system, devices, and the user database.
+ Get and set Finder information for files.
+ Launch apps."
  "see https://developer.apple.com/documentation/appkit/nsworkspace?language=objc")

(doc-objc-class "NSWorkspaceOpenConfiguration" ; ns-workspace-open-configuration
  "The configuration options for opening URLs or launching apps. "
  "Create an `ns-workspace-open-configuration' object before launching an app
or opening a URL using the shared `ns-workspace' object.
Use the properties of this object to customize the behavior of the launched app
or the handling of the URLs.

For example, you might tell the app to hide itself immediately after launch."
  "see https://developer.apple.com/documentation/appkit/nsworkspace/openconfiguration?language=objc")

;;; Handoff

(doc-objc-class "NSUserActivity"        ; ns-user-activity
  "A representation of the state of your app at a moment in time."
  "see https://developer.apple.com/documentation/Foundation/NSUserActivity?language=objc")

;;; App Services

(doc-objc-class "NSSharingService"      ; ns-sharing-service
  "An object that facilitates the sharing of content with social media services, or with apps like Mail or Safari."
  "see https://developer.apple.com/documentation/appkit/nssharingservice?language=objc")

(doc-objc-class "NSToolbarItem"         ; ns-toolbar-item
  "A single item that appears in a window’s toolbar."
  "see https://developer.apple.com/documentation/appkit/nstoolbaritem?language=objc")

(doc-objc-class "NSSharingServicePickerToolbarItem" ; ns-sharing-service-picker-toolbar-item
  "A toolbar item that displays the macOS share sheet."
  "see https://developer.apple.com/documentation/appkit/nssharingservicepickertoolbaritem?language=objc")

;;; App Help

(doc-objc-class "NSHelpManager"         ; ns-help-manager
  "An object for displaying online help for an app."
  "see https://developer.apple.com/documentation/appkit/nshelpmanager?language=objc")

;;; Errors

;;; App Structure

;;;; appkit.lisp ends here
