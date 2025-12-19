;;;; appkit.lisp --- ObjC bindings for AppKit Framework
;; Construct and manage a graphical, event-driven user interface for your macOS app.
;; https://developer.apple.com/documentation/appkit?language=objc

(uiop:define-package #:coca.appkit
  (:use :cl :coca.objc :coca.foundation)
  (:export

   ;; App and Environment
   #:ns-application
   #:ns-running-application
   #:ns-workspace
   #:ns-workspace-open-configuration
   #:ns-user-activity
   #:ns-sharing-service
   #:ns-toolbar-item
   #:ns-sharing-service-picker-toolbar-item
   #:ns-help-manager

   ;; Documents, Data, and Pasteboard
   #:ns-document
   #:ns-document-controller
   #:ns-persistent-document
   #:ns-pasteboard
   #:ns-file-promise-provider
   #:ns-file-promise-receiver

   ;; Cocoa Bindings

   ;; Resource Management

   ;; App Extensions

   ;; Views and Controls

   ;; View Management

   ;; View Layout

   ;; Appearance Customization

   ;; Animation

   ;; Windows, Panels, and Screens
   #:ns-window
   #:ns-window-style-mask
   #:ns-backing-store-type
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

   ;; Supporting Continuity Camera in Your Mac App

   ;; Mouse, Keyboard, and Trackpad

   ;; Menus, Cursors, and the Dock

   ;; Gestures

   ;; Touch Bar

   ;; Drag and Drop

   ;; Accessibility for AppKit

   ;; Images and PDF

   ;; Drawing

   ;; Color

   ;; Printing

   ;; Text Display

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

   ;; Writing Tools
   ))

(in-package :coca.appkit)

(cffi:define-foreign-library appkit
  (:darwin (:framework "AppKit")))
(cffi:use-foreign-library appkit)


;;;; App and Environment
;; Learn about the objects that you use to interact with the system.
;; see https://developer.apple.com/documentation/appkit/app-and-environment?language=objc

;;; Life Cycle

(doc-objc-class "NSApplication"         ; ns-application
  "An object that manages an app’s main event loop and resources used by all of that app’s objects."
  "see https://developer.apple.com/documentation/appkit/nsapplication?language=objc")

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


;;;; Resource Management
;; Manage the storyboards and nib files containing your app’s user interface,
;; and learn how to load data that is stored in resource files.
;; see https://developer.apple.com/documentation/appkit/resource-management?language=objc


;;;; App Extensions
;; Extend your app’s basic functionality to other parts of the system.
;; see https://developer.apple.com/documentation/appkit/app-extensions?language=objc


;;;; Views and Controls
;; Present your content onscreen and handle user input and events.
;; see https://developer.apple.com/documentation/appkit/views-and-controls?language=objc


;;;; View Management
;; Manage your user interface, including the size and position of views in a window.
;; see https://developer.apple.com/documentation/appkit/view-management?language=objc


;;;; View Layout
;; Position and size views using a stack view or Auto Layout constraints.
;; see https://developer.apple.com/documentation/appkit/view-layout?language=objc


;;;; Appearance Customization
;; Add Dark Mode support to your app, and use appearance proxies to modify your UI.
;; see https://developer.apple.com/documentation/appkit/appearance-customization?language=objc


;;;; Animation
;; Animate your views and other content to create a more engaging experience for users.
;; see https://developer.apple.com/documentation/appkit/animation?language=objc


;;;; Windows, Panels, and Screens
;; Organize your view hierarchies and facilitate their display onscreen.
;; see https://developer.apple.com/documentation/appkit/windows-panels-and-screens?language=objc

;;; Windows

(doc-objc-class "NSWindow"              ; ns-window
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
  (:full-size-content-view    (ash 1 15) "When set, the window's contentView consumes the full size of the window."
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

(define-objc-enum ns-backing-store-type
  (:retained    0 "Deprecated"
                "The window uses a buffer"
                "but draws directly to the screen where possible and to the buffer for obscured portions.")
  (:nonretained 1 "Deprecated"
                "The window draws directly to the screen without using any buffer.")
  (:buffered    2 "The window renders all drawing into a display buffer and then flushes it to the screen."))

(doc-objc-class "NSPanel"               ; ns-panel
  "A special kind of window that typically performs a function that is auxiliary to the main window. "
  "For details about how panels work,
especially to find out how their behavior differs from window behavior, see How Panels Work:
https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/WinPanel/Concepts/UsingPanels.html#//apple_ref/doc/uid/20000224"
  "see https://developer.apple.com/documentation/appkit/nspanel?language=objc")

(doc-objc-class "NSWindowTab"           ; ns-window-tab
  "A tab associated with a window that is part of a tabbing group. "
  "see https://developer.apple.com/documentation/appkit/nswindowtab?language=objc")

(doc-objc-class "NSWindowTabGroup"      ; ns-window-tab-group
  "A group of windows that display together as a single tabbed window."
  "see https://developer.apple.com/documentation/appkit/nswindowtabgroup?language=objc")

;;; Window Restoration

;;; Screens

(doc-objc-class "NSScreen"              ; ns-screen
  "An object that describes the attributes of a computer’s monitor or screen. "
  "see https://developer.apple.com/documentation/appkit/nsscreen?language=objc")

;;; Popovers

(doc-objc-class "NSPopover"             ; ns-popover
  "A means to display additional content related to existing content on the screen. "
  "see https://developer.apple.com/documentation/appkit/nspopover?language=objc")

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


;;;; Supporting Continuity Camera in Your Mac App
;; Incorporate scanned documents and pictures from a user’s iPhone, iPad, or iPod touch
;; into your Mac app using Continuity Camera.
;; see https://developer.apple.com/documentation/appkit/supporting-continuity-camera-in-your-mac-app?language=objc


;;;; Mouse, Keyboard, and Trackpad
;; Handle events related to mouse, keyboard, and trackpad input.
;; see https://developer.apple.com/documentation/appkit/mouse-keyboard-and-trackpad?language=objc


;;;; Menus, Cursors, and the Dock
;; Implement menus and cursors to facilitate interactions with your app, and use your
;; app’s Dock tile to convey updated information.
;; see https://developer.apple.com/documentation/appkit/menus-cursors-and-the-dock?language=objc


;;;; Gestures
;; Encapsulate your app’s event-handling logic in gesture recognizers so that you can reuse
;; that code throughout your app.
;; see https://developer.apple.com/documentation/appkit/gestures?language=objc


;;;; Touch Bar
;; Display interactive content and controls in the Touch Bar.
;; see https://developer.apple.com/documentation/appkit/touch-bar?language=objc


;;;; Drag and Drop
;; Support the direct manipulation of your app’s content using drag and drop.
;; see https://developer.apple.com/documentation/appkit/drag-and-drop?language=objc


;;;; Accessibility for AppKit
;; Make your AppKit apps accessible to everyone who uses macOS.
;; see https://developer.apple.com/documentation/appkit/accessibility-for-appkit?language=objc


;;;; Images and PDF
;; Create and manage images, in bitmap, PDF, and other formats.
;; see https://developer.apple.com/documentation/appkit/images-and-pdf?language=objc


;;;; Drawing
;; Draw shapes, images, and other content on the screen.
;; see https://developer.apple.com/documentation/appkit/drawing?language=objc


;;;; Color
;; Represent colors using built-in or custom formats, and give users options for selecting
;; and applying colors.
;; see https://developer.apple.com/documentation/appkit/color?language=objc


;;;; Printing
;; Display the system print panels and manage the printing process.
;; see https://developer.apple.com/documentation/appkit/printing?language=objc


;;;; Text Display
;; Display text and check spelling.
;; see https://developer.apple.com/documentation/appkit/text-display?language=objc


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


;;;; Writing Tools
;; Add support for Writing Tools to your app’s text views.
;; see https://developer.apple.com/documentation/appkit/writing-tools?language=objc

;;;; appkit.lisp ends here
