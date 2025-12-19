;;;; webkit.lisp --- ObjC bindings for WebKit Framework
;; Integrate web content seamlessly into your app,
;; and customize content interactions to meet your app's needs.
;; https://developer.apple.com/documentation/WebKit?language=objc

(uiop:define-package #:coca.webkit
  (:use :cl :coca.objc)
  (:export

   ;; WebKit for AppKit and UIKit
   #:wk-web-view
   #:wk-preferences
   #:wk-webpage-preferences
   #:wk-website-data-store
   #:wk-website-data-record
   #:wk-http-cookie-store
   #:wk-back-forward-list
   #:wk-back-forward-list-item
   #:wk-navigation
   #:wk-navigation-action
   #:wk-navigation-response
   #:wk-download
   #:wk-user-content-controller
   #:wk-content-rule-list-store
   #:wk-content-world
   #:wk-frame-info
   #:wk-security-origin
   #:wk-user-script
   #:wk-find-configuration
   #:wk-find-result
   #:wk-snapshot-configuration
   #:wk-pdf-configuration
   #:wk-web-extension
   #:wk-web-extension-context
   #:wk-web-extension-controller
   ))

(in-package :coca.webkit)

(cffi:define-foreign-library webkit
  (:darwin (:framework "WebKit")))
(cffi:use-foreign-library webkit)

;;;; WebKit for AppKit and UIKit
;; Display web content in AppKit or UIKit apps, or apps built with Objective-C.
;; Present a `wk-web-view' object from your custom view hierarchies
;; and load the content you want to display. Use supporting objects to
;; manage cookies, evaluate scripts, control navigation, generate
;; snapshots, and perform text-based searches.
;; see https://developer.apple.com/documentation/webkit/webkit-for-appkit-and-uikit?language=objc

;;; Web views

(doc-objc-class "WKWebView"             ; wk-web-view
  "An object that displays interactive web content, such as for an in-app browser."
  "A WKWebView object is a platform-native view that you use to
incorporate web content seamlessly into your app's UI. A web view
supports a full web-browsing experience, and presents HTML, CSS, and
JavaScript content alongside your app's native views. Use it when web
technologies satisfy your app's layout and styling requirements more
readily than native views. For example, you might use it when your
app's content changes frequently.

A web view offers control over the navigation and user experience
through delegate objects. Use the navigation delegate to react when
the user clicks links in your web content, or interacts with the
content in a way that affects navigation. For example, you might
prevent the user from navigating to new content unless specific
conditions are met. Use the UI delegate to present native UI elements,
such as alerts or contextual menus, in response to interactions with
your web content.

Embed a WKWebView object programmatically into your view hierarchy, or
add it using Interface Builder. Interface Builder supports many
customizations, such as configuring data detectors, media playback,
and interaction behaviors. For more extensive customizations, create
your web view programmatically using a WKWebViewConfiguration
object. For example, use a web view configuration object to specify
handlers for custom URL schemes, manage cookies, and customize
preferences for your web content.

Before your web view appears onscreen, load content from a web server
using a URLRequest structure or load content directly from a local
file or HTML string. The web view automatically loads embedded
resources such as images or videos as part of the initial load
request. It then renders your content and displays the results inside
the view’s bounds rectangle. The following code example shows a view
controller that replaces its default view with a custom WKWebView
object.

A web view automatically converts telephone numbers that appear in web
content to Phone links. When the user taps a Phone link, the Phone app
launches and dials the number. Use the WKWebViewConfiguration object
to change the default data detector behavior.

You can also use setMagnification:centeredAtPoint: to programmatically
set the scale of web content the first time it appears in a web
view. Thereafter, the user can change the scale using gestures."
  "Manage the navigation through your web content
====================================================
WKWebView provides a complete browsing experience, including the
ability to navigate between different webpages using links, forward
and back buttons, and more. When the user clicks a link in your
content, the web view acts like a browser and displays the content at
that link. To disallow navigation, or to customize your web view’s
navigation behavior, provide your web view with a navigation delegate
— an object that conforms to the WKNavigationDelegate protocol. Use
your navigation delegate to modify the web view’s navigation behavior,
or to track the loading progress of new content.

You can also use the methods of WKWebView to navigate programmatically
through your content, or to trigger navigation from other parts of
your app’s interface. For example, if your UI includes forward and
back buttons, connect those buttons to the goBack: and goForward:
methods of your web view to trigger the corresponding web
navigation. Use the canGoBack and canGoForward properties to determine
when to enable or disable your buttons."
  "Provide sharing options
=============================
People may want to share the contents of your web view with an app or
other people. Use a UIActivityViewController to present a share sheet
offering all the ways people can share the web content.

If your app has the com.apple.developer.web-browser entitlement, the
iOS share sheet can offer Add to Home Screen for an http or https
webpage, creating a convenient link to a web app or bookmark. To allow
someone to add the current webpage to the Home Screen, include the
WKWebView instance in the activityItems array when you call
initWithActivityItems:applicationActivities: to create the
UIActivityViewController. For more information about building a
browser app, see Preparing your app to be the default web browser.
see https://developer.apple.com/documentation/Xcode/preparing-your-app-to-be-the-default-browser?language=objc"
  "see https://developer.apple.com/documentation/webkit/wkwebview?language=objc")

;;; Web view configuration

(doc-objc-class "WKPreferences"         ; wk-preferences
  "An object that encapsulates the standard behaviors to apply to websites."
  "see https://developer.apple.com/documentation/webkit/wkpreferences?language=objc")

(doc-objc-class "WKWebpagePreferences"  ; wk-webpage-preferences
  "An object that specifies the behaviors to use when loading and rendering page content."
  "see https://developer.apple.com/documentation/webkit/wkwebpagepreferences?language=objc")

;;; Web data management

(doc-objc-class "WKWebsiteDataStore"    ; wk-website-data-store
  "An object that manages cookies, disk and memory caches, and other types of data for a web view."
  "see https://developer.apple.com/documentation/webkit/wkwebsitedatastore?language=objc")

(doc-objc-class "WKWebsiteDataRecord"   ; wk-website-data-record
  "A record of the data that a particular website stores persistently."
  "see https://developer.apple.com/documentation/webkit/wkwebsitedatarecord?language=objc")

(doc-objc-class "WKHTTPCookieStore"     ; wk-http-cookie-store
  "An object that manages the HTTP cookies associated with a particular web view."
  "see https://developer.apple.com/documentation/webkit/wkhttpcookiestore?language=objc")

;;; Navigation

(doc-objc-class "WKBackForwardList"     ; wk-back-forward-list
  "An object that manages the list of previously loaded webpages,
which the web view uses for forward and backward navigation."
  "see https://developer.apple.com/documentation/webkit/wkbackforwardlist?language=objc")

(doc-objc-class "WKBackForwardListItem" ; wk-back-forward-list-item
  "A representation of a webpage that the web view previously visited."
  "see https://developer.apple.com/documentation/webkit/wkbackforwardlistitem?language=objc")

(doc-objc-class "WKNavigation"          ; wk-navigation
  "An object that tracks the loading progress of a webpage."
  "see https://developer.apple.com/documentation/webkit/wknavigation?language=objc")

(doc-objc-class "WKNavigationAction"    ; wk-navigation-action
  "An object that contains information about an action that causes navigation to occur."
  "see https://developer.apple.com/documentation/webkit/wknavigationaction?language=objc")

(doc-objc-class "WKNavigationResponse"  ; wk-navigation-response
  "An object that contains the response to a navigation request,
and which you use to make navigation-related policy decisions."
  "see https://developer.apple.com/documentation/webkit/wknavigationresponse?language=objc")

;;; Downloads

(doc-objc-class "WKDownload"            ; wk-download
  "An object that represents the download of a web resource."
  "see https://developer.apple.com/documentation/webkit/wkdownload?language=objc")

;;; Page content

(doc-objc-class "WKUserContentController" ; wk-user-content-controller
  "An object for managing interactions between JavaScript code and your web view,
and for filtering content in your web view."
  "see https://developer.apple.com/documentation/webkit/wkusercontentcontroller?language=objc")

(doc-objc-class "WKContentRuleListStore" ; wk-content-rule-list-store
  "An object that contains the rules for how to load and filter content in the web view."
  "see https://developer.apple.com/documentation/webkit/wkcontentruleliststore?language=objc")

(doc-objc-class "WKContentWorld"        ; wk-content-world
  "An object that defines a scope of execution for JavaScript code,
and which you use to prevent conflicts between different scripts."
  "see https://developer.apple.com/documentation/webkit/wkcontentworld?language=objc")

(doc-objc-class "WKFrameInfo"           ; wk-frame-info
  "An object that contains information about a frame on a webpage."
  "see https://developer.apple.com/documentation/webkit/wkframeinfo?language=objc")

(doc-objc-class "WKSecurityOrigin"      ; wk-security-origin
  "An object that identifies the origin of a particular resource."
  "see https://developer.apple.com/documentation/webkit/wksecurityorigin?language=objc")

(doc-objc-class "WKUserScript"          ; wk-user-script
  "A script that the web view injects into a webpage."
  "see https://developer.apple.com/documentation/webkit/wkuserscript?language=objc")

;;; Page-level search

(doc-objc-class "WKFindConfiguration"   ; wk-find-configuration
  "The configuration parameters to use when searching the contents of the web view."
  "see https://developer.apple.com/documentation/webkit/wkfindconfiguration?language=objc")

(doc-objc-class "WKFindResult"          ; wk-find-result
  "An object that contains the results of searching the web view’s contents."
  "see https://developer.apple.com/documentation/webkit/wkfindresult?language=objc")

;;; Contextual menus

;;; Snapshots

(doc-objc-class "WKSnapshotConfiguration" ; wk-snapshot-configuration
  "The configuration data to use when generating an image from a web view’s contents."
  "see https://developer.apple.com/documentation/webkit/wksnapshotconfiguration?language=objc")

(doc-objc-class "WKPDFConfiguration"    ; wk-pdf-configuration
  "The configuration data to use when generating a PDF representation of a web view’s contents."
  "see https://developer.apple.com/documentation/webkit/wkpdfconfiguration?language=objc")

;;; Web extensions

(doc-objc-class "WKWebExtension"        ; wk-web-extension
  "An object that encapsulates a web extension’s resources that the manifest file defines."
  "see https://developer.apple.com/documentation/webkit/wkwebextension?language=objc")

(doc-objc-class "WKWebExtensionContext" ; wk-web-extension-context
  "An object that represents the runtime environment for a web extension."
  "see https://developer.apple.com/documentation/webkit/wkwebextensioncontext?language=objc")

(doc-objc-class "WKWebExtensionController" ; wk-web-extension-controller
  "An object that manages a set of loaded extension contexts."
  "see https://developer.apple.com/documentation/webkit/wkwebextensioncontroller?language=objc")

;;; Errors

;;; Macros

;;; Data types

;;; Deprecated

;;; WebKit APIs

;;;; webkit.lisp ends here
