;;;; webview.lisp --- Wraper of WebView

(in-package :coca.webkit)

(define-objc-class "CocaWebViewDelegate" "NSObject"
  "WKNavigationDelegate"
  "WKUIDelegate")

(define-objc-global-variable webview-delegate
    (init (alloc "CocaWebViewDelegate"))
  "The WebViewDelegate for all the `webview'. ")

(define-coca-base-view (webview "WKWebView") ()
  (:objc-property
   (inspectable
    :bool (:inspectable t "setInspectable:")
    "If or not can inspect WEBVIEW with Safari Web Inspector. ")
   (loadingp
    :bool ("isLoading" :read-only)
    "If or not WEBVIEW is currently loading contents. ")
   (estimated-progress
    :double ("estimatedProgress" :read-only)
    "Get an estimate of what fraction of the current WEBVIEW navigation.
Return value should be [0, 1]. ")
   (url
    :ns-url ("URL" :read-only)
    "Get/Set URL of WEBVIEW. ")
   (user-agent
    :ns-string ("userAgent" "setUserAgent:")
    "Custom User-Agent string. ")
   (has-only-secure-content-p
    :bool ("hasOnlySecureContent" :read-only)
    "Whether the web view loaded all resources on the page
through securely encrypted connections.")
   (page-zoom
    :double "pageZoom"
    "Scale factor by which WEBVIEW scales content relative to its bounds.")
   (allow-magnification
    :bool (:allow-magnification :read-only)
    "Whether magnify gestures change the web view's magnification.")
   (magnification
    :double "magnification"
    "The factor by which the page content is currently scaled.")

   ;; Navigation
   (can-go-back
    :bool ("canGoBack" :read-only)
    "If or not WEBVIEW can go back to previous page. ")
   (can-go-forward
    :bool ("canGoForward" :read-only)
    "If or not WEBVIEW can go forward to next page. "))
  (:init-keys (page-zoom     1.0d0 page-zoom?)
              (magnification 1.0d0 magnification?))
  (:init-binding (delegate (webview-delegate))
                 (ptr      (obj-ptr webview)))
  (:init
   (invoke ptr "setNavigationDelegate:" :object delegate)
   (invoke ptr "setUIDelegate:"         :object delegate)
   (when page-zoom?     (setf (page-zoom     webview) page-zoom))
   (when magnification? (setf (magnification webview) magnification)))
  (:documentation
   "Wrapper of WKWebView. "))


;;;; Manipulate

(defun %load-url (webview url)
  (declare (type webview webview)
           (type (or pathname string quri:uri) url))
  (with-ptr webview ptr
    (dispatch-main ()
      (invoke ptr "loadURL:" :ns-url url))))

(defgeneric navigate (webview url)
  (:documentation
   "Navigate URL in WEBVIEW.
Return the URL.

Parameter:
+ WEBVIEW
+ URL
  + pathname
  + string
  + quri:uri
")
  (:method ((webview webview) (pathname pathname))
    (%load-url webview pathname))
  (:method ((webview webview) (url string))
    (navigate webview (quri:uri url)))
  (:method ((webview webview) (url quri:uri))
    (%load-url webview url)))

(defmethod (setf url) (url (webview webview))
  (navigate webview url))

(defgeneric go-back (webview)
  (:documentation
   "Navigate WEBVIEW to previous page.
Return `t' if success, `nil' otherwise. ")
  (:method :around ((webview webview))
    (when (can-go-back webview)
      (call-next-method)))
  (:method ((webview webview))
    (with-ptr webview ptr
      (dispatch-main ()
        (invoke ptr "goBack:" :pointer ptr)))))

(defgeneric go-forward (webview)
  (:documentation
   "Navigate WEBVIEW forward.
Return `t' if success, `nil' otherwise. ")
  (:method :around ((webview webview))
    (when (can-go-forward webview)
      (call-next-method)))
  (:method ((webview webview))
    (with-ptr webview ptr
      (dispatch-main ()
        (invoke ptr "goForward:" :pointer ptr)))))

(defgeneric reload (webview)
  (:documentation
   "Reload WEBVIEW. ")
  (:method ((webview webview))
    (with-ptr webview ptr
      (dispatch-main ()
        (invoke ptr "reload")))))

(defgeneric reload-from-origin (webview))

(defgeneric stop-loading (webview)
  (:documentation
   "Stop current loading WEBVIEW. ")
  (:method ((webview webview))
    (with-ptr webview ptr
      (dispatch-main ()
        (invoke ptr "stopLoading")))))

;; TODO:
;; (defgeneric download (webview thing)
;;   (:documentation
;;    "Download THING with WEBVIEW. "))


;;;; Events

;; TODO: navigation abstraction in lisp

(defgeneric start-navigation-handler (webview navigation)
  (:documentation
   "Called when WEBVIEW start to navigate NAVIGATION.

Parameters:
+ WEBVIEW
+ NAVIGATION
")
  (:method ((webview webview) navigation)))

(define-objc-method ("CocaWebViewDelegate"
                     "webView:didStartProvisionalNavigation:"
                     :encoding "v@:@@")
                    :void ((webview    :object)
                           (navigation :object))
  (alx:when-let ((webview (find-obj webview)))
    (start-navigation-handler webview navigation)))

(defgeneric finish-navigation-handler (webview navigation)
  (:documentation
   "Called when WEBVIEW finish navigating NAVIGATION.

Parameters:
+ WEBVIEW
+ NAVIGATION
"))

(define-objc-method ("CocaWebViewDelegate"
                     "webview:didFinishNavigation:"
                     :encoding "v@:@@")
                    :void ((webview    :object)
                           (navigation :object))
  (alx:when-let ((webview (find-obj webview)))
    (finish-navigation-handler webview navigation)))

;;;; webview.lisp ends here
