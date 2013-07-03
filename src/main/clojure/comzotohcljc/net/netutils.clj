;;
;; COPYRIGHT (C) 2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
;;
;; THIS IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
;; MODIFY IT UNDER THE TERMS OF THE APACHE LICENSE
;; VERSION 2.0 (THE "LICENSE").
;;
;; THIS LIBRARY IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL
;; BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
;;
;; SEE THE LICENSE FOR THE SPECIFIC LANGUAGE GOVERNING PERMISSIONS
;; AND LIMITATIONS UNDER THE LICENSE.
;;
;; You should have received a copy of the Apache License
;; along with this distribution; if not you may obtain a copy of the
;; License at
;; http://www.apache.org/licenses/LICENSE-2.0
;;

(ns ^{ :doc "" :author "kenl" }
  comzotohcljc.net.netutils)

(use '[clojure.tools.logging :only (info warn error debug)])
(import '(java.security.cert X509Certificate CertificateException))
(import '(org.jboss.netty.handler.codec.http HttpMessage))
(import '(java.security KeyStoreException
  KeyStore InvalidAlgorithmParameterException))
(import '(javax.net.ssl
  SSLContext SSLEngine X509TrustManager
  TrustManagerFactorySpi TrustManager
  ManagerFactoryParameters))
(import '(com.zotoh.frwk.net SSLTrustMgrFactory))
(import '(com.zotoh.frwk.io XData))
(import '(org.apache.commons.lang3 StringUtils))
(import '(org.apache.http.client))
(import '(org.apache.http.client.methods HttpGet HttpPost))
(import '(org.apache.http.impl.client DefaultHttpClient))
(import '(org.apache.http Header
  StatusLine HttpEntity HttpResponse))
(import '(java.io File IOException))
(import '(org.apache.http.util EntityUtils))
(import '(java.net URL URI))
(import '(org.apache.http.params HttpConnectionParams))
(import '(org.apache.http.entity InputStreamEntity))
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.mimeutils :as MM])
(require '[comzotohcljc.util.strutils :as SU])

(def ^:dynamic *socket-timeout* 5000)
(def ^:dynamic  *LHOST* "localhost")
(def ^:dynamic  *WP_HTTP* "HTTP")
(def ^:dynamic  *WP_SMTP* "SMTP")
(def ^:dynamic  *WP_SFTP* "SFTP")
(def ^:dynamic  *WP_FTP* "FTP")
(def ^:dynamic  *WP_FILE* "FILE")

(defrecord HTTPResult [^String content-type ^String encoding ^long content-length ^bytes body])

(defrecord HTTPMsgInfo [^String protocol ^String method ^String uri
                        is-chunked keep-alive
                        ^long clen headers params] )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal functions to support apache http client.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- mkApacheClientHandle []
  (let [ cli (DefaultHttpClient.) pms (.getParams cli) ]
    (HttpConnectionParams/setConnectionTimeout pms *socket-timeout*)
    (HttpConnectionParams/setSoTimeout pms *socket-timeout*)
    cli))

(defn- get-bits [ent] (if (nil? ent) nil (EntityUtils/toByteArray ent)) )
(defn- get-str [ent] (EntityUtils/toString ent "utf-8"))

(defn- p-ok [rsp]
  (let [ ent (.getEntity rsp)
         ct (if (nil? ent) nil (.getContentType ent))
         cv (if (nil? ct) "" (SU/strim (.getValue ct)))
         cl (.toLowerCase cv) ]
    (CU/TryC
      (debug "content-encoding: " (.getContentEncoding ent))
      (debug "content-type: " cv))
    (let [ bits (get-bits ent)
           clen (if (nil? bits) 0 (alength bits)) ]
      (HTTPResult. (SU/nsb cv) (MM/get-charset cv) clen bits))))
    ;;(cond
      ;;(or (.startsWith cl "text/")
          ;;(.startsWith cl "application/xml")
          ;;(.startsWith cl "application/json")) (get-bits ent) ;;(get-str ent)
      ;;:else (get-bits ent))) )

(defn- p-error [rsp exp]
  (do
    (CU/TryC (EntityUtils/consumeQuietly (.getEntity rsp)))
    (throw exp)) )

(defn- p-redirect [rsp]
  (p-error rsp (IOException. "Redirect not supported.")) )

(defn- p-reply [ ^HttpResponse rsp ]
  (let [ st (.getStatusLine rsp)
         rc (if (nil? st) 0 (.getStatusCode st))
         msg (if (nil? st) "" (.getReasonPhrase st)) ]
    (cond
      (and (>= rc 200) (< rc 300)) (p-ok rsp)
      (and (>= rc 300) (< rc 400)) (p-redirect rsp)
      :else (p-error rsp (IOException. (str "Service Error: code = " rc ": " msg))))) )

(defn- do-post [cli ^URL targetUrl contentType ^XData xdata chunkIt beforeSendFunc]
  (try
    (let [ p (HttpPost. targetUrl)
           ent (InputStreamEntity. (.stream xdata) (.size xdata)) ]
      (.setEntity p (doto ent
                          (.setContentType contentType)
                          (.setChunked chunkIt)))
      (when-not (nil? beforeSendFunc) (beforeSendFunc p))
      (p-reply (.execute cli p)))
    (finally
        (.. cli getConnectionManager shutdown))) )

(defn sync-post ^{ :doc "Perform a http-post on the target url." }
  ([^URL targetUrl contentType ^XData xdata] (sync-post targetUrl contentType xdata false nil))
  ([^URL targetUrl contentType ^XData xdata chunkIt beforeSendFunc]
    (let [ cli (mkApacheClientHandle) ]
        (do-post cli targetUrl contentType xdata chunkIt beforeSendFunc))) )

(defn- do-get [cli ^URL targetUrl beforeSendFunc]
  (try
    (let [ g (HttpGet. targetUrl) ]
      (when-not (nil? beforeSendFunc) (beforeSendFunc g))
      (p-reply (.execute cli g)))
    (finally
      (.. cli getConnectionManager shutdown))) )

(defn sync-get ^{ :doc "Perform a http-get on the target url." }
  ([^URL targetUrl] (sync-get targetUrl nil))
  ([^URL targetUrl beforeSendFunc]
    (let [ cli (mkApacheClientHandle) ]
      (do-get cli targetUrl beforeSendFunc))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-simpleClientSSLEngine ^{ :doc "Simple minded, trusts everyone." }
  []
  (let [ c (SSLContext/getInstance "TLS") ]
    (.init c nil (SSLTrustMgrFactory/getTrustManagers) nil)) )

(defn- clean-str [s]
  (StringUtils/stripStart (StringUtils/stripEnd s ";,") ";,"))

(defn parse-ie [line]
  (let [ p1 #".*(MSIE\s*(\S+)\s*).*"
         m1 (re-matches p1 line)
         p2 #".*(Windows\s*Phone\s*(\S+)\s*).*"
         m2 (re-matches p2 line)
         bw "IE"
         dt (if (SU/has-nocase? "iemobile") :mobile :pc) ]

    (let [ bv (if (and (not (empty? m1)) (> (.size m1) 2))
                (clean-str (nth m1 2))
                "")
           dev (if (and (not (empty? m2)) (> (.size m2) 2))
                 { :device-version (clean-str (nth m1 2))
                  :device-moniker "windows phone"
                  :device-type :phone }
                 {} ) ]
      (merge {:browser :ie :browser-version bv :device-type dt}
             dev))))

(defn parse-chrome [line]
  (let [ p1 #".*(Chrome/(\S+)).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (.size m1) 2))
                (clean-str (nth m1 2))
                "") ]
    {:browser :chrome :browser-version bv :device-type :pc }))

(defn parse-kindle [line]
  (let [ p1 #".*(Silk/(\S+)).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (.size m1) 2))
                (clean-str (nth m1 2))
                "") ]
    { :browser :silk :browser-version bv :device-type :mobile :device-moniker "kindle" } ))

(defn parse-android [line]
  (let [ p1 #".*(Android\s*(\S+)\s*).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (.size m1) 2))
                (clean-str (nth m1 2))
                "") ]
    { :browser :chrome :browser-version bv :device-type :mobile :device-moniker "android" } ))

(defn parse-ffox [line]
  (let [ p1 #".*(Firefox/(\S+)\s*).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (.size m1) 2))
                (clean-str (nth m1 2))
                "") ]
    { :browser :firefox :browser-version bv :device-type :pc } ))

(defn parse-safari [line]
  (let [ p1 #".*(Version/(\S+)\s*).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (.size m1) 2))
                (clean-str (nth m1 2))
                "")
         rc { :browser :safari :browser-version bv :device-type :pc } ]
    (cond
      (SU/has-nocase? line "mobile/") (merge rc { :device-type :mobile })
      (SU/has-nocase? line "iphone") (merge rc { :device-type :phone :device-moniker "iphone" } )
      (SU/has-nocase? line "ipad") (merge rc { :device-type :mobile :device-moniker "ipad" } )
      (SU/has-nocase? line "ipod") (merge rc { :device-type :mobile :device-moniker "ipod" } )
      :else rc )))




(defn parse-userAgentLine ^{ :doc "" }
  [line]
  (cond
    if (_uaStr.indexOf("Safari/") > 0 && _uaStr.indexOf("Mac OS X") > 0) {
    if ( _uaStr.indexOf("Gecko/") > 0 && _uaStr.indexOf("Firefox/") > 0 ) {
    if ( _uaStr.indexOf("Windows") > 0 &&  _uaStr.indexOf("Trident/") > 0 ) {
    if ( _uaStr.indexOf("AppleWebKit/") > 0 && _uaStr.indexOf("Safari/") > 0 &&
    _uaStr.indexOf("Chrome/") > 0 ) {
    if ( _uaStr.indexOf("AppleWebKit/") > 0 && _uaStr.indexOf("Safari/") > 0 &&
    _uaStr.indexOf("Silk/") > 0 ) {
    if ( _uaStr.indexOf("AppleWebKit/") > 0 && _uaStr.indexOf("Safari/") > 0 &&
    _uaStr.indexOf("Android") > 0 ) {
    (parse_ie ua)
    (parse_chrome ua)
    (parse_android ua)
    (parse_kindle ua)
    (parse_safari ua)
    (parse_ffox ua)







(def ^:private netutils-eof nil)

