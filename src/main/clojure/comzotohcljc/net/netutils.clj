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


(def ^:dynamic  *LHOST* "localhost")

(def ^:dynamic  *WP_HTTP* "HTTP")
(def ^:dynamic  *WP_SMTP* "SMTP")
(def ^:dynamic  *WP_SFTP* "SFTP")
(def ^:dynamic  *WP_FTP* "FTP")
(def ^:dynamic  *WP_FILE* "FILE")

;; 2XX: generally OK

(def ^:dynamic  *HTTP_OK*  200)
(def ^:dynamic  *HTTP_CREATED*  201)
(def ^:dynamic  *HTTP_ACCEPTED*  202)
(def ^:dynamic  *HTTP_NOT_AUTHORITATIVE*  203)
(def ^:dynamic  *HTTP_NO_CONTENT*  204)
(def ^:dynamic  *HTTP_RESET*  205)
(def ^:dynamic  *HTTP_PARTIAL*  206)

;; 3XX: relocation/redirect

(def ^:dynamic  *HTTP_MULT_CHOICE*  300)
(def ^:dynamic  *HTTP_MOVED_PERM*  301)
(def ^:dynamic  *HTTP_MOVED_TEMP*  302)
(def ^:dynamic  *HTTP_SEE_OTHER*  303)
(def ^:dynamic  *HTTP_NOT_MODIFIED*  304)
(def ^:dynamic  *HTTP_USE_PROXY*  305)

;; 4XX: client error

(def ^:dynamic  *HTTP_BAD_REQUEST*  400)
(def ^:dynamic  *HTTP_UNAUTHORIZED*  401)
(def ^:dynamic  *HTTP_PAYMENT_REQUIRED*  402)
(def ^:dynamic  *HTTP_FORBIDDEN*  403)
(def ^:dynamic  *HTTP_NOT_FOUND*  404)
(def ^:dynamic  *HTTP_BAD_METHOD*  405)
(def ^:dynamic  *HTTP_NOT_ACCEPTABLE*  406)
(def ^:dynamic  *HTTP_PROXY_AUTH*  407)
(def ^:dynamic  *HTTP_CLIENT_TIMEOUT*  408)
(def ^:dynamic  *HTTP_CONFLICT*  409)
(def ^:dynamic  *HTTP_GONE*  410)
(def ^:dynamic  *HTTP_LENGTH_REQUIRED*  411)
(def ^:dynamic  *HTTP_PRECON_FAILED*  412)
(def ^:dynamic  *HTTP_ENTITY_TOO_LARGE*  413)
(def ^:dynamic  *HTTP_REQ_TOO_LONG*  414)
(def ^:dynamic  *HTTP_UNSUPPORTED_TYPE*  415)

;; 5XX: server error

(def ^:dynamic  *HTTP_SERVER_ERROR*  500)
(def ^:dynamic  *HTTP_INTERNAL_ERROR*  501)
(def ^:dynamic  *HTTP_BAD_GATEWAY*  502)
(def ^:dynamic  *HTTP_UNAVAILABLE*  503)
(def ^:dynamic  *HTTP_GATEWAY_TIMEOUT*  504)
(def ^:dynamic  *HTTP_VERSION*  505)

(def ^:dynamic *socket-timeout* 5000)

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

(defn- do-post [cli ^URI targetUrl contentType ^XData xdata chunkIt beforeSendFunc]
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
  ([^URI targetUrl contentType ^XData xdata] (sync-post targetUrl contentType xdata false nil))
  ([^URI targetUrl contentType ^XData xdata chunkIt beforeSendFunc]
    (let [ cli (mkApacheClientHandle) ]
        (do-post cli targetUrl contentType xdata chunkIt beforeSendFunc))) )

(defn- do-get [cli ^URI targetUrl beforeSendFunc]
  (try
    (let [ g (HttpGet. targetUrl) ]
      (when-not (nil? beforeSendFunc) (beforeSendFunc g))
      (p-reply (.execute cli g)))
    (finally
      (.. cli getConnectionManager shutdown))) )

(defn sync-get ^{ :doc "Perform a http-get on the target url." }
  ([^URI targetUrl] (sync-get targetUrl nil))
  ([^URI targetUrl beforeSendFunc]
    (let [ cli (mkApacheClientHandle) ]
      (do-get cli targetUrl beforeSendFunc))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-simpleClientSSLEngine ^{ :doc "Simple minded, trusts everyone." }
  []
  (let [ c (SSLContext/getInstance "TLS") ]
    (.init c nil (SSLTrustMgrFactory/getTrustManagers) nil)) )








(def ^:private netutils-eof nil)

