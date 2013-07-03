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
  comzotohcljc.netty.nettyio)

(use '[clojure.tools.logging :only (info warn error debug)])
(import '(org.apache.commons.io FileUtils))
(import '(org.apache.commons.lang3 StringUtils))
(import '(java.io IOException ByteArrayOutputStream File InputStream))
(import '(java.nio ByteBuffer))
(import '(java.util HashMap))
(import '(java.net URI URL InetSocketAddress))
(import '(java.util.concurrent Executors))
(import '(javax.net.ssl SSLEngine SSLContext))
(import '(javax.net.ssl X509TrustManager TrustManager))
(import '(org.jboss.netty.bootstrap Bootstrap ClientBootstrap ServerBootstrap))
(import '(org.jboss.netty.channel
  ChannelFutureListener Channels SimpleChannelHandler ChannelFuture
  ChannelPipelineFactory ChannelPipeline))
(import '(org.jboss.netty.buffer ByteBufferBackedChannelBuffer))
(import '(org.jboss.netty.channel.group
  ChannelGroupFuture ChannelGroup ChannelGroupFutureListener
  DefaultChannelGroup))
(import '(org.jboss.netty.channel.socket.nio
  NioClientSocketChannelFactory NioServerSocketChannelFactory))
(import '(org.jboss.netty.handler.codec.http HttpChunkAggregator DefaultHttpRequest
  HttpContentCompressor HttpHeaders HttpVersion HttpChunk
  HttpMessage HttpRequest HttpResponse HttpResponseStatus
  DefaultHttpResponse QueryStringDecoder HttpMethod 
  HttpRequestDecoder HttpServerCodec HttpClientCodec
  HttpResponseEncoder))
(import '(org.jboss.netty.handler.ssl SslHandler))
(import '(org.jboss.netty.handler.stream ChunkedStream ChunkedWriteHandler))
(import '(com.zotoh.frwk.net NetUtils))
(import '(com.zotoh.frwk.io XData))
(import '(org.apache.commons.io IOUtils))
(require '[comzotohcljc.crypto.cryptutils :as CY])
(require '[comzotohcljc.crypto.stores :as ST])
(require '[comzotohcljc.net.netutils :as NU])
(require '[comzotohcljc.util.fileutils :as FU])
(require '[comzotohcljc.util.strutils :as SU])
(require '[comzotohcljc.util.ioutils :as IO])
(require '[comzotohcljc.util.coreutils :as CU])


(def ^:dynamic *HTTP-CODES*
     (let [fields (:fields (bean HttpResponseStatus))
     to-key (comp int #(.getCode (.get % nil)))
     kkeys (map to-key fields)
     vvals (map (comp str #(.get % nil)) fields)]
       (into {} (map vec (partition 2 (interleave kkeys vvals))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main netty classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- make-server-bootstrap ^{ :doc "Make a netty server bootstrap." }
  [options]
  (let [ bs (ServerBootstrap. (NioServerSocketChannelFactory.
            (Executors/newCachedThreadPool)
            (Executors/newCachedThreadPool)))
         dft { :child.receiveBufferSize (int (* 2 1024 1024))
               :child.tcpNoDelay true
               :reuseAddress true }
         opts (merge dft options) ]
    (doseq [ en (seq opts) ]
      (.setOption bs (name (first en)) (last en)))
    [bs opts]))

(defn- make-client-bootstrap ^{ :doc "Make a netty client bootstrap." }
  [options]
  (let [ bs (ClientBootstrap. (NioClientSocketChannelFactory.
            (Executors/newCachedThreadPool) (Executors/newCachedThreadPool)))
         dft { :tcpNoDelay true
               :keepAlive true }
         opts (merge dft options) ]
    (doseq [ en (seq opts) ]
      (.setOption bs (name (first en)) (last en)))
    [bs opts]))

(defn- make-channel-group ^{ :doc "Make a channel group." }
  [] (DefaultChannelGroup. (CU/uid)))

(defn- make-sslServer ^{ :doc "Make a server-side SSLContext." }
  [^URL keyUrl ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj]
  (let [ ctx (SSLContext/getInstance "TLS")
         ks (with-open [ inp (.openStream keyUrl) ]
              (if (CY/pkcs-file? keyUrl)
                  (CY/get-pkcsStore inp pwdObj)
                  (CY/get-jksStore inp pwdObj)))
    cs (comzotohcljc.crypto.stores.CryptoStore. ks pwdObj) ]
    (.init ctx
      (-> cs (.keyManagerFactory) (.getKeyManagers))
      (-> cs (.trustManagerFactory) (.getTrustManagers))
      (CY/get-srand))
    ctx))

(defn- make-sslClient ^{ :doc "Make a client-side SSLContext." }
  ;;[^URL keyUrl ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj]
  [ssl]
  (if (not ssl)
    nil
    (let [ ctx (SSLContext/getInstance "TLS") ]
      (.init ctx nil (into-array TrustManager [(CY/make-simpleTrustMgr)]) nil)
      ctx)))

(defn- make-resp-status
  ([] (make-resp-status HttpResponseStatus/OK))
  ([status]
    (DefaultHttpResponse. (HttpVersion/HTTP_1_1) status)))

(defprotocol ^{ :doc "" } NettyServiceIO
  (before-send [_ ch msg] )
  (onerror [_ ch msginfo exp] )
  (onreq [_ ch msginfo xdata] )
  (onres [_ ch msginfo xdata] ))

(defrecord NettyServer [ ^ServerBootstrap server ^ChannelGroup cgroup options])
(defrecord NettyClient [ ^ClientBootstrap client ^ChannelGroup cgroup options])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^:private add-listener (fn [a & more ] (class a)))

(defn- op-complete [cf done ok nok]
  (cond
    (not (nil? done)) (apply done cf)
    (and (not (nil? nok)) (not (.isSuccess cf))) (apply nok cf)
    (and (not (nil? ok)) (.isSuccess cf)) (apply ok cf)
    :else nil))

(defmethod ^:private add-listener ChannelGroupFuture
  [cf { ok :yes nok :no done :done }]
  (-> cf (.addListener
              (reify ChannelGroupFutureListener
                (operationComplete [_ cf]
                  (op-complete cf done ok nok))))))

(defmethod ^:private add-listener ChannelFuture
  [cf { ok :yes nok :no done :done }]
  (-> cf (.addListener
              (reify ChannelFutureListener
                (operationComplete [_ cf]
                  (op-complete cf done ok nok))))))

;; make channel-future listener to close the channel
(defn- maybe-keepAlive [ cf keepAlive]
  (when-not keepAlive
    (add-listener cf { :done #(do (.close (.getChannel %))) } )))

;; turn headers into a clj-map
(defn- nio-mapheaders [^HttpMessage msg]
  (reduce (fn [sum n]
            (assoc sum (.toLowerCase n) (vec (.getHeaders msg n))))
    {} (seq (.getHeaderNames msg))))

;; get info from http message
(defn- nio-extract-msg [^HttpMessage msg]
  (let [ pn (-> msg (.getProtocolVersion) (.toString))
         clen (HttpHeaders/getContentLength msg)
         kalive (HttpHeaders/isKeepAlive msg)
         chunked (.isChunked msg)
         headers (nio-mapheaders msg) ]
    { :protocol pn :is-chunked chunked
      :keep-alive kalive :clen clen
      :headers headers } ))

;; extract info from request
(defn- nio-extract-req [^HttpRequest req]
  (let [ decr (QueryStringDecoder. (.getUri req))
         md (-> req (.getMethod) (.getName))
         uri (.getPath decr)
         m1 (nio-extract-msg req)
         params (reduce (fn [sum en]
                          (assoc sum (.toLowerCase (nth en 0)) (vec (nth en 1))))
                      {} (seq (.getParameters decr))) ]
    (comzotohcljc.net.netutils.HTTPMsgInfo.
      (:protocol m1) md uri (:is-chunked m1) (:keep-alive m1) (:clen m1) (:headers m1) params)))

;; extract info from response
(defn- nio-extract-res [^HttpResponse res]
  (let [ m1 (nio-extract-msg res) ]
    (comzotohcljc.net.netutils.HTTPMsgInfo.
      (:protocol m1) "" "" (:is-chunked m1) (:keep-alive m1) (:clen m1) (:headers m1) {})))

(defn- send-100-cont [ctx]
  (let [ ch (.getChannel ctx) ]
    (.write ch (DefaultHttpResponse. HttpVersion/HTTP_1_1  HttpResponseStatus/CONTINUE))))

(defn- nio-pcomplete [ctx msg]
  (let [ ch (.getChannel ctx)
         attObj (.getAttachment ch)
         xdata (:xs attObj)
         info (:info attObj)
         dir (:dir attObj)
         usercb (:cb attObj)
         os (:os attObj)
         clen (:clen attObj) ]
    (when (and (> clen 0) (instance? ByteArrayOutputStream os))
      (.resetContent xdata os))
    (cond
      (= dir -1) (do
                   (CU/TryC (.close ch))
                   (.onres usercb ch info xdata))
      (= dir 1) (do 
                  (.onreq usercb ch info xdata))
      :else nil)
    ))

(defn- nio-sockit-down [ctx msg]
  (let [ ch (.getChannel ctx)
         attObj (.getAttachment ch)
         cbuf (.getContent msg)
         xdata (:xs attObj)
         cout (:os attObj)
         csum (:clen attObj)
         nsum (NetUtils/sockItDown cbuf cout csum)
         nout (if (.isDiskFile xdata)
                  cout
                  (NetUtils/swapFileBacked xdata cout nsum)) ]
    (.setAttachment ch (merge attObj { :os nout :clen nsum } ))))

(defn- nio-cfgctx [ctx atts usercb]
  (let [ os (IO/make-baos)
         x (XData.)
         clen 0
         m (merge { :xs x :os os :clen clen :cb usercb } atts) ]
    (.setAttachment (.getChannel ctx) m)
    m))

(defn- nio-perror [ctx ev]
  (let [ ch (.getChannel ctx)
         attObj (.getAttachment ch)
         usercb (:cb attObj) ]
    (.onerror usercb ch (.getCause ev) )))

;; close any output stream created during the message handling
(defn- nio-finz [ctx]
  (let [ att (-> ctx (.getChannel) (.getAttachment))
         os (:os att) ]
    (IOUtils/closeQuietly os)))

;; handle a request
(defn- nio-preq [ctx ^HttpRequest req usercb]
  (let [ msginfo (nio-extract-req req)
         attObj (nio-cfgctx ctx { :dir 1 :info msginfo } usercb) ]
    (debug "nio-preq: received a " (:method msginfo ) " request from " (:uri msginfo))
    (when (HttpHeaders/is100ContinueExpected req) (send-100-cont ctx))
    (if (:is-chunked msginfo)
      (debug "nio-preq: request is chunked.")
      ;; msg not chunked, suck all data from the request
      (do (try
              (nio-sockit-down ctx req)
            (finally (nio-finz ctx)))
        (nio-pcomplete ctx req)))))

(defn- nio-redirect [ctx ev]
  ;; TODO: handle redirect properly, for now, same as error
  (nio-perror ctx ev))

(defn- nio-presbody [ctx ^HttpResponse res]
    (if (.isChunked res)
      (debug "nio-presbody: response is chunked.")
      (try
        (nio-sockit-down ctx res)
        (finally (nio-finz ctx)))))

;; handle a response
(defn- nio-pres [ctx ev ^HttpResponse res usercb]
  (let [ msginfo (nio-extract-res res)
         s (.getStatus res)
         r (.getReasonPhrase s)
         c (.getCode s) ]
    (debug "nio-pres: got a response: code " c " reason: " r)
    (nio-cfgctx ctx { :dir -1 :info msginfo } usercb)
    (cond
      (and (>= c 200) (< c 300)) (nio-presbody ctx res)
      (and (>= c 300) (< c 400)) (nio-redirect ctx ev)
      :else (nio-perror ctx ev))))

;; handle a chunk - part of a message
(defn- nio-chunk [ctx ^HttpChunk msg]
  (let [ done (try
                  (nio-sockit-down ctx msg)
                  (.isLast msg)
                (catch Throwable e#
                    (do (nio-finz ctx) (throw e#)))) ]
    (if done (nio-pcomplete ctx msg) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make our internal channel handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- pipelinehdlr [usercb]
  (proxy [SimpleChannelHandler] []

    (exceptionCaught [ctx ev]
      (let [ ch (.getChannel ctx)
             attObj (.getAttachment ch)
             msginfo (:info attObj)
             keepAlive (if (nil? msginfo) false (:keep-alive msginfo)) ]
        (.onerror usercb ch msginfo (.getCause ev))
        (when-not keepAlive (CU/TryC (.close ch)))))

    (messageReceived [ctx ev]
      (let [ msg (.getMessage ev) ]
        (cond
          (instance? HttpResponse msg) (nio-pres ctx ev msg usercb)
          (instance? HttpRequest msg) (nio-preq ctx msg usercb)
          (instance? HttpChunk msg) (nio-chunk ctx msg)
          :else (throw (IOException. "Received some unknown object." ) ))))

    ))

(defn- make-pipeServer [^SSLContext sslctx usercb]
  (reify ChannelPipelineFactory
    (getPipeline [_]
      (let [ pl (org.jboss.netty.channel.Channels/pipeline)
             eg (if (nil? sslctx)
                    nil
                    (doto (.createSSLEngine sslctx)(.setUseClientMode false))) ]
        (when-not (nil? eg) (.addLast pl "ssl" (SslHandler. eg)))
        (doto pl
          ;;(.addLast "decoder" (HttpRequestDecoder.))
          ;;(.addLast "encoder" (HttpResponseEncoder.))
          (.addLast "codec" (HttpServerCodec.))
          (.addLast "chunker" (ChunkedWriteHandler.))
          (.addLast "handler" (pipelinehdlr usercb))) ))))

(defn- make-pipeClient [^SSLContext sslctx usercb]
  (reify ChannelPipelineFactory
    (getPipeline [_]
      (let [ pl (org.jboss.netty.channel.Channels/pipeline)
             eg (if (nil? sslctx)
                    nil
                    (doto (.createSSLEngine sslctx)(.setUseClientMode true))) ]
        (when-not (nil? eg) (.addLast pl "ssl" (SslHandler. eg)))
        (doto pl
          ;;(.addLast "decoder" (HttpRequestDecoder.))
          ;;(.addLast "encoder" (HttpResponseEncoder.))
          (.addLast "codec" (HttpClientCodec.))
          (.addLast "chunker" (ChunkedWriteHandler.))
          (.addLast "handler" (pipelinehdlr usercb))) ))))

(defn- make-xxx-server 
  ([host port keyUrl pwdObj usercb ] (make-xxx-server host port keyUrl pwdObj usercb {}))
  ([host port keyUrl pwdObj usercb options]
   (let [ ssl (if (nil? keyUrl) nil (make-sslServer keyUrl pwdObj))
          rc (make-server-bootstrap options)
          bs (first rc)
          cg (make-channel-group)
          pf (make-pipeServer ssl usercb) ]
     (.setPipelineFactory bs pf)
     (.add cg (.bind bs (InetSocketAddress. host (int port))))
     (debug "netty-xxx-server: running on host " host ", port " port)
     (NettyServer. bs cg (last rc)) )))

(defn- kill-9 [^ServerBootstrap bs]
    (do (.releaseExternalResources bs)))

(defn finz-server ^{ :doc "Bring down a netty server." }
  [ {^ServerBootstrap server :server ^ChannelGroup cg :cgroup } ]
  (if (nil? cg)
    (kill-9 server)
    (-> (.close cg) (add-listener { :done (fn [_] (kill-9 server)) }))))

(defn make-mem-httpd ^{ :doc "Make an in-memory http server." }

  ([host port ^File vdir
    ^comzotohcljc.netty.nettyio.NettyServiceIO usercb]
   (make-mem-httpd host port vdir nil nil usercb))

  ([host port ^File vdir ^URL keyUrl
    ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj
    ^comzotohcljc.netty.nettyio.NettyServiceIO usercb]
    (make-xxx-server host port keyUrl pwdObj usercb )))

(defn- reply-xxx [ch status]
  (let [ res (make-resp-status status)
         attObj (.getAttachment ch)
         info (if (nil? attObj) nil (:info attObj))
         kalive (if (nil? info) false (:keep-alive info)) ]
    (doto res
      (.setChunked false)
      (.setHeader "content-length", "0"))
    (-> (.write ch res) (maybe-keepAlive kalive))))

(defn- reply-get-vfile [ch xdata]
  (let [ res (make-resp-status)
         clen (.size xdata)
         attObj (.getAttachment ch)
         info (if (nil? attObj) nil (:info attObj))
         kalive (if (nil? info) false (:keep-alive info)) ]
    (doto res
      (.setHeader "content-type" "application/octet-stream")
      (.setHeader "content-length" (SU/nsb clen)))
    (.write ch res)
    (-> (.write ch (ChunkedStream. (.stream xdata)))
      (maybe-keepAlive kalive))))

(defn- filer-handler [vdir]
  (let [ putter (fn [ch fname xdata]
                  (try
                      (FU/save-file vdir fname xdata)
                      (reply-xxx ch (HttpResponseStatus/OK))
                    (catch Throwable e#
                      (reply-xxx ch (HttpResponseStatus/INTERNAL_SERVER_ERROR)))))

         getter (fn [ch fname]
                  (let [ xdata (FU/get-file vdir fname) ]
                    (if (.hasContent xdata)
                      (reply-get-vfile ch xdata)
                      (reply-xxx ch (HttpResponseStatus/NO_CONTENT))))) ]
    (reify NettyServiceIO
      (before-send [_ ch msg] nil)
      (onerror [_ ch msginfo exp]
        (do
          (error exp)
          (reply-xxx ch (HttpResponseStatus/INTERNAL_SERVER_ERROR))))
      (onres [_ ch msginfo xdata] nil)
      (onreq [_ ch msginfo xdata]
        (let [ mtd (.toUpperCase (:method msginfo))
               uri (:uri msginfo)
               pos (.lastIndexOf uri (int \/))
               p (if (< pos 0) uri (.substring uri (inc pos))) ]
          (debug "Method = " mtd ", Uri = " uri ", File = " p)
          (cond
            (or (= mtd "POST")(= mtd "PUT")) (putter ch p xdata)
            (= mtd "GET") (getter ch p)
            :else (reply-xxx ch (HttpResponseStatus/METHOD_NOT_ALLOWED))))))
      ))

(defn make-mem-filer ^{ :doc "" }
  ([host port ^File vdir] (make-mem-filer host port vdir nil nil))
  ([host port ^File vdir ^URL keyUrl ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj]
    (make-xxx-server host port keyUrl pwdObj (filer-handler vdir) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http client functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-httpClient ^{ :doc "" }
  [options]
  (let [ rc (make-client-bootstrap options)
         cg (make-channel-group) ]
    (NettyClient. (first rc) cg (last rc))))

(defn- make-nilServiceIO []
  (reify NettyServiceIO
    (before-send [_ ch msg] (debug "HttpClientr: empty before-send." ))
    (onerror [_ ch msginfo exp] (debug "HttpClientr: empty onerror." ))
    (onreq [_ ch msginfo xdata] (debug "HttpClientr: empty onreq." ))
    (onres [_ ch msginfo xdata] (debug "HttpClientr: empty onres." ))))

(defn- iniz-httpClient
  ([clientr ^URL targetUrl] (iniz-httpClient clientr targetUrl (make-nilServiceIO) ))
  ([clientr ^URL targetUrl serviceIO]
    (let [ ctx (make-sslClient (= "https" (.getScheme targetUrl)))
           host (.getHost targetUrl)
           pnum (.getPort targetUrl)
           port (if (< pnum 0) (if (nil? ctx) 80 443) pnum)
           pl (make-pipeClient ctx serviceIO)
           cli (:client clientr)
           cg (:cgroup clientr)
         ]
      (debug "HttpClientr: connecting to " host ":" port)
      (.setPipelineFactory cli pl)
      (let [ cf (doto (.connect cli (InetSocketAddress. host port))
                  (.awaitUninterruptibly))
             ok (.isSuccess cf)
             e (if (not ok) (.getCause cf) nil) ]
        (when-not ok
          (if (nil? e)
            (throw (IOException. ""))
            (throw e)))
        (let [ ch (.getChannel cf) ]
          (.add cg ch)
          (debug "HttpClientr: connected to " host ":" port " - OK.")
          ch)))))

(defn- send-httpClient [ch clientr req xdata serviceIO]
  (let [ clen (if (nil? xdata) 0 (.size xdata))
         before-send (:before-send serviceIO)
         options (:options clientr)
         uri (.getUri req) ]
    (doto req
      (.setHeader (org.jboss.netty.handler.codec.http.HttpHeaders$Names/CONNECTION)
          (if (:keepAlive options)
            org.jboss.netty.handler.codec.http.HttpHeaders$Values/KEEP_ALIVE
            org.jboss.netty.handler.codec.http.HttpHeaders$Values/CLOSE))
      (.setHeader
        (org.jboss.netty.handler.codec.http.HttpHeaders$Names/HOST)
        (.getHost (URL. uri))))
    (when-not (nil? before-send) (apply before-send req))
    (let [ ct (.getHeader req "content-type") ]
      (when (and (StringUtils/isEmpty ct) (not (nil? xdata)))
        (.setHeader req "content-type" "application/octet-stream")) )
    (when (> clen 0)
      (do
        (debug "HttpClientr: content has length " clen)
        (.setHeader req "content-length" (str "" clen))))
    (debug "HttpClientr: about to flush out request (headers)")
    (let [ cf (doto (.write ch req)
                (add-listener { :ok #(do (debug "HttpClientr: req headers flushed" )) })) ]
      (when (> clen 0)
        (doto
          (if (> clen (com.zotoh.frwk.io.IOUtils/streamLimit))
            (.write cf (ChunkedStream. (.stream xdata)))
            (.write cf (ByteBufferBackedChannelBuffer.
                         (ByteBuffer/wrap (.javaBytes xdata)))))
          (add-listener { :ok #(do (debug "HttpClientr: req payload flushed" )) }))))
    ))

(defn async-post ^{ :doc "" }
  ([clientr ^URL targetUrl ^XData xdata] (async-post clientr targetUrl xdata (make-nilServiceIO)))
  ([clientr ^URL targetUrl ^XData xdata serviceIO]
    (let [ req (DefaultHttpRequest. (HttpVersion/HTTP_1_1)
                                    (HttpMethod/POST) (.toString targetUrl))
           ch (iniz-httpClient clientr targetUrl serviceIO) ]
      (send-httpClient ch clientr req xdata serviceIO))))

(defn async-get ^{ :doc "" }
  ([clientr ^URL targetUrl] (async-get clientr targetUrl (make-nilServiceIO)))
  ([clientr ^URL targetUrl serviceIO]
    (let [ req (DefaultHttpRequest. (HttpVersion/HTTP_1_1)
                                    (HttpMethod/GET) (.toString targetUrl))
           ch (iniz-httpClient clientr targetUrl serviceIO) ]
      (send-httpClient ch clientr req nil serviceIO))))




























(def ^:private nettyio-eof nil)


