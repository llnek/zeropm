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
  com.zotoh.cljc.netty.nettyio
  (:use [clojure.tools.logging :only (info warn error debug)])
  (:import (org.apache.commons.io FileUtils))
  (:import (org.jboss.netty.channel Channels))
  (:import (java.io File InputStream))
  (:import (java.net URL))
  (:import (java.util.concurrent Executors))
  (:import (javax.net.ssl SSLEngine SSLContext))
  (:import (org.jboss.netty.bootstrap Bootstrap ClientBootstrap ServerBootstrap))
  (:import (org.jboss.netty.channel ChannelPipelineFactory ChannelPipeline))
  (:import (org.jboss.netty.channel.group DefaultChannelGroup))
  (:import (org.jboss.netty.channel.socket.nio NioClientSocketChannelFactory NioServerSocketChannelFactory))
  (:import (org.jboss.netty.handler.codec.http HttpChunkAggregator
    HttpContentCompressor
    HttpRequestDecoder
    HttpResponseEncoder))
  (:import (org.jboss.netty.handler.ssl SslHandler))
  (:import (org.jboss.netty.handler.stream ChunkedWriteHandler))
  (:require [com.zotoh.cljc.crypto.cryptutils :as CY])
  (:require [com.zotoh.cljc.crypto.stores :as ST])
  (:require [com.zotoh.cljc.net.netutils :as NU])
  (:require [com.zotoh.cljc.util.coreutils :as CU])
  )

(defn- make-cf-lsnr [msginfo]
  (reify ChannelFutureListener
    (operationComplete [_ cf]
      (when-not (get msginfo :keep-alive) (.close (.getChannel cf))))))

(defn make-server-bootstrap ^{ :doc "" }
  []
  (let [ b (ServerBootstrap. (NioServerSocketChannelFactory.
            (Executors/newCachedThreadPool)
            (Executors/newCachedThreadPool))) ]
    (doto b
      (.setOption "reuseAddress" true)
      ;; Options for its children
      (.setOption "child.receiveBufferSize" 1024*1024)
      (.setOption "child.tcpNoDelay" true))))

(defn make-client-bootstrap ^{ :doc "" }
  []
  (let [ bs (ClientBootstrap. (NioClientSocketChannelFactory.
                       (Executors/newCachedThreadPool) (Executors/newCachedThreadPool))) ]
    bs))

(defn make-channel-group ^{ :doc "" } [] (DefaultChannelGroup. (CU/uid)))

(defn make-ssl-context ^{ :doc "" }
  [keyUrl pwdObj]
  (let [ ctx (SSLContext/getInstance "TLS") ]
    (with-open [ inp (.openStream keyUrl) ]
      (let [ ks (if (-> keyUrl (.getFile) (.toLowerCase) (.endsWith ".jks")) (CY/get-jksStore) (CY/get-pkcsStore))
             cs (-> (com.zotoh.cljc.crypto.stores.CryptoStore. (doto ks (CY/init-store! (cast InputStream nil) pwdObj)) pwdObj)) ]
        (.addKeyEntity cs inp pwdObj)
        (.init ctx
          (-> cs (.keyManagerFactory ) (.getKeyManagers))
          (-> cs (.trustManagerFactory) (.getTrustManagers))
          (CY/get-srand))
        (doto (.createSSLEngine ctx) (.setUseClientMode false))))) )

(defn- nio-mapheaders [msg]
  (let [ names (.getHeaderNames msg) jm (HashMap.) ]
    (doseq [ n (seq names) ]
      (.put jm (.toLowerCase n) (vec (.getHeaders msg n))))
    (into {} jm)))

(defn- nio-extract-msg [msg]
  (let [ pn (-> msg (.getProtocolVersion) (.toString))
         chunked (.isChunked msg)
         clen (HttpHeaders/getContentLength msg)
         kalive (HttpHeaders/isKeepAlive msg)
         headers (reduce (fn [sum n]
                      (assoc sum (.toLowerCase n) (vec (.getHeaders msg n))))
                      {} (seq (.getHeaderNames msg))) ]
    (hashmap :protocol pn :is-chunked chunked :keep-alive kalive :clen clen :headers headers)))

(defn- nio-extract-req [req]
  (let [ decr (QueryStringDecoder. (.getUri req))
         md (-> req (.getMethod) (.getName))
         uri (.getPath decr)
         m1 (nio-extract-msg req)
         params (reduce (fn [sum en]
                      (assoc sum (nth en 0) (vec (nth en 1)))
                      {} (seq (.getParameters decr)))) ]
    (merge (hashmap :method md :uri uri :params params) m1)))

(defn- nio-extract-res [res] (nio-extract-msg res))

(defprotocol NettyServiceIO
  (onerror [this ch exp] )
  (onreq [this ch msginfo xdata] )
  (onres [this ch msginfo xdata] ))

(defn- send-100-cont [ev]
  (let [ ch (.getChannel ev) ]
    (.write ch (DefaultHttpResponse. HttpVersion/HTTP_1_1  HttpResponseStatus/CONTINUE))
    nil))

(defn- nio-pcomplete [ctx ev msg]
  (let [ ch (.getChannel ctx)
         attObj (.getAttachment ch)
         xdata (get attObj :xs)
         info (get attObj :info)
         dir (get attObj :dir)
         os (get attObj :os)
         clen (get attObj :clen) ]
    (when (and (> clen 0) (instance? ByteArrayOutputStream os))
      (.resetContent xdata os))
    (cond
      (= dir -1) (.onres usercb ch info xdata)
      (= dir 1) (.onreq usercb ch info xdata)
      :else  nil)
    ))

(defn- nio-sockit-down [ctx msg]
  (let [ attObj (.getAttachment ctx)
         cbuf (.getContent msg)
         xdata (get attObj :xs)
         cout (get attObj :os)
         csum (get attObj :clen)
         nsum (NetUtils/sockItDown cbuf cout csum)
         nout (if (.isDiskFile xdata)
                  cout
                  (NetUtils/swapFileBacked xdata cout nsum)) ]
    (.setAttachment ctx (merge attObj (hash-map :os nout :clen nsum)) )))

(defn- nio-cfgctx [ctx atts usercb]
  (let [ os (IO/make-baos)
         x (XData.)
         clen 0
         m (merge (hash-map :xs x :os os :clen clen :cb usercb) atts) ]
    (.setAttachment ctx m)))

(defn- nio-perror [ctx ev]
  (let [ ch (.getChannel ctx)
         exp (.getCause ev)
         attObj (.getAttachment ch)
         usercb (get attObj :cb) ]
    (.onerror usercb exp)))

(defn- nio-finz [ctx]
  (let [ att (.getAttachment ctx) os (:os att) ] (IOUtils/closeQuietly os)))

(defn- nio-preq [ctx ev req usercb]
  (let [ mtd (.. req getMethod getName) uri (.getUri req) hds (nio-mapheaders req) ]
    (nio-cfgctx ctx { :dir 1 :info (nio-exract-req req) } usercb)
    (when (HttpHeaders/is100ContinueExpected req) (send-100-cont ev))
    (debug "nio-preq: received a " mtd " request from " uri)
    (if (.isChunked req)
      (debug "nio-preq: request is chunked.")
      (try
        (nio-sockit-down ctx req)
        (finally (nio-finz ctx))))))

(defn- nio-presbody [ctx ev res]
    (if (.isChunked res)
      (debug "nio-presbody: response is chunked.")
      (try
        (nio-sockit-down ctx res)
        (finally (nio-finz ctx)))))

(defn- nio-pres [ctx ev res usercb]
  (let [ s (.getStatus res)
         r (.getReasonPhrase s)
         c (.getCode s) ]
    (debug "nio-pres: got a response: code " c " reason: " r)
    (nio-cfgctx ctx { :dir -1 :info (nio-extract-res res) } usercb)
    (cond
      (and (>= c 200) (< c 300)) (nio-presbody ctx ev res)
      (and (>= c 300) (< c 400)) (nio-perror ctx ev)
      :else (nio-perror ctx,ev))))

(defn- nio-chunk [ctx ev msg]
  (try
    (nio-sockit-down ctx msg)
    (when (.isLast msg) (nio-pcomplete ctx ev msg))
    (catch Throwable e#
      (do (nio-finz ctx) (throw e#)))) )

(defn mkchannelhdlr [usercb]
  (proxy [ SimpleChannelHandler ] []

    (exceptionCaught [_ ctx ev]
      (let [ ch (.getChannel ctx) attObj (.getAttachment ch)
             keepAlive (get attObj :keepAlive) ]
        (.onerror usercb (.getCause ev))
        (when-not keepAlive (.close ch))))

    (messageReceived [ _ ctx ev ]
      (let [ msg (.getMessage ev) ]
        (cond
          (instance? HttpResponse msg) (nio-pres ctx ev (cast HttpResponse msg) usercb)
          (instance? HttpRequest msg) (nio-preq ctx ev (cast HttpRequest msg) usercb)
          (instance? HttpChunk msg) (nio-chunk ctx ev (cast HttpChunk msg))
          :else (throw (IOException. "Received some unknown object." ) ))))

    ))

(defn- mkpipelinefac [^SSLEngine ssleng usercb]
  (reify ChannelPipelineFactory
    (getPipeline [this]
      (let [ pl (org.jboss.netty.channel.Channels/pipeline) ]
        (when-not (nil? ssleng) (.addLast pl "ssl" (SslHandler. ssleng)))
        (doto pl
          (.addLast "decoder" (HttpServerCodec.))
          (.addLast "chunker" (ChunkedWriteHandler.))
          (.addLast "handler" h))
        pl))))

(defn- make-xxx-server ([host port vdir keyUrl pwdObj usercb]
   (let [ ssl (if (nil? keyUrl) nil (make-ssl-context keyUrl pwdObj))
          bs (make-server-bootstrap)
          cg (make-channel-group)
          pf (mkpiplinefac ssl usercb) ]
     (.setPipelineFactory bs pf)
     (doto bs
       (.setOption "child.receiveBufferSize" (int (* 2 1024 1024)))
       (.setOption "child.tcpNoDelay" true)
       (.setOption "reuseAddress" true))
     (.add cg (.bind bs (InetSocketAddress. host port)))
     (debug "memxxxserver: running on host " host ", port " port)
     (-> cg (.close) (.addListener (reify ChannelGroupFutureListener
                     (operationComplete [this _]
                       (.releaseExternalResources bs))))))))

(defn make-mem-httpd ^{ :doc "" }
  ([host port vdir] (make-mem-httpd host port vdir nil nil))
  ([host port vdir keyUrl pwdObj]
    (make-xxx-server host port vdir keyUrl pwdObj nil)))


(defn- reply-xxx [ctx ev s]
  (let [ res (DefaultHttpResponse. (HttpVersion/HTTP_1_1) s)
         ch (.getChannel ctx)
         attObj (.getAttachment ch) ]
    (doto res
      (.setChunked false)
      (.setHeader "content-length", "0"))
    (doto (.write ch res)
      (.addListener (make-cf-lsnr (get attObj :keepAlive))))))

(defn- reply-get-vfile [ctx ev xdata]
  (let [ res (DefaultHttpResponse. (HttpVersion/HTTP_1_1) (HttpResponseStatus/OK))
         ch (.getChannel ctx)
         attObj (.getAttachment ch)
         clen (.size xdata) ]
    (doto res
      (.setHeader "content-type" "application/octet-stream")
      (.setHeader "content-length" (SU/nsb clen)))
    (.write ch res)
    (doto (.write ch (ChunkedStream. (.stream xdata)))
      (.addListener (make-cf-lsnr (get attObj :keepAlive))))))

(defn- filer-handler [ctx ev vdir]
  (let [ putter (fn [fname xdata]
                  (reply-xxx ctx ev (HttpResponseStatus/OK))
                  (save-vfile vdir fname xdata))
         getter (fn [fname]
                  (let [ xdata (get-vfile vdir fname) ]
                    (if (.hasContent xdata)
                      (reply-get-vfile ctx ev xdata)
                      (reply-xxx ctx ev (HttpResponseStatus/NO_CONTENT))))) ]
    (reify NettyServiceIO
      (onerror [this ctx ch exp] nil)
      (onres [this ctx ch xdata] nil)
      (onreq [this ctx ch xdata]
        (let [ mtd (-> msg (.getMethod) (.getName) (.toUpperCase))
               ch (.getChannel ctx)
               keepAlive (HttpHeaders/isKeepAlive req)
               uri (.getUri msg)
               pos (.lastIndexOf uri \/)
               p (if (< pos 0) uri (.substring uri (inc pos))) ]
          (debug "Method = " mtd ", Uri = " uri ", File = " p)
          (.setAttachment ch { :keepAlive keepAlive } )
          (cond
            (or (= mtd "POST")(= mtd "PUT")) (putter p msg)
            (= mtd "GET") (getter p)
            (:else (reply-xxx ctx ev (HttpResponseStatus/NOT_FOUND))))))
      )))

(defn make-mem-filer ^{ :doc "" }
  ([host port vdir] (make-mem-filer host port vdir nil nil))
  ([host port vdir keyUrl pwdObj]
    (make-xxx-server host port vdir keyUrl pwdObj nil)))






(def ^:private nettyio-eof nil)


