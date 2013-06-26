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
  comzotohcljc.netty.nettyio
  (:use [clojure.tools.logging :only (info warn error debug)])
  (:import (org.apache.commons.io FileUtils))
  (:import (java.io IOException ByteArrayOutputStream File InputStream))
  (:import (java.util HashMap))
  (:import (java.net URL InetSocketAddress))
  (:import (java.util.concurrent Executors))
  (:import (javax.net.ssl SSLEngine SSLContext))
  (:import (org.jboss.netty.bootstrap Bootstrap ClientBootstrap ServerBootstrap))
  (:import (org.jboss.netty.channel
    ChannelFutureListener Channels SimpleChannelHandler
    ChannelPipelineFactory ChannelPipeline))
  (:import (org.jboss.netty.channel.group ChannelGroupFutureListener DefaultChannelGroup))
  (:import (org.jboss.netty.channel.socket.nio
    NioClientSocketChannelFactory NioServerSocketChannelFactory))
  (:import (org.jboss.netty.handler.codec.http HttpChunkAggregator
    HttpContentCompressor HttpHeaders HttpVersion HttpChunk
    HttpMessage HttpRequest HttpResponse HttpResponseStatus
    DefaultHttpResponse QueryStringDecoder
    HttpRequestDecoder HttpServerCodec
    HttpResponseEncoder))  
  (:import (org.jboss.netty.handler.ssl SslHandler))
  (:import (org.jboss.netty.handler.stream ChunkedStream ChunkedWriteHandler))
  (:import (com.zotoh.frwk.net NetUtils))
  (:import (com.zotoh.frwk.io XData))
  (:import (org.apache.commons.io IOUtils))  
  (:require [comzotohcljc.crypto.cryptutils :as CY])
  (:require [comzotohcljc.crypto.stores :as ST])
  (:require [comzotohcljc.net.netutils :as NU])
  (:require [comzotohcljc.util.fileutils :as FU])
  (:require [comzotohcljc.util.strutils :as SU])
  (:require [comzotohcljc.util.ioutils :as IO])
  (:require [comzotohcljc.util.coreutils :as CU])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main netty classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-server-bootstrap ^{ :doc "" }
  []
  (let [ b (ServerBootstrap. (NioServerSocketChannelFactory.
            (Executors/newCachedThreadPool)
            (Executors/newCachedThreadPool))) ]
    (doto b
      (.setOption "reuseAddress" true)
      ;; Options for its children
      (.setOption "child.receiveBufferSize" (int (* 2 1024 1024)))
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
             cs (-> (comzotohcljc.crypto.stores.CryptoStore. (doto ks (CY/init-store! (cast InputStream nil) pwdObj)) pwdObj)) ]
        (.addKeyEntity cs inp pwdObj)
        (.init ctx
          (-> cs (.keyManagerFactory ) (.getKeyManagers))
          (-> cs (.trustManagerFactory) (.getTrustManagers))
          (CY/get-srand))
        (doto (.createSSLEngine ctx) (.setUseClientMode false))))) )

(defprotocol NettyServiceIO
  (onerror [this ^Channel ch ^comzotohcljc.net.netutils.HTTPMsgInfo msginfo ^Throwable exp] )
  (onreq [this ^Channel ch ^comzotohcljc.net.netutils.HTTPMsgInfo msginfo ^XData xdata] )
  (onres [this ^Channel ch ^comzotohcljc.net.netutils.HTTPMsgInfo msginfo ^XData xdata] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make channel-future listener to close the channel
(defn- make-cf-lsnr [^comzotohcljc.net.netutils.HTTPMsgInfo msginfo]
  (reify ChannelFutureListener
    (operationComplete [_ cf]
      (when-not (:keep-alive msginfo) (.close (.getChannel cf))))))

;; turn headers into a clj-map
(defn- nio-mapheaders [^HttpMessage msg]
  (let [ names (.getHeaderNames msg) jm (HashMap.) ]
    (doseq [ n (seq names) ]
      (.put jm (.toLowerCase n) (vec (.getHeaders msg n))))
    (into {} jm)))

;; get info from http message
(defn- nio-extract-msg [^HttpMessage msg]
  (let [ pn (-> msg (.getProtocolVersion) (.toString))
         chunked (.isChunked msg)
         clen (HttpHeaders/getContentLength msg)
         kalive (HttpHeaders/isKeepAlive msg)
         headers (reduce (fn [sum n]
                      (assoc sum (.toLowerCase n) (vec (.getHeaders msg n))))
                      {} (seq (.getHeaderNames msg))) ]
    (hash-map :protocol pn :is-chunked chunked :keep-alive kalive :clen clen :headers headers)))

(defn- nio-extract-req [^HttpRequest req]
  (let [ decr (QueryStringDecoder. (.getUri req))
         md (-> req (.getMethod) (.getName))
         uri (.getPath decr)
         m1 (cast map (nio-extract-msg req))
         params (reduce (fn [sum en]
                      (assoc sum (nth en 0) (vec (nth en 1)))
                      {} (seq (.getParameters decr)))) ]
    (comzotohcljc.net.netutils.HTTPMsgInfo.
      (:protocol m1) md uri (:is-chunked m1) (:keep-alive m1) (:clen m1) (:headers m1) params)))

(defn- nio-extract-res [^HttpResponse res]
  (let [ m1 (cast map (nio-extract-msg res)) ]
    (comzotohcljc.net.netutils.HTTPMsgInfo.
      (:protocol m1) "" "" (:is-chunked m1) (:keep-alive m1) (:clen m1) (:headers m1) {})))

(defn- send-100-cont [ctx]
  (let [ ch (.getChannel ctx) ]
    (.write ch (DefaultHttpResponse. HttpVersion/HTTP_1_1  HttpResponseStatus/CONTINUE))
    nil))

(defn- nio-pcomplete [ctx msg]
  (let [ ch (.getChannel ctx)
         attObj (.getAttachment ch)
         xdata (:xs attObj )
         info (:info attObj )
         dir (:dir attObj)
         usercb (:cb attObj)
         os (:os attObj )
         clen (:clen attObj ) ]
    (when (and (> clen 0) (instance? ByteArrayOutputStream os))
      (.resetContent xdata os))
    (cond
      (= dir -1) (.onres usercb ch info xdata)
      (= dir 1) (.onreq usercb ch info xdata)
      :else nil)
    ))

(defn- nio-sockit-down [ctx msg]
  (let [ attObj (.getAttachment ctx)
         cbuf (.getContent msg)
         xdata (:xs attObj )
         cout (:os attObj )
         csum (:clen attObj )
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
    (.setAttachment ctx m)
    m))

(defn- nio-perror [ctx ev]
  (let [ ch (.getChannel ctx)
         exp (.getCause ev)
         attObj (.getAttachment ch)
         usercb (:cb attObj) ]
    (.onerror usercb ch exp)))

(defn- nio-finz [ctx]
  (let [ att (.getAttachment ctx) os (:os att) ] (IOUtils/closeQuietly os)))

(defn- nio-preq [ctx ^HttpRequest req usercb]
  (let [ msginfo (nio-extract-req req) 
         attObj (nio-cfgctx ctx { :dir 1 :info msginfo } usercb) ]
    (debug "nio-preq: received a " (:method msginfo ) " request from " (:uri msginfo))
    (when (HttpHeaders/is100ContinueExpected req) (send-100-cont ctx))
    (if (:is-chunked msginfo)
      (debug "nio-preq: request is chunked.")
      (try
        (nio-sockit-down ctx req)
        (finally (nio-finz ctx))))))

(defn- nio-presbody [ctx ^HttpResponse res]
    (if (.isChunked res)
      (debug "nio-presbody: response is chunked.")
      (try
        (nio-sockit-down ctx res)
        (finally (nio-finz ctx)))))

(defn- nio-pres [ctx ev ^HttpResponse res usercb]
  (let [ s (.getStatus res)
         r (.getReasonPhrase s)
         c (.getCode s) ]
    (debug "nio-pres: got a response: code " c " reason: " r)
    (nio-cfgctx ctx { :dir -1 :info (nio-extract-res res) } usercb)
    (cond
      (and (>= c 200) (< c 300)) (nio-presbody ctx res)
      (and (>= c 300) (< c 400)) (nio-perror ctx ev)
      :else (nio-perror ctx ev))))

(defn- nio-chunk [ctx ^HttpChunk msg]
  (try
    (nio-sockit-down ctx msg)
    (when (.isLast msg) (nio-pcomplete ctx msg))
    (catch Throwable e#
      (do (nio-finz ctx) (throw e#)))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make the channel handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- mkpipelinehdlr [usercb]
  (proxy [SimpleChannelHandler] []

    (exceptionCaught [_ ctx ev]
      (let [ ch (.getChannel ctx)
             attObj (.getAttachment ch)
             keepAlive (if (nil? attObj) false (:keep-alive attObj)) ]
        (.onerror usercb ch (.getCause ev))
        (when-not keepAlive (.close ch))))

    (messageReceived [_ ctx ev]
      (let [ msg (.getMessage ev) ]
        (cond
          (instance? HttpResponse msg) (nio-pres ctx ev (cast HttpResponse msg) usercb)
          (instance? HttpRequest msg) (nio-preq ctx (cast HttpRequest msg) usercb)
          (instance? HttpChunk msg) (nio-chunk ctx (cast HttpChunk msg))
          :else (throw (IOException. "Received some unknown object." ) ))))

    ))

(defn- mkpipelinefac [^SSLEngine ssleng usercb]
  (reify ChannelPipelineFactory
    (getPipeline [_]
      (let [ pl (org.jboss.netty.channel.Channels/pipeline) ]
        (when-not (nil? ssleng) (.addLast pl "ssl" (SslHandler. ssleng)))
        (doto pl
          (.addLast "decoder" (HttpServerCodec.))
          (.addLast "chunker" (ChunkedWriteHandler.))
          (.addLast "handler" (mkpipelinehdlr usercb)))
        pl))))

(defn- make-xxx-server ([host port keyUrl pwdObj usercb]
   (let [ ssl (if (nil? keyUrl) nil (make-ssl-context keyUrl pwdObj))
          bs (make-server-bootstrap)
          cg (make-channel-group)
          pf (mkpipelinefac ssl usercb) ]
     (.setPipelineFactory bs pf)
     (.add cg (.bind bs (InetSocketAddress. host port)))
     (debug "memxxxserver: running on host " host ", port " port)
     (-> cg (.close) (.addListener (reify ChannelGroupFutureListener
                     (operationComplete [this _]
                       (.releaseExternalResources bs))))))))

(defn make-mem-httpd ^{ :doc "" }
  ([host port vdir usercb] (make-mem-httpd host port vdir nil nil usercb))
  ([host port vdir keyUrl pwdObj usercb]
    (make-xxx-server host port keyUrl pwdObj usercb)))

(defn- reply-xxx [ch s]
  (let [ res (DefaultHttpResponse. (HttpVersion/HTTP_1_1) s)
         attObj (.getAttachment ch)
         info (if (nil? attObj) nil (:info attObj))
         kalive (if (nil? info) false (:keep-alive info)) ]
    (doto res
      (.setChunked false)
      (.setHeader "content-length", "0"))
    (doto (.write ch res)
      (.addListener (make-cf-lsnr (comzotohcljc.net.netutils.HTTPMsgInfo. "" "" "" false kalive 0 {} {}))))))

(defn- reply-get-vfile [ch xdata]
  (let [ res (DefaultHttpResponse. (HttpVersion/HTTP_1_1) (HttpResponseStatus/OK))
         clen (.size xdata)
         attObj (.getAttachment ch)
         info (if (nil? attObj) nil (:info attObj))
         kalive (if (nil? info) false (:keep-alive info)) ]
    (doto res
      (.setHeader "content-type" "application/octet-stream")
      (.setHeader "content-length" (SU/nsb clen)))
    (.write ch res)
    (doto (.write ch (ChunkedStream. (.stream xdata)))
      (.addListener (make-cf-lsnr (comzotohcljc.net.netutils.HTTPMsgInfo. "" "" "" false kalive 0 {} {}))))))

(defn- filer-handler [vdir]
  (let [ putter (fn [ch fname xdata]
                  (reply-xxx ch (HttpResponseStatus/OK))
                  (FU/save-file vdir fname xdata))
         getter (fn [ch fname]
                  (let [ xdata (FU/get-file vdir fname) ]
                    (if (.hasContent xdata)
                      (reply-get-vfile ch xdata)
                      (reply-xxx ch (HttpResponseStatus/NO_CONTENT))))) ]
    (reify NettyServiceIO
      (onerror [this ch msginfo exp] (reply-xxx ch (HttpResponseStatus/INTERNAL_SERVER_ERROR)))
      (onres [this ch msginfo xdata] nil)
      (onreq [this ch msginfo xdata]
        (let [ mtd (.toUpperCase (:method msginfo))
               uri (:uri msginfo)
               pos (.lastIndexOf uri \/)
               p (if (< pos 0) uri (.substring uri (inc pos))) ]
          (debug "Method = " mtd ", Uri = " uri ", File = " p)
          (cond
            (or (= mtd "POST")(= mtd "PUT")) (putter p xdata)
            (= mtd "GET") (getter p)
            :else (reply-xxx ch (HttpResponseStatus/METHOD_NOT_ALLOWED))))))
      ))

(defn make-mem-filer ^{ :doc "" }
  ([host port vdir] (make-mem-filer host port vdir nil nil))
  ([host port vdir keyUrl pwdObj]
    (make-xxx-server host port keyUrl pwdObj (filer-handler vdir))))





(def ^:private nettyio-eof nil)


