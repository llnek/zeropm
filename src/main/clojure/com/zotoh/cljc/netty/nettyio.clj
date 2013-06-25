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
  (:require [com.zotoh.cljc.util.coreutils :as CU])
  )

(defn- make-cf-lsnr [keeyAlive?]
  (reify ChannelFutureListener
    (operationComplete [this cf]
      (when (not keepAlive?) (.close (.getChannel cf))))))

(defn make-server-bootstrap ^{ :doc "" }
  []
  (let [ b (ServerBootstrap. (NioServerSocketChannelFactory.
            (Executors/newCachedThreadPool)
            (Executors/newCachedThreadPool))) ]
    b))

(defn make-client-bootstrap ^{ :doc "" }
  []
  (let [ bs (ClientBootstrap. (NioClientSocketChannelFactory.
                       (Executors/newCachedThreadPool) (Executors/newCachedThreadPool))) ]
    (.setOption bs "tcpNoDelay" true)
    (.setOption bs "keepAlive" true)
    bs))

(defn make-channel-group ^{ :doc "" } [] (DefaultChannelGroup. (CU/uid)))

(defn save-vfile ^{ :doc "" }
  [dir fname xdata]
  (let [ fp (File. dir fname) ]
    (FileUtils/deleteQuietly fp)
    (if (.isDiskFile xdata)
      (FileUtils/moveFile (.fileRef xdata) fp)
      (FileUtils/writeByteArrayToFile fp (.javaBytes xdata)))
    (info "nettyio: saved file: " (CU/nice-fpath fp) ", " (.length fp) " (bytes) - OK.")))

(defn get-vfile ^{ :doc "" }
  [dir fname]
  (let [ fp (File. dir fname) rc (XData.) ]
    (if (and (.exists fp) (.canRead fp))
      (doto rc (.setDeleteFile false)
              (.resetContent fp) )
      nil)) )

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
    (proxy XXX
      (onReq [this ctx ev msg]
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


