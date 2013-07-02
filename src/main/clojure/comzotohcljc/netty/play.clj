(ns ^{ :doc "" :author "kenl" }
  comzotohcljc.netty.play)

(use '[clojure.tools.logging :only (info warn error debug)])
(import '(org.apache.commons.io FileUtils))
(import '(java.io IOException ByteArrayOutputStream File InputStream))
(import '(java.util HashMap))
(import '(java.net URI URL InetSocketAddress))
(import '(java.util.concurrent Executors))
(import '(javax.net.ssl SSLEngine SSLContext))
(import '(java.security KeyStore))
(import '(org.jboss.netty.bootstrap Bootstrap ClientBootstrap ServerBootstrap))
(import '(org.jboss.netty.channel
  ChannelFutureListener Channels SimpleChannelHandler
  ChannelPipelineFactory ChannelPipeline))
(import '(org.jboss.netty.channel.group ChannelGroup ChannelGroupFutureListener DefaultChannelGroup))
(import '(org.jboss.netty.channel.socket.nio
  NioClientSocketChannelFactory NioServerSocketChannelFactory))
(import '(org.jboss.netty.handler.codec.http HttpChunkAggregator
  HttpContentCompressor HttpHeaders HttpVersion HttpChunk
  HttpMessage HttpRequest HttpResponse HttpResponseStatus
  DefaultHttpResponse QueryStringDecoder
  HttpRequestDecoder HttpServerCodec
  HttpResponseEncoder))
(import '(javax.net.ssl KeyManager TrustManager KeyManagerFactory TrustManagerFactory))
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


(defn- reply-xxx [ch]
  (let [ res (DefaultHttpResponse. (HttpVersion/HTTP_1_1)  (HttpResponseStatus/OK)) ]
    (doto res
      (.setChunked false)
      (.setHeader "content-length", "0"))
    (.write ch res)
    (.close ch)))

(defn- mkpxy []

  (proxy [SimpleChannelHandler] []

    (exceptionCaught [ctx ev]
      (let [ ch (.getChannel ctx) ]
        (error (.getCause ev))
        (reply-xxx ch)))

    (messageReceived [ctx ev]
      (let [ msg (.getMessage ev) ]
        (debug "WHAT THE FUCK " (class msg))
        (debug "WHAT THE HELL " ev)
        (reply-xxx (.getChannel ctx))))

    ))

(defn make-server-bootstrap [host port]
  (let [ bs (ServerBootstrap. (NioServerSocketChannelFactory.
            (Executors/newCachedThreadPool)
            (Executors/newCachedThreadPool)))
        p12 (java.net.URL. "file:/tmp/test.p12")
         cg (DefaultChannelGroup. "asadfsf") ctx (SSLContext/getInstance "TLS")
         ks (KeyStore/getInstance "PKCS12" "BC") ts (KeyStore/getInstance "PKCS12" "BC")
          tmf (TrustManagerFactory/getInstance (TrustManagerFactory/getDefaultAlgorithm)) kmf (KeyManagerFactory/getInstance (KeyManagerFactory/getDefaultAlgorithm)) ]

    (doto bs
      (.setOption "reuseAddress" true) (.setOption "child.receiveBufferSize" (int (* 2 1024 1024))) (.setOption "child.tcpNoDelay" true))

    (with-open [inp (.openStream p12) ] (.load ks inp (.toCharArray "helpme")))

    (with-open [inp (.openStream p12) ] (.load ts inp (.toCharArray "helpme")))

    (.init kmf ks (.toCharArray "helpme")) (.init tmf ts)

    (.init ctx (.getKeyManagers kmf) (.getTrustManagers tmf) nil)

    (let [
           pipe  (reify ChannelPipelineFactory
                  (getPipeline [_]
                    (let [ pl (org.jboss.netty.channel.Channels/pipeline) 
                           eng (doto (.createSSLEngine ctx)(.setUseClientMode false))
                          ]
                      (.addLast pl "ssl" (SslHandler. eng))
                      (doto pl
                        (.addLast "decoder" (HttpRequestDecoder.))
                        (.addLast "encoder" (HttpResponseEncoder.))
                        (.addLast "chunker" (ChunkedWriteHandler.))
                        (.addLast "handler" (mkpxy)))))) ]                     
       (.setPipelineFactory bs pipe)
       (.add cg (.bind bs (InetSocketAddress. host (int port))))
       (debug "memxxxserver: running on host " host ", port " port)
       { :server bs :cgroup cg } 
      )))

(def ^:private play-eof nil)


