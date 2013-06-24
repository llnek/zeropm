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
  )

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

(defn make-ssl-context ^{ :doc "" }
  [keyUrl pwdObj]
  (let [ ctx (SSLContext/getInstance "TLS") ]
    (with-open [ inp (.openStream keyUrl) ]
      (let [ ks (if (-> keyUrl (.getFile) (.toLowerCase) (.endsWith ".jks")) (CY/get-jksStore) (CY/get-pkcsStore))
             cs (->CryptoStore (doto ks (CY/init-store! (cast InputStream nil) pwdObj)) pwdObj) ]
        (.addKeyEntity cs inp pwdObj)
        (.init ctx
          (-> cs (.keyManagerFactory ) (.getKeyManagers))
          (-> cs (.trustManagerFactory) (.getTrustManagers))
          (CY/get-srand))
        (doto (.createSSLEngine ctx) (.setUseClientMode false))))) )











