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
  comzotohcljc.crypto.stores)

(use '[clojure.tools.logging :only (info warn error debug)])
(import '(java.io File FileInputStream IOException InputStream))
(import '(java.security.cert CertificateFactory X509Certificate Certificate))
(import '(java.security KeyStore PrivateKey
  KeyStore$TrustedCertificateEntry
  KeyStore$PasswordProtection
  KeyStore$PrivateKeyEntry))
(import '(javax.net.ssl KeyManagerFactory TrustManagerFactory))
(import '(javax.security.auth.x500 X500Principal))
(require '[clojure.math.numeric-tower :as math])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.crypto.cryptors :as CR])
(require '[comzotohcljc.crypto.cryptutils :as CO])


(defn- onNewKey [^KeyStore keystore nm ^PrivateKey pkey pm]
  (let [ cc (.getCertificateChain pkey) ]
    (doseq [ c (seq cc) ]
      (.setCertificateEntry keystore (CO/new-alias) c))
    (.setEntry keystore nm pkey pm)))

(defn- getCAs [^KeyStore keystore tca root]
  (let [ en (.aliases keystore) ]
    (loop [ rc [] ]
        (if (not (.hasMoreElements en))
          rc
          (let [ a (.nextElement en) ]
            (if (.isCertificateEntry keystore a)
              (let [ cert (.getTrustedCertificate (.getEntry keystore a nil))
                     issuer (.getIssuerX500Principal cert)
                     subj (.getSubjectX500Principal cert)
                     matched (and (not (nil? issuer)) (= issuer subj)) ]
                (if (or (and root (not matched)) (and tca matched))
                  (recur rc)
                  (recur (conj rc cert))))
              (recur rc)))))))

(defprotocol CryptoStoreAPI
  (addKeyEntity [this bits pwdObj] )
  (addCertEntity [this bits] )
  (trustManagerFactory [this] )
  (keyManagerFactory [this] )
  (certAliases [this] )
  (keyAliases [this] )
  (keyEntity [this nm pwdObj] )
  (certEntity [this nm] )
  (removeEntity [this nm] )
  (intermediateCAs [this] )
  (rootCAs [this] )
  (trustedCerts [this] )
  (addPKCS7Entity [this bits] ))

(defn- mkStore [^KeyStore keystore]
  (case (.getType keystore)
    "PKCS12" (CO/get-pkcsStore)
    "JKS" (CO/get-jksStore)
    (throw (IllegalArgumentException. "wrong keystore type."))))

(deftype CryptoStore [^KeyStore keystore ^comzotohcljc.crypto.cryptors.PasswordAPI passwdObj] CryptoStoreAPI

  (addKeyEntity [this bits pwdObj]
    ;; we load the p12 content into an empty keystore, then extract the entry
    ;; and insert it into the current one.
    (let [ ch (.toCharArray pwdObj)
           tmp (doto (mkStore keystore) (.load bits ch))
           pp (KeyStore$PasswordProtection. ch)
           pkey (cast KeyStore$PrivateKeyEntry (.getEntry tmp (.nextElement (.aliases tmp)) pp)) ]
    (.onNewKey this (CO/new-alias) pkey pp)))

  (addCertEntity [this bits]
    (let [ c (cast X509Certificate (.generateCertificate (CertificateFactory/getInstance "X.509") bits)) ]
      (.setCertificateEntry keystore (CO/new-alias) c)))

  (trustManagerFactory [this]
    (doto (TrustManagerFactory/getInstance (TrustManagerFactory/getDefaultAlgorithm))
      (.init keystore)))

  (keyManagerFactory [this]
    (doto (KeyManagerFactory/getInstance (KeyManagerFactory/getDefaultAlgorithm))
      (.init keystore  (.toCharArray passwdObj))))

  (certAliases [this] (CO/cert-aliases keystore))
  (keyAliases [this] (CO/pkey-aliases keystore))

  (keyEntity [this nm pwdObj]
    (.getEntry keystore nm (KeyStore$PasswordProtection. (.toCharArray pwdObj))))

  (certEntity [this nm]
    (.getEntry keystore nm nil) )

  (removeEntity [this nm]
    (if-not (nil? nm)
      (when (.containsAlias keystore nm) (.deleteEntry keystore nm))))

  (intermediateCAs [this] (getCAs keystore true false))

  (rootCAs [this] (getCAs keystore false true))

  (trustedCerts [this]
    (map #(-> (.certEntity this %) (.getTrustedCertificate)) (.certAliases this)))

  (addPKCS7Entity [this bits]
    (let [ certs (.generateCertificates (CertificateFactory/getInstance "X.509") bits) ]
      (doseq [ c (seq certs) ]
        (.setCertificateEntry keystore (CO/new-alias) (cast Certificate c)))))

)


























(def ^:private stores-eof nil)


