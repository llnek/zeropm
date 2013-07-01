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

(ns testzotohcljc.crypto.cryptostuff)

(use '[clojure.test])
(import '(java.security Policy KeyStore SecureRandom))
(import '(java.util Date GregorianCalendar))
(require '[comzotohcljc.crypto.cryptors :as RT])
(require '[comzotohcljc.crypto.stores :as ST])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.ioutils :as IO])
(require '[comzotohcljc.crypto.cryptutils :as RU])


(def ^:private ROOTPFX (CU/rc-bytes "com/zotoh/frwk/crypto/test.pfx"))
(def ^:private ROOTJKS (CU/rc-bytes "com/zotoh/frwk/crypto/test.jks"))
(def ^:private ENDDT (.getTime (GregorianCalendar. 2050 1 1)))
(def ^:private TESTPWD (RT/pwdify "secretsecretsecretsecretsecret"))
(def ^:private HELPME (RT/pwdify "helpme"))
(def ^:private SECRET (RT/pwdify "secret"))

(def ^:private ROOTCS 
  (comzotohcljc.crypto.stores.CryptoStore. 
                        (RU/init-store! (RU/get-pkcsStore) ROOTPFX HELPME) HELPME))

(def ^:private ROOTKS 
  (comzotohcljc.crypto.stores.CryptoStore. 
                        (RU/init-store! (RU/get-jksStore) ROOTJKS HELPME) HELPME))

(deftest test-cryptostuff-module

(is (not (= "heeloo, how are you?" (RT/caesar-decrypt (RT/caesar-encrypt "heeloo, how are you?" 709394) 666))))
(is (= "heeloo, how are you?" (RT/caesar-decrypt (RT/caesar-encrypt "heeloo, how are you?" 709394) 709394)))

(is (= "heeloo" (let [ c (comzotohcljc.crypto.cryptors.JasyptCryptor.) ]
                      (.decrypt c (.encrypt c "heeloo")))))

(is (= "heeloo" (let [ c (comzotohcljc.crypto.cryptors.JasyptCryptor.) ]
                      (.decrypt c SECRET (.encrypt c SECRET "heeloo")))))

(is (= "heeloo" (let [ c (comzotohcljc.crypto.cryptors.JavaCryptor.) ]
                      (.decrypt c (.encrypt c "heeloo")))))

(is (= "heeloo" (let [ c (comzotohcljc.crypto.cryptors.JavaCryptor.) ]
                      (.decrypt c TESTPWD (.encrypt c TESTPWD "heeloo")))))

(is (= "heeloo" (let [ c (comzotohcljc.crypto.cryptors.BouncyCryptor.) ]
                      (.decrypt c (.encrypt c "heeloo")))))

(is (= "heeloo" (let [ c (comzotohcljc.crypto.cryptors.BouncyCryptor.) ]
                      (.decrypt c TESTPWD (.encrypt c TESTPWD "heeloo")))))

(is (= (.length (.text (RT/create-strong-pwd 16))) 16))
(is (= (.length (RT/create-random-string 64)) 64))

(is (instance? comzotohcljc.crypto.cryptors.Password (RT/pwdify "secret-text")))
(is (.startsWith (.encoded (RT/pwdify "secret-text")) "CRYPT:"))


(is (= "SHA-512" (.getAlgorithm (RU/make-MsgDigest RU/*SHA_512*))))
(is (= "MD5" (.getAlgorithm (RU/make-MsgDigest RU/*MD_5*))))

(is (> (RU/next-serial) 0))

(is (instance? SecureRandom (RU/get-srand)))

(is (> (.length (RU/new-alias)) 0))

(is (= "PKCS12" (.getType (RU/get-pkcsStore))))
(is (= "JKS" (.getType (RU/get-jksStore))))

(is (instance? Policy (RU/make-easyPolicy)))

(is (> (.length (RU/gen-mac (CU/bytesify "secret") "heeloo world")) 0))
(is (> (.length (RU/gen-hash "heeloo world")) 0))

(is (not (nil? (RU/make-keypair "RSA" 1024))))

(is (let [ v (RU/make-csrreq 1024 "C=AU,ST=NSW,L=Sydney,O=Google,OU=HQ,CN=www.google.com" "PEM") ]
          (and (= (.size v) 2) (> (alength (first v)) 0) (> (alength (nth v 1)) 0))) )

(is (let [ fout (IO/make-tmpfile "kenl" ".p12")]
      (RU/make-ssv1PKCS12 (Date.) ENDDT "C=AU,ST=NSW,L=Sydney,O=Google" HELPME 1024 fout)
      (> (.length fout) 0)))

(is (let [ fout (IO/make-tmpfile "" ".jks") ]
      (RU/make-ssv1JKS (Date.) ENDDT "C=AU,ST=NSW,L=Sydney,O=Google" SECRET 1024 fout)
            (> (.length fout) 0)))

(is (let [ pke (.keyEntity ROOTCS (first (.keyAliases ROOTCS)) HELPME)
       fout (IO/make-tmpfile "" ".p12")
       pk (.getPrivateKey pke)
       cs (.getCertificateChain pke) ]
            (RU/make-ssv3PKCS12 (Date.) ENDDT "C=AU,ST=NSW,L=Sydney,O=Google" SECRET 1024  cs pk fout)
              (> (.length fout) 0)))

(is (let [ pke (.keyEntity ROOTKS (first (.keyAliases ROOTKS)) HELPME)
       fout (IO/make-tmpfile "" ".jks")
       pk (.getPrivateKey pke)
       cs (.getCertificateChain pke) ]
            (RU/make-ssv3JKS (Date.) ENDDT "C=AU,ST=NSW,L=Sydney,O=Google" SECRET 1024  cs pk fout)
              (> (.length fout) 0)))

(is (let [ fout (IO/make-tmpfile "" ".p7b") ]
        (RU/export-pkcs7 (CU/rc-url "com/zotoh/frwk/crypto/test.pfx") HELPME fout)
          (> (.length fout) 0)))


)

(def ^:private cryptostuff-eof nil)

(clojure.test/run-tests 'testzotohcljc.crypto.cryptostuff)

