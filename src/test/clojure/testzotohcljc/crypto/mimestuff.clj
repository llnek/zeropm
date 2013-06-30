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

(ns testzotohcljc.crypto.mimestuff)

(use '[clojure.test])
(import '(org.apache.commons.io FileUtils IOUtils))
(import '(java.security Policy KeyStore SecureRandom))
(import '(java.util Date GregorianCalendar))
(import '(java.io File InputStream))
(import '(javax.mail Multipart BodyPart))
(import '(javax.mail.internet MimeBodyPart MimeMessage MimeMultipart))
(import '(javax.activation DataHandler DataSource))
(import '(com.zotoh.frwk.crypto SDataSource))
(import '(com.zotoh.frwk.io XData))
(require '[comzotohcljc.crypto.cryptors :as RT])
(require '[comzotohcljc.crypto.stores :as ST])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.ioutils :as IO])
(require '[comzotohcljc.util.metautils :as MU])
(require '[comzotohcljc.crypto.cryptutils :as RU])


(def ^:private ROOTPFX (CU/rc-bytes "com/zotoh/frwk/crypto/test.pfx"))
(def ^:private HELPME (RT/pwdify "helpme"))
(def ^:private ROOTCS
  (comzotohcljc.crypto.stores.CryptoStore.
                        (RU/init-store! (RU/get-pkcsStore) ROOTPFX HELPME) HELPME))

(deftest test-mimestuff-module

(is (with-open [ inp (CU/rc-stream "com/zotoh/frwk/mime/mime.eml") ]
        (let [ msg (RU/new-mimeMsg "" "" inp) mp (.getContent msg) ]
               (and (>= (.indexOf (.getContentType msg) "multipart/mixed") 0)
                    (= (.getCount mp) 2)
                    (not (RU/is-signed? mp))
                    (not (RU/is-compressed? mp))
                    (not (RU/is-encrypted? mp)) ))))

(is (with-open [ inp (CU/rc-stream "com/zotoh/frwk/mime/mime.eml") ]
      (let [ pke (.keyEntity ROOTCS (first (.keyAliases ROOTCS)) HELPME)
               msg (RU/new-mimeMsg "" "" inp)
               cs (.getCertificateChain pke)
               pk (.getPrivateKey pke)
               rc (RU/smime-digsig  pk cs RU/*SHA512* msg) ]
        (RU/is-signed? rc))))

(is (with-open [ inp (CU/rc-stream "com/zotoh/frwk/mime/mime.eml") ]
      (let [ pke (.keyEntity ROOTCS (first (.keyAliases ROOTCS)) HELPME)
               msg (RU/new-mimeMsg "" "" inp)
               mp (.getContent msg)
               cs (.getCertificateChain pke)
               pk (.getPrivateKey pke)
               rc (RU/smime-digsig  pk cs RU/*SHA512* mp) ]
        (RU/is-signed? rc))))

(is (with-open [ inp (CU/rc-stream "com/zotoh/frwk/mime/mime.eml") ]
      (let [ pke (.keyEntity ROOTCS (first (.keyAliases ROOTCS)) HELPME)
               msg (RU/new-mimeMsg "" "" inp)
               mp (.getContent msg)
               bp (.getBodyPart mp 1)
               cs (.getCertificateChain pke)
               pk (.getPrivateKey pke)
               rc (RU/smime-digsig  pk cs RU/*SHA512* bp) ]
        (RU/is-signed? rc))))

(is (with-open [ inp (CU/rc-stream "com/zotoh/frwk/mime/mime.eml") ]
      (let [ pke (.keyEntity ROOTCS (first (.keyAliases ROOTCS)) HELPME)
               msg (RU/new-mimeMsg "" "" inp)
               cs (.getCertificateChain pke)
               pk (.getPrivateKey pke)
               mp (RU/smime-digsig  pk cs RU/*SHA512* msg)
               baos (IO/make-baos)
               msg2 (doto (RU/new-mimeMsg "" "")
                      (.setContent (cast Multipart mp))
                      (.saveChanges)
                      (.writeTo baos))
               msg3 (RU/new-mimeMsg "" "" (IO/streamify (.toByteArray baos)))
               mp3 (.getContent msg3)
               rc (RU/peeksmime-signedContent mp3) ]
        (instance? Multipart rc))))

(is (with-open [ inp (CU/rc-stream "com/zotoh/frwk/mime/mime.eml") ]
      (let [ pke (.keyEntity ROOTCS (first (.keyAliases ROOTCS)) HELPME)
               msg (RU/new-mimeMsg "" "" inp)
               cs (.getCertificateChain pke)
               pk (.getPrivateKey pke)
               mp (RU/smime-digsig  pk cs RU/*SHA512* msg)
               baos (IO/make-baos)
               msg2 (doto (RU/new-mimeMsg "" "")
                      (.setContent (cast Multipart mp))
                      (.saveChanges)
                      (.writeTo baos))
               msg3 (RU/new-mimeMsg "" "" (IO/streamify (.toByteArray baos)))
               mp3 (.getContent msg3)
               rc (RU/test-smimeDigSig mp3 cs) ]
        (if (and (not (nil? rc)) (= (.size rc) 2))
          (and (instance? Multipart (nth rc 0)) (instance? (MU/bytes-class) (nth rc 1)))
          false))))


(is (let [ pke (.keyEntity ROOTCS (first (.keyAliases ROOTCS)) HELPME)
                s (SDataSource. (CU/bytesify "hello world") "text/plain")
                cs (.getCertificateChain pke)
                pk (.getPrivateKey pke)
                bp (doto (MimeBodyPart.)
                    (.setDataHandler (DataHandler. s)))
                bp2 (RU/smime-encrypt (nth cs 0) RU/*DES_EDE3_CBC* bp)
                baos (IO/make-baos)
                msg (doto (RU/new-mimeMsg)
                        (.setContent (.getContent bp2) (.getContentType bp2))
                        (.saveChanges)
                        (.writeTo baos))
                msg2 (RU/new-mimeMsg (IO/streamify (.toByteArray baos)))
                enc (RU/is-encrypted? (.getContentType msg2))
                rc (RU/smime-decrypt [pk] msg2) ]
      ;; rc is a bodypart
           (and (not (nil? rc))
              (> (.indexOf (CU/stringify rc) "hello world") 0))))

(is (let [ pke (.keyEntity ROOTCS (first (.keyAliases ROOTCS)) HELPME)
                s2 (SDataSource. (CU/bytesify "what's up dawg") "text/plain")
                s1 (SDataSource. (CU/bytesify "hello world") "text/plain")
                cs (.getCertificateChain pke)
                pk (.getPrivateKey pke)
                bp2 (doto (MimeBodyPart.)
                      (.setDataHandler (DataHandler. s2)))
                bp1 (doto (MimeBodyPart.)
                      (.setDataHandler (DataHandler. s1)))
                mp (doto (MimeMultipart.)
                     (.addBodyPart bp1)
                     (.addBodyPart bp2))
                msg (doto (RU/new-mimeMsg) (.setContent  mp))
                bp3 (RU/smime-encrypt (nth cs 0) RU/*DES_EDE3_CBC* msg)
                baos (IO/make-baos)
                msg2 (doto (RU/new-mimeMsg)
                        (.setContent (.getContent bp3) (.getContentType bp3))
                        (.saveChanges)
                        (.writeTo baos))
                msg3 (RU/new-mimeMsg (IO/streamify (.toByteArray baos)))
                enc (RU/is-encrypted? (.getContentType msg3))
                rc (RU/smime-decrypt [pk] msg3) ]
      ;; rc is a multipart
           (and (not (nil? rc))
              (> (.indexOf (CU/stringify rc) "what's up dawg") 0)
              (> (.indexOf (CU/stringify rc) "hello world") 0))))


(is (let [ pke (.keyEntity ROOTCS (first (.keyAliases ROOTCS)) HELPME)
             cs (.getCertificateChain pke)
             pk (.getPrivateKey pke)
             data (XData. "heeloo world")
             sig (RU/pkcs-digsig pk cs RU/*SHA512* data)
             dg (RU/test-pkcsDigSig (nth cs 0) data sig) ]
        (if (and (not (nil? dg)) (instance? (MU/bytes-class) dg))
          true
          false)))

(is (with-open [ inp (CU/rc-stream "com/zotoh/frwk/mime/mime.eml") ]
        (let [ msg (RU/new-mimeMsg "" "" inp)
               bp (RU/smime-compress msg) 
               x (RU/smime-decompress bp) ]
          (if (and (not (nil? x))
                    (> (alength (.javaBytes x)) 0) )
            true
            false))))

(is (let [ bp (RU/smime-compress "text/plain" (XData. "heeloo world"))
           baos (IO/make-baos)
           x (RU/smime-decompress bp) ]
          (if (and (not (nil? x))
                    (> (alength (.javaBytes x)) 0) )
            true
            false)))

(is (let [ bp (RU/smime-compress "text/plain" "blah-blah" "some-id" (XData. "heeloo world"))
           baos (IO/make-baos)
           x (RU/smime-decompress bp) ]
          (if (and (not (nil? x))
                    (> (alength (.javaBytes x)) 0) )
            true
            false)))

(is (let [ f (RU/fingerprint-sha1 (CU/bytesify "heeloo world")) ]
  (if (and (not (nil? f)) (> (.length f) 0))
    true
    false)) )

(is (let [ f (RU/fingerprint-md5 (CU/bytesify "heeloo world")) ]
  (if (and (not (nil? f)) (> (.length f) 0))
    true
    false)) )

(is (let [f (RU/fingerprint-sha1 (CU/bytesify "heeloo world"))
          g (RU/fingerprint-md5 (CU/bytesify "heeloo world")) ]
  (if (= f g) false true)))





)

(def ^:private mimestuff-eof nil)

;;(clojure.test/run-tests 'testzotohcljc.crypto.mimestuff)

