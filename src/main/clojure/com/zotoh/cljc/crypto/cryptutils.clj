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
  com.zotoh.cljc.crypto.cryptutils
  (:use [clojure.tools.logging :only (info warn error debug)])
  (:require [clojure.math.numeric-tower :as math])
  (:import (java.io File FileInputStream ByteArrayOutputStream ByteArrayInputStream InputStreamReader))
  (:import (java.math BigInteger))
  (:import (java.util Random Date))
  (:import (javax.activation DataHandler CommandMap MailcapCommandMap))
  (:import (javax.mail BodyPart MessagingException Multipart Session ))
  (:import (javax.mail.internet ContentType MimeBodyPart MimeMessage MimeMultipart MimeUtility ))

  (:import (org.bouncycastle.asn1 ASN1ObjectIdentifier))
  (:import (org.bouncycastle.cms CMSAlgorithm))
  (:import (java.security KeyStore$PasswordProtection))
  (:import (java.security KeyStore$PrivateKeyEntry))
  (:import (java.security Policy PermissionCollection CodeSource
    Permissions KeyPair KeyPairGenerator KeyStore
    MessageDigest PrivateKey Provider PublicKey
    AllPermission SecureRandom Security))
  (:import (java.security.cert CertificateFactory Certificate X509Certificate))
  (:import (org.bouncycastle.jce.provider BouncyCastleProvider))
  (:import (org.bouncycastle.asn1.x509 X509Extension))
  (:import (org.bouncycastle.cert.jcajce JcaCertStore
    JcaX509CertificateConverter
    JcaX509ExtensionUtils
    JcaX509v1CertificateBuilder
    JcaX509v3CertificateBuilder))
  (:import (org.bouncycastle.cms CMSProcessableByteArray CMSSignedDataGenerator CMSSignedGenerator))
  (:import (org.bouncycastle.cms.jcajce JcaSignerInfoGeneratorBuilder))
  (:import (org.bouncycastle.openssl PEMParser PEMReader))
  (:import (org.bouncycastle.operator DigestCalculatorProvider ContentSigner))
  (:import (org.bouncycastle.operator.jcajce JcaDigestCalculatorProviderBuilder JcaContentSignerBuilder))
  (:import (org.bouncycastle.pkcs PKCS10CertificationRequestBuilder PKCS10CertificationRequest))
  (:import (org.bouncycastle.pkcs.jcajce JcaPKCS10CertificationRequestBuilder))
  (:import (javax.crypto Cipher KeyGenerator Mac SecretKey))
  (:import (javax.crypto.spec SecretKeySpec))
  (:import (javax.security.auth.x500 X500Principal))
  (:import (org.apache.commons.codec.binary Hex Base64))
  (:import (org.apache.commons.lang3 StringUtils))
  (:import (org.apache.commons.io FileUtils IOUtils))
  (:require [com.zotoh.cljc.util.seqnumgen :as SN])  
  (:require [com.zotoh.cljc.crypto.cryptors :as CR])
  (:require [com.zotoh.cljc.util.coreutils :as CU])
  (:require [com.zotoh.cljc.util.strutils :as SU])
  )

(def ^:dynamic *DES_EDE3_CBC* CMSAlgorithm/DES_EDE3_CBC)
(def ^:dynamic *RC2_CBC* CMSAlgorithm/RC2_CBC)
(def ^:dynamic *IDEA_CBC* CMSAlgorithm/IDEA_CBC)
(def ^:dynamic *CAST5_CBC* CMSAlgorithm/CAST5_CBC)
(def ^:dynamic *AES128_CBC* CMSAlgorithm/AES128_CBC)
(def ^:dynamic *AES192_CBC* CMSAlgorithm/AES192_CBC)
(def ^:dynamic *AES256_CBC* CMSAlgorithm/AES256_CBC)
(def ^:dynamic *CAMELLIA128_CBC* CMSAlgorithm/CAMELLIA128_CBC)
(def ^:dynamic *CAMELLIA192_CBC* CMSAlgorithm/CAMELLIA192_CBC)
(def ^:dynamic *CAMELLIA256_CBC* CMSAlgorithm/CAMELLIA256_CBC)
(def ^:dynamic *SEED_CBC* CMSAlgorithm/SEED_CBC)
(def ^:dynamic *DES_EDE3_WRAP* CMSAlgorithm/DES_EDE3_WRAP)
(def ^:dynamic *AES128_WRAP* CMSAlgorithm/AES128_WRAP)
(def ^:dynamic *AES256_WRAP* CMSAlgorithm/AES256_WRAP)
(def ^:dynamic *CAMELLIA128_WRAP* CMSAlgorithm/CAMELLIA128_WRAP)
(def ^:dynamic *CAMELLIA192_WRAP* CMSAlgorithm/CAMELLIA192_WRAP)
(def ^:dynamic *CAMELLIA256_WRAP* CMSAlgorithm/CAMELLIA256_WRAP)
(def ^:dynamic *SEED_WRAP* CMSAlgorithm/SEED_WRAP)
(def ^:dynamic *ECDH_SHA1KDF* CMSAlgorithm/ECDH_SHA1KDF)

(def ^:dynamic *EXPLICIT_SIGNING* "EXPLICIT")
(def ^:dynamic *IMPLICIT_SIGNING* "IMPLICIT")
(def ^:dynamic *DER_CERT* "DER")
(def ^:dynamic *PEM_CERT* "PEM")

(def ^:dynamic *SHA512* "SHA512withRSA")
(def ^:dynamic *SHA256* "SHA256withRSA")
(def ^:dynamic *SHA1* "SHA1withRSA")
(def ^:dynamic *SHA_512* "SHA-512")
(def ^:dynamic *SHA_1* "SHA-1")
(def ^:dynamic *SHA_256* "SHA-256")
(def ^:dynamic *MD_5* "MD5")
(def ^:dynamic *MD5* "MD5withRSA")

(def ^:dynamic *BFISH* "BlowFish")
(def ^:dynamic *PKCS12* "PKCS12")
(def ^:dynamic *JKS* "JKS")
(def ^:dynamic *SHA1* "SHA1")
(def ^:dynamic *MD5* "MD5")
(def ^:dynamic *AES256_CBC*  "AES256_CBC")
(def ^:dynamic *RAS*  "RAS")
(def ^:dynamic *DES*  "DES")
(def ^:dynamic *RSA*  "RSA")
(def ^:dynamic *DSA*  "DSA")

(def ^:private DEF_ALGO "SHA1WithRSAEncryption")
(def ^:private DEF_MAC "HmacSHA512")

(defn nextSerialNumber "" []
  (let [ r (Random. (.getTime (Date.))) ]
    (BigInteger/valueOf (math/abs (.nextLong r)))) )

;; this function should fail if the non-restricted (unlimited-strength) jce files are not placed in jre-home
(defn assertJCEPolicy "" []
  (let [ kgen (KeyGenerator/getInstance *BFISH*)
         d1 (.init kgen 256)
         cipher (Cipher/getInstance *BFISH*)
         d2 (.init cipher (Cipher/ENCRYPT_MODE) (SecretKeySpec. (.. kgen generateKey getEncoded) *BFISH*)) ]
    (.doFinal cipher (.getBytes "This is just an example" "utf-8"))))

(def ^:dynamic *BCProvider* (eval '(let[ bcp (BouncyCastleProvider.) ] (Security/addProvider bcp) bcp)) )
(eval '(assertJCEPolicy))
(eval '(let [ mc (cast MailcapCommandMap (CommandMap/getDefaultCommandMap)) ]
         (.addMailcap mc (str "application/pkcs7-signature;; " 
                    "x-java-content-handler=org.bouncycastle.mail.smime.handlers.pkcs7_signature"))
         (.addMailcap mc (str "application/pkcs7-mime;; " "x-java-content-handler=org.bouncycastle.mail.smime.handlers.pkcs7_mime"))
         (.addMailcap mc (str "application/x-pkcs7-signature;; " 
                  "x-java-content-handler=org.bouncycastle.mail.smime.handlers.x_pkcs7_signature") )
         (.addMailcap mc (str "application/x-pkcs7-mime;; " 
                    "x-java-content-handler=org.bouncycastle.mail.smime.handlers.x_pkcs7_mime"))
         (.addMailcap mc (str "multipart/signed;; " 
                  "x-java-content-handler=org.bouncycastle.mail.smime.handlers.multipart_signed") )))

(defn getSecureRandom "" [] (SecureRandom/getInstance "SHA1PRNG" ))
(defn newAlias [] (str "" (System/currentTimeMillis) (SN/nextInt)))

(defn getJKSStore []
  (let [ ks (KeyStore/getInstance "JKS" (Security/getProvider "SUN")) ]
    (.load ks nil)
    ks))

(defn getP12Store []
  (let [ ks (KeyStore/getInstance "PKCS12" *BCProvider*) ]
    (.load ks nil)
    ks))

(defn initStoreBytes! [^KeyStore store ^bytes bits pwd]
  (let [ ca (if (nil? pwd) nil (.toCharArray pwd)) ]
    (if (nil? bits)
      (.load store nil ca)
      (.load store (ByteArrayInputStream. bits) ca) )
    store))

(defn initStore! [^KeyStore store ^File f pwd]
  (let [ ca (if (nil? pwd) nil (.toCharArray pwd)) ]
    (if (nil? f)
      (.load store nil ca)
      (with-open [ inp (FileInputStream. f) ]
        (.load store inp ca)))
    store))

(defn bitsToCert "" [^bytes certBits]
  (let [ ks (getP12Store)
         nm (newAlias)
         c (cast X509Certificate (.generateCertificate (CertificateFactory/getInstance "X.509") (ByteArrayInputStream. certBits))) ]
      (.setCertificateEntry ks nm c)
      (.getEntry ks nm nil)) )

(defn bitsToKey "" [ ^bytes privateKeyBits pwd]
  (let [ ks (getP12Store) ]
    (.load ks (ByteArrayInputStream. privateKeyBits) pwd)
    (.getEntity ks (aget (.keyAliases ks) 0) (KeyStore$PasswordProtection. (.toCharArray pwd)))) )


(defn mkSimplePolicy "" []
  (proxy [Policy] []
    (getPermissions [this cs]
      (let [ p (Permissions.) ]
        (.add p (AllPermission.))
        p))))

(defn genMAC
  ""
  ([ ^bytes skey data ] (genMAC skey data DEF_MAC))
  ([ ^bytes skey data algo]
    (let [ mac (Mac/getInstance algo  *BCProvider*) ]
      (.init mac (SecretKeySpec. skey algo))
      (.update mac (.getBytes data "utf-8"))
      (Hex/encodeHexString (.doFinal mac)))) )

(defn genHash
  ""
  ([data] (genHash data *SHA_512*))
  ([data algo]
    (let [ dig (MessageDigest/getInstance algo)
           b (.digest dig (.getBytes data "utf-8")) ]
      (Base64/encodeBase64String b))) )

(defn mkKeyPair "" [ algo keyLength]
  (let [ kpg (KeyPairGenerator/getInstance algo *BCProvider*) ]
    (.initialize kpg keyLength (getSecureRandom))
    (.generateKeyPair kpg)) )

(defn- loadPKCS12Key "" [^File p12File pwd]
  (with-open [ inp (FileInputStream. p12File) ]
    (let [ ks (KeyStore/getInstance "PKCS12" *BCProvider*)
           ca (.toCharArray pwd)
           d1 (.load ks inp ca)
           rc (.getEntry ks (.nextElement (.aliases ks)) (KeyStore$PasswordProtection. ca)) ]
      rc)) )

(defn- fmtPEM "" [top end ^bytes bits]
  (let [ baos (ByteArrayOutputStream.)
         bs (Base64/encodeBase64 bits)
         len (alength bs)
         bb (byte-array 1) ]
    (.write baos (.getBytes top "utf-8"))
    (loop [pos 0]
      (if (= pos len)
        (do (.write baos (.getBytes end "utf-8"))
          (.toByteArray baos))
        (do
          (when (and (> pos 0) (= (mod pos 64) 0)) (.write baos (.getBytes "\n" "utf-8")))
          (aset bb 0 (aget bs pos))
          (.write baos bb)
          (recur (inc pos)))))) )

(defn exportPrivateKey "" [ ^PrivateKey pkey fmt ]
  (let [ bits (.getEncoded pkey) ]
    (if (= fmt *PEM_CERT*)
      (fmtPEM "-----BEGIN RSA PRIVATE KEY-----\n" "\n-----END RSA PRIVATE KEY-----\n" bits)
      bits)) )

(defn exportCertificate "" [ ^X509Certificate cert fmt ]
  (let [ bits (.getEncoded cert) ]
    (if (= fmt *PEM_CERT*)
      (fmtPEM "-----BEGIN CERTIFICATE-----\n" "-----END CERTIFICATE-----\n" bits)
      bits)) )


(defn mkCSR
  ""
  [keyLength dnStr fmt]
  (do
    (debug "cryptoutils: mkCSR: dnStr= " dnStr ", key-len= " keyLength)
    (let [ kp (mkKeyPair *RSA* keyLength)
           k (.getPrivate kp)
           cs (.build (.setProvider (JcaContentSignerBuilder. DEF_ALGO) *BCProvider*) k)
           bits (.getEncoded (.build (JcaPKCS10CertificationRequestBuilder. (X500Principal. dnStr) (.getPublic kp)) cs))
           rc (if (= fmt *PEM_CERT*)
                (fmtPEM "-----BEGIN CERTIFICATE REQUEST-----\n" "\n-----END CERTIFICATE REQUEST-----\n" bits)
                bits) ]
      [ rc (exportPrivateKey k fmt) ] )) )

;; generate self-signed cert
;; self signed-> issuer is self
(defn- mkSSV1Cert "" [ pv kp start end dnStr keyLength algo]
  (let [ dnName (X500Principal. dnStr)
         prv (.getPrivate kp)
         pub (.getPublic kp)
         bdr (JcaX509v1CertificateBuilder. dnName (nextSerialNumber) start end dnName pub)
         cs (.build (.setProvider (JcaContentSignerBuilder. algo) pv) prv)
         cert (.getCertificate (.setProvider (JcaX509CertificateConverter.) pv) (.build bdr cs)) ]
    (.checkValidity cert (Date.))
    (.verify cert pub)
    [cert prv]))

(defn- mkSSV1 "" [ ^KeyStore ks ^KeyPair kp algo friendlyName ^Date start ^Date end dnStr pwd keyLength ^File out]
  (do
    (debug "mkSSV1: dn= " dnStr ", key-len= " keyLength)
    (let [ props (mkSSV1Cert (.getProvider ks) kp start end dnStr keyLength algo)
           ca (.toCharArray pwd)
           baos (ByteArrayOutputStream. (int 4096)) ]
      (.setKeyEntry ks friendlyName (nth props 1) ca (into-array Certificate (nth props 0)))
      (.store ks baos ca)
      (FileUtils/write out (.toByteArray baos)) )) )

(defn mkPKCS12 "" [friendlyName ^bytes keyPEM ^bytes certPEM pwd ^File out]
  (let [ ct (.getTrustedCertificate (bitsToCert certPEM))
         rdr (InputStreamReader. (ByteArrayInputStream. keyPEM))
         ss (getP12Store)
         baos (ByteArrayOutputStream. (int 4096))
         kp (cast KeyPair (.readObject (PEMParser. rdr))) ]
    (.setKeyEntry ss friendlyName (.getPrivate kp) (.toCharArray pwd) (into-array [ct]))
    (.store ss baos (.toCharArray pwd))
    (FileUtils/write out (.toByteArray baos))))

(defn mkSSV1PKCS12 "" [friendlyName ^Date start ^Date end dnStr pwd keyLength ^File out]
  (let [ ks (KeyStore/getInstance "PKCS12" *BCProvider*)
         d1 (.load ks nil nil)
         kp (mkKeyPair *RSA* keyLength) ]
    (mkSSV1 ks kp DEF_ALGO friendlyName start end dnStr pwd keyLength out)) )

(defn mkSSV1JKS "" [friendlyName ^Date start ^Date end dnStr pwd keyLength ^File out]
  (let [ ks (KeyStore/getInstance "JKS" "SUN")
         d1 (.load ks nil nil)
         kp (mkKeyPair *DSA* keyLength) ]
    (mkSSV1 ks kp "SHA1withDSA" friendlyName start end dnStr pwd keyLength out)) )

(defn- mkSSV3Cert "" [pv kp start end dnStr issuer issuerKey keyLength algo]
  (let [ subject (X500Principal. dnStr)
         top (cast X509Certificate issuer)
         prv (.getPrivate kp)
         pub (.getPublic kp)
         bdr (JcaX509v3CertificateBuilder. top (nextSerialNumber) start end subject pub)
         exUte (JcaX509ExtensionUtils.)
         cs (.build (.setProvider (JcaContentSignerBuilder. algo) pv) issuerKey) ]
    (.addExtension bdr (X509Extension/authorityKeyIdentifier) false (.createAuthorityKeyIdentifier exUte top))
    (.addExtension bdr (X509Extension/subjectKeyIdentifier) false (.createSubjectKeyIdentifier exUte pub))
    (let [ cert (.getCertificate (.setProvider (JcaX509CertificateConverter.) pv) (.build bdr cs)) ]
      (.checkValidity cert (Date.))
      (.verify cert (.getPublicKey top))
      [cert prv] )))

(defn- mkSSV3 "" [ ^KeyStore ks ^KeyPair kp algo friendlyName ^Date start ^Date end dnStr pwd keyLength issuerCerts issuerKey ^File out]
  (do
    (debug "mkSSV3: dn= " dnStr ", key-len= " keyLength)
    (let [ props (mkSSV3Cert (.getProvider ks) kp start end dnStr (aget issuerCerts 0) issuerKey keyLength algo)
           ca (.toCharArray pwd)
           baos (ByteArrayOutputStream. (int 4096))
           cs (cons (nth props 0) (seq issuerCerts)) ]
      (.setKeyEntry ks friendlyName (nth props 1) ca (to-array cs))
      (.store ks baos ca)
      (FileUtils/write out (.toByteArray baos)))) )

(defn mkSSV3PKCS12 "" [friendlyName ^Date start ^Date end dnStr pwd keyLength issuerCerts issuerKey ^File out]
  (let [ ks (KeyStore/getInstance "PKCS12" *BCProvider*)
         d1 (.load ks nil nil)
         kp (mkKeyPair *RSA* keyLength) ]
    (mkSSV3 ks kp DEF_ALGO friendlyName start end dnStr pwd keyLength issuerCerts issuerKey out)) )

(defn mkSSV3JKS "" [friendlyName ^Date start ^Date end dnStr pwd keyLength issuerCerts issuerKey ^File out]
  (let [ ks (KeyStore/getInstance "JKS" "SUN")
         d1 (.load ks nil nil)
         kp (mkKeyPair *DSA* keyLength) ]
    (mkSSV3 ks  kp  "SHA1withDSA" friendlyName start end dnStr pwd keyLength issuerCerts issuerKey out)))


(defn exportPKCS7 "" [ ^File p12File pwd ^File fileOut]
  (let [ pkey (loadPKCS12Key p12File pwd)
         cc (.getCertificateChain pkey)
         cl (list (seq cc))
         cp (.build (.setProvider (JcaDigestCalculatorProviderBuilder.) *BCProvider*))
         bdr (JcaSignerInfoGeneratorBuilder. cp)
;;    "SHA1withRSA"
         cs (.build (.setProvider (JcaContentSignerBuilder. (CMSSignedGenerator/DIGEST_SHA512)) *BCProvider*) (.getPrivateKey pkey))
         gen (CMSSignedDataGenerator.) ]
    (.addSignerInfoGenerator gen (.build bdr cs (cast X509Certificate (aget cc 0))))
    (.addCertificates gen (JcaCertStore. cl))
    (let [ bits (.getEncoded (.generate gen (CMSSignedGenerator/DATA)
                                        (CMSProcessableByteArray. (.getBytes "Hello")) false *BCProvider* false)) ]
      (FileUtils/write fileOut bits))) )


(defn newSession "" [user pwd]
  (Session/getInstance (System/getProperties)
        (if (StringUtils/isEmpty user) nil (DefaultAuthenticator. user (SU/nsb pwd)) )))

(defn newMimeMsg ""
  ([user pwd] (newMimMsg user pwd nil))
  ([user pwd inp]
      (let [ s (newSession user pwd) ]
        (if (nil? inp) (MimeMessage s) (MimeMessage s inp)))) )





(def ^:private cryptoutils-eof nil)


