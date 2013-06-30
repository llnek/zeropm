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
  comzotohcljc.crypto.cryptutils)

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[clojure.math.numeric-tower :as math])
(import '(java.io PrintStream File InputStream IOException
  ByteArrayOutputStream ByteArrayInputStream
  FileInputStream InputStreamReader))
(import '(java.security GeneralSecurityException))
(import '(java.math BigInteger))
(import '(java.net URL))
(import '(java.util Random Date))
(import '(javax.activation DataHandler CommandMap MailcapCommandMap))
(import '(javax.mail BodyPart MessagingException Multipart Session ))
(import '(javax.mail.internet ContentType
  MimeBodyPart MimeMessage MimeMultipart MimeUtility ))
(import '(org.bouncycastle.asn1 ASN1ObjectIdentifier))
(import '(org.bouncycastle.cms CMSAlgorithm))
(import '(org.bouncycastle.cert X509CertificateHolder))
(import '(java.security KeyStore$PasswordProtection))
(import '(java.security KeyStore$PrivateKeyEntry))
(import '(java.security Policy PermissionCollection CodeSource
  Permissions KeyPair KeyPairGenerator KeyStore
  MessageDigest PrivateKey Provider PublicKey
  AllPermission SecureRandom Security))
(import '(java.security.cert CertificateFactory
  Certificate X509Certificate))
(import '(org.bouncycastle.jce.provider BouncyCastleProvider))
(import '(org.bouncycastle.asn1.x509 X509Extension))
(import '(org.bouncycastle.asn1 ASN1EncodableVector))
(import '(org.bouncycastle.asn1.cms AttributeTable IssuerAndSerialNumber))
(import '(org.bouncycastle.asn1.smime SMIMECapabilitiesAttribute
  SMIMECapability
  SMIMECapabilityVector
  SMIMEEncryptionKeyPreferenceAttribute))
(import '(org.bouncycastle.asn1.x500 X500Name))
(import '(org.bouncycastle.cms CMSCompressedDataParser
  CMSException
  CMSProcessable
  CMSProcessableByteArray
  CMSProcessableFile
  CMSSignedData
  CMSSignedDataGenerator
  CMSTypedData
  CMSTypedStream
  DefaultSignedAttributeTableGenerator
  Recipient
  RecipientInfoGenerator
  RecipientInformation
  SignerInformation))
(import '(org.bouncycastle.cms.jcajce JcaSignerInfoGeneratorBuilder
  JcaSimpleSignerInfoVerifierBuilder
  JceCMSContentEncryptorBuilder
  JceKeyTransEnvelopedRecipient
  JceKeyTransRecipientInfoGenerator
  ZlibExpanderProvider))
(import '(org.bouncycastle.mail.smime SMIMECompressedGenerator
  SMIMEEnveloped
  SMIMEEnvelopedGenerator
  SMIMEException
  SMIMESigned
  SMIMESignedGenerator
  SMIMESignedParser))
(import '(org.bouncycastle.operator OperatorCreationException ContentSigner))
(import '(org.bouncycastle.operator.jcajce JcaDigestCalculatorProviderBuilder JcaContentSignerBuilder))
(import '(org.bouncycastle.util Store))
(import '(org.bouncycastle.operator.bc BcDigestCalculatorProvider))
(import '(javax.security.auth.x500 X500Principal))
(import '(org.bouncycastle.cms.jcajce JceKeyTransRecipientId))
(import '(org.bouncycastle.mail.smime SMIMEEnvelopedParser))
(import '(org.apache.commons.mail DefaultAuthenticator))
(import '(org.bouncycastle.cert.jcajce JcaCertStore
  JcaX509CertificateConverter
  JcaX509ExtensionUtils
  JcaX509v1CertificateBuilder
  JcaX509v3CertificateBuilder))
(import '(org.bouncycastle.cms CMSProcessableByteArray
  CMSSignedDataGenerator CMSSignedGenerator))
(import '(org.bouncycastle.cms.jcajce JcaSignerInfoGeneratorBuilder))
(import '(org.bouncycastle.openssl PEMParser PEMReader))
(import '(org.bouncycastle.operator
  DigestCalculatorProvider ContentSigner))
(import '(org.bouncycastle.operator.jcajce
  JcaDigestCalculatorProviderBuilder JcaContentSignerBuilder))
(import '(org.bouncycastle.pkcs
  PKCS10CertificationRequestBuilder PKCS10CertificationRequest))
(import '(org.bouncycastle.pkcs.jcajce
  JcaPKCS10CertificationRequestBuilder))
(import '(javax.crypto Cipher
  KeyGenerator Mac SecretKey))
(import '(javax.crypto.spec SecretKeySpec))
(import '(javax.security.auth.x500 X500Principal))
(import '(org.apache.commons.codec.binary Hex Base64))
(import '(org.apache.commons.lang3 StringUtils))
(import '(org.apache.commons.io FileUtils IOUtils))
(import '(com.zotoh.frwk.crypto SDataSource))
(import '(com.zotoh.frwk.io XData))
(require '[comzotohcljc.util.seqnumgen :as SN])
(require '[comzotohcljc.crypto.cryptors :as CR])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.mimeutils :as MM])
(require '[comzotohcljc.util.ioutils :as IO])
(require '[comzotohcljc.util.strutils :as SU])


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

(def ^:dynamic *AES256_CBC*  "AES256_CBC")
(def ^:dynamic *BFISH* "BlowFish")
(def ^:dynamic *PKCS12* "PKCS12")
(def ^:dynamic *JKS* "JKS")
(def ^:dynamic *SHA1* "SHA1")
(def ^:dynamic *MD5* "MD5")
(def ^:dynamic *RAS*  "RAS")
(def ^:dynamic *DES*  "DES")
(def ^:dynamic *RSA*  "RSA")
(def ^:dynamic *DSA*  "DSA")

(def ^:private DEF_ALGO "SHA1WithRSAEncryption")
(def ^:private DEF_MAC "HmacSHA512")

(defn assert-jce ^{ :doc "this function should fail if the non-restricted (unlimited-strength) jce files are not placed in jre-home" }
  []
  (let [ kgen (doto (KeyGenerator/getInstance *BFISH*) (.init 256))
         cipher (doto (Cipher/getInstance *BFISH*)
                      (.init (Cipher/ENCRYPT_MODE)
                             (SecretKeySpec. (.. kgen generateKey getEncoded) *BFISH*))) ]
    (.doFinal cipher (CU/bytesify "This is just an example"))))

(def ^:dynamic *BCProvider* (eval '(let[ bcp (BouncyCastleProvider.) ] (Security/addProvider bcp) bcp)) )
(eval '(assert-jce))
(eval '(doto (cast MailcapCommandMap (CommandMap/getDefaultCommandMap))
         (.addMailcap (str "application/pkcs7-signature;; "
                    "x-java-content-handler=org.bouncycastle.mail.smime.handlers.pkcs7_signature"))
         (.addMailcap (str "application/pkcs7-mime;; " "x-java-content-handler=org.bouncycastle.mail.smime.handlers.pkcs7_mime"))
         (.addMailcap (str "application/x-pkcs7-signature;; "
                  "x-java-content-handler=org.bouncycastle.mail.smime.handlers.x_pkcs7_signature") )
         (.addMailcap (str "application/x-pkcs7-mime;; "
                    "x-java-content-handler=org.bouncycastle.mail.smime.handlers.x_pkcs7_mime"))
         (.addMailcap (str "multipart/signed;; "
                  "x-java-content-handler=org.bouncycastle.mail.smime.handlers.multipart_signed") )))

(defn make-MsgDigest ^{ :doc "Get a message digest instance." }
  [^String algo]
  (MessageDigest/getInstance (SU/nsb algo) *BCProvider*))

(defn next-serial ^{ :doc "Get a random Big Integer." }
  []
  (let [ r (Random. (.getTime (Date.))) ]
    (BigInteger/valueOf (math/abs (.nextLong r)))) )

(defn dbg-provider ^{ :doc "" }
  [^PrintStream os]
  (CU/TryC
    (.list *BCProvider* os)))

(defn get-srand ^{ :doc "Get a secure random." }
  []
  (SecureRandom/getInstance "SHA1PRNG" ))

(defn new-alias  ^{ :doc "" }
  []
  (str "" (System/currentTimeMillis) (SN/next-int)))

(defn get-pkcsStore ^{ :doc "Create a PKCS12 key-store." }
  []
  (doto (KeyStore/getInstance "PKCS12" *BCProvider*)
    (.load nil)))

(defn get-jksStore ^{ :doc "Create a JKS key-store." }
  []
  (doto (KeyStore/getInstance "JKS" (Security/getProvider "SUN"))
    (.load nil)))

(defmulti ^{ :doc "Initialize the key-store." } init-store!
  (fn [a b c]
    (cond
      (instance? InputStream b) :stream
      (instance? File b) :file
      :else :bytes)))

(defmethod init-store! :stream [^KeyStore store ^InputStream inp ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj]
  (let [ ca (if (nil? pwdObj) nil (.toCharArray pwdObj)) ]
    (doto store (.load (if (nil? inp) nil inp) ca))))

(defmethod init-store! :bytes [^KeyStore store ^bytes bits ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj]
  (init-store! store (IO/streamify bits) pwdObj))

(defmethod init-store! :file [^KeyStore store ^File f ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj]
  (with-open [ inp (FileInputStream. f) ]
    (init-store! store inp pwdObj)))

(defn- find-aliases [^KeyStore keystore pred]
  (let [ en (.aliases keystore) ]
    (loop [ rc [] ]
      (if (not (.hasMoreElements en))
        rc
        (let [ n (.nextElement en) ]
          (if (pred keystore n)
            (recur (conj rc n))
            (recur rc)))))))

(defn cert-aliases ^{ :doc "Enumerate all cert aliases in the key-store." }
  [^KeyStore keystore]
  (find-aliases keystore (fn [ks n] (.isCertificateEntry ks n))))

(defn pkey-aliases ^{ :doc "Enumerate all key aliases in the key-store." }
  [^KeyStore keystore]
  (find-aliases keystore (fn [ks n] (.isKeyEntry ks n))))

(defn conv-cert ^{ :doc "Returns a KeyStore$TrustedCertificateEntry." }
  [^bytes bits]
  (let [ ks (get-pkcsStore)
         nm (new-alias)
         c (-> (CertificateFactory/getInstance "X.509")
                      (.generateCertificate  (IO/streamify bits))) ]
    (.setCertificateEntry ks nm (cast X509Certificate c))
    (.getEntry ks nm nil)) )

(defn conv-pkey ^{ :doc "Returns a KeyStore$PrivateKeyEntry." }
  [^bytes bits ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj]
  (let [ ca (if (nil? pwdObj) nil (.toCharArray pwdObj))
         ks (get-pkcsStore) ]
    (.load ks (IO/streamify bits) ca)
    (.getEntity ks (first (pkey-aliases ks)) (KeyStore$PasswordProtection. ca))))

(defn make-easyPolicy ^{ :doc "Make a Policy that enables all permissions." }
  []
  (proxy [Policy] []
    (getPermissions [this cs]
      (doto (Permissions.) (.add (AllPermission.))))))

(defn gen-mac ^{ :doc "Generate a Message Auth Code." }
  ([^bytes skey ^String data] (gen-mac skey data DEF_MAC))
  ([^bytes skey ^String data ^String algo]
    (let [ mac (doto (Mac/getInstance algo *BCProvider*)
                     (.init (SecretKeySpec. skey algo))
                     (.update (CU/bytesify data))) ]
      (Hex/encodeHexString (.doFinal mac)))))

(defn gen-hash ^{ :doc "Generate a Message Digest." }
  ([^String data] (gen-hash data *SHA_512*))
  ([^String data ^String algo]
    (let [ dig (MessageDigest/getInstance algo)
           b (.digest dig (CU/bytesify data)) ]
      (Base64/encodeBase64String b))) )

(defn make-keypair ^{ :doc "Make a Asymmetric key-pair." }
  [^String algo ^long keylen]
  (let [ kpg (doto (KeyPairGenerator/getInstance algo *BCProvider*)
                   (.initialize keylen (get-srand))) ]
    (.generateKeyPair kpg)) )

(defn- loadPKCS12Key [^URL p12File pwdObj]
  (with-open [ inp (.openStream p12File) ]
    (let [ ca (if (nil? pwdObj) nil (.toCharArray pwdObj))
           ks (doto (get-pkcsStore)
                    (.load inp ca))
           rc (.getEntry ks (SU/nsb (.nextElement (.aliases ks))) (KeyStore$PasswordProtection. ca)) ]
      rc)) )

(defn- fmtPEM [top end ^bytes bits]
  (let [ bs (Base64/encodeBase64 bits)
         nl (CU/bytesify "\n")
         baos (IO/make-baos)
         len (alength bs)
         bb (byte-array 1) ]
    (.write baos (CU/bytesify top))
    (loop [pos 0]
      (if (= pos len)
        (do (.write baos (CU/bytesify end)) (.toByteArray baos))
        (do
          (when (and (> pos 0) (= (mod pos 64) 0)) (.write baos nl))
          (aset bb 0 (aget bs pos))
          (.write baos bb)
          (recur (inc pos)))))) )

(defn export-pkey ^{ :doc "Export Private Key." }
  [^PrivateKey pkey ^String fmt]
  (let [ bits (.getEncoded pkey) ]
    (if (= fmt *PEM_CERT*)
      (fmtPEM "-----BEGIN RSA PRIVATE KEY-----\n" "\n-----END RSA PRIVATE KEY-----\n" bits)
      bits)) )

(defn export-cert ^{ :doc "Export Certificate." }
  [^X509Certificate cert ^String fmt]
  (let [ bits (.getEncoded cert) ]
    (if (= fmt *PEM_CERT*)
      (fmtPEM "-----BEGIN CERTIFICATE-----\n" "-----END CERTIFICATE-----\n" bits)
      bits)) )

(defn make-csrreq ^{ :doc "Make a PKCS10 - csr-request." }
  [^long keylen ^String dnStr ^String fmt]
  (do
    (debug "cryptoutils: make-csrreq: dnStr= " dnStr ", key-len= " keylen)
    (let [ kp (make-keypair *RSA* keylen)
           k (.getPrivate kp)
           u (.getPublic kp)
           csb (JcaContentSignerBuilder. DEF_ALGO)
           cs (.build (.setProvider csb *BCProvider*) k)
           xdn (X500Principal. dnStr)
           rbr (JcaPKCS10CertificationRequestBuilder. xdn u)
           bits (.getEncoded (.build rbr cs))
           rc (if (= fmt *PEM_CERT*)
                (fmtPEM "-----BEGIN CERTIFICATE REQUEST-----\n" "\n-----END CERTIFICATE REQUEST-----\n" bits)
                bits) ]
      [ rc (export-pkey k fmt) ] )) )

;; generate self-signed cert
;; self signed-> issuer is self
(defn- mkSSV1Cert [pv kp start end dnStr keylen algo]
  (let [ dnName (X500Principal. dnStr)
         prv (.getPrivate kp)
         pub (.getPublic kp)
         bdr (JcaX509v1CertificateBuilder. dnName (next-serial) start end dnName pub)
         cs (.build (.setProvider (JcaContentSignerBuilder. algo) pv) prv)
         cert (.getCertificate (.setProvider (JcaX509CertificateConverter.) pv) (.build bdr cs)) ]
    (.checkValidity cert (Date.))
    (.verify cert pub)
    [cert prv] ))

(defn- mkSSV1 [^KeyStore ks ^KeyPair kp ^String algo ^Date start ^Date end dnStr
               ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj keylen ^File out]
  (do
    (debug "mkSSV1: dn= " dnStr ", key-len= " keylen)
    (let [ props (mkSSV1Cert (.getProvider ks) kp start end dnStr keylen algo)
           ca (.toCharArray pwdObj)
           baos (IO/make-baos) ]
      (.setKeyEntry ks (CU/uid) (nth props 1) ca (into-array Certificate [ (nth props 0) ] ))
      (.store ks baos ca)
      (FileUtils/writeByteArrayToFile out (.toByteArray baos)) )) )

(defn make-pkcs12 ^{ :doc "Make a PKCS12 object from key and cert." }
  [^bytes keyPEM ^bytes certPEM ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj ^File out]
  (let [ ct (.getTrustedCertificate (conv-cert certPEM))
         rdr (InputStreamReader. (IO/streamify keyPEM))
         ca (.toCharArray pwdObj)
         baos (IO/make-baos)
         ss (get-pkcsStore)
         kp (cast KeyPair (.readObject (PEMParser. rdr))) ]
    (.setKeyEntry ss (CU/uid) (.getPrivate kp) ca (into-array Certificate [ct]))
    (.store ss baos ca)
    (FileUtils/writeByteArrayToFile out (.toByteArray baos))))

(defn make-ssv1PKCS12  ^{ :doc "Make a SSV1 (root level) type PKCS12 object." }
  [^Date start ^Date end dnStr ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj keylen ^File out]
  (let [ ks (get-pkcsStore)
         kp (make-keypair *RSA* keylen) ]
    (mkSSV1 ks kp DEF_ALGO start end dnStr pwdObj keylen out)) )

(defn make-ssv1JKS ^{ :doc "Make a SSV1 (root level) type JKS object." }
  [^Date start ^Date end dnStr ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj keylen ^File out]
  (let [ ks (get-jksStore)
         kp (make-keypair *DSA* keylen) ]
    (mkSSV1 ks kp "SHA1withDSA" start end dnStr pwdObj keylen out)) )

(defn- mkSSV3Cert [pv ^KeyPair kp start end dnStr ^Certificate issuer ^PrivateKey issuerKey keylen algo]
  (let [ top (cast X509Certificate issuer)
         subject (X500Principal. dnStr)
         prv (.getPrivate kp)
         pub (.getPublic kp)
         bdr (JcaX509v3CertificateBuilder. top (next-serial) start end subject pub)
         exUte (JcaX509ExtensionUtils.)
         cs (.build (.setProvider (JcaContentSignerBuilder. algo) pv) issuerKey) ]
    (.addExtension bdr (X509Extension/authorityKeyIdentifier) false (.createAuthorityKeyIdentifier exUte top))
    (.addExtension bdr (X509Extension/subjectKeyIdentifier) false (.createSubjectKeyIdentifier exUte pub))
    (let [ cert (.getCertificate (.setProvider (JcaX509CertificateConverter.) pv) (.build bdr cs)) ]
      (.checkValidity cert (Date.))
      (.verify cert (.getPublicKey top))
      [cert prv] )))

(defn- mkSSV3 [ ^KeyStore ks ^KeyPair kp algo ^Date start ^Date end dnStr 
                ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj keylen issuerCerts ^PrivateKey issuerKey ^File out]
  (do
    (debug "mkSSV3: dn= " dnStr ", key-len= " keylen)
    (let [ iscs (vec issuerCerts)
           props (mkSSV3Cert (.getProvider ks) kp start end dnStr (first iscs) issuerKey keylen algo)
           ca (.toCharArray pwdObj)
           baos (IO/make-baos)
           cs (cons (nth props 0) iscs) ]
      (.setKeyEntry ks (CU/uid) (nth props 1) ca (into-array Certificate cs))
      (.store ks baos ca)
      (FileUtils/writeByteArrayToFile out (.toByteArray baos)))) )

(defn make-ssv3PKCS12 ^{ :doc "Make a SSV3 type PKCS12 object." }
  [^Date start ^Date end dnStr ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj
    keylen issuerCerts ^PrivateKey issuerKey ^File out]
  (let [ ks (get-pkcsStore)
         ;;kp (make-keypair *RSA* keylen) ]
         kp (make-keypair (.getAlgorithm issuerKey)  keylen) ]
    (mkSSV3 ks kp DEF_ALGO start end dnStr pwdObj keylen issuerCerts issuerKey out)) )

;; JKS uses SUN and hence needs to use DSA
(defn make-ssv3JKS ^{ :doc "Make a SSV3 JKS object." }
  [^Date start ^Date end dnStr ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj keylen issuerCerts issuerKey ^File out]
  (let [ ks (get-jksStore)
         ;;kp (make-keypair *DSA* keylen) ]
         kp (make-keypair (.getAlgorithm issuerKey) keylen) ]
    (mkSSV3 ks  kp "SHA1withDSA" start end dnStr pwdObj keylen issuerCerts issuerKey out)))

(defn export-pkcs7 ^{ :doc "Extract and export PKCS7 info from a PKCS12 object." }
  [^URL p12File ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj ^File fileOut]
  (let [ pkey (loadPKCS12Key p12File pwdObj)
         cc (.getCertificateChain pkey)
         k (.getPrivateKey pkey)
         cl (vec cc)
         cp (.build (.setProvider (JcaDigestCalculatorProviderBuilder.) *BCProvider*))
         bdr (JcaSignerInfoGeneratorBuilder. cp)
;;    "SHA1withRSA"
         ;;cs (.build (.setProvider (JcaContentSignerBuilder. (CMSSignedGenerator/DIGEST_SHA512)) *BCProvider*) k)
         cs (.build (.setProvider (JcaContentSignerBuilder. *SHA512*) *BCProvider*) k)
         gen (CMSSignedDataGenerator.) ]
    (.addSignerInfoGenerator gen (.build bdr cs (cast X509Certificate (first cl))))
    (.addCertificates gen (JcaCertStore. cl))
    (let [ dummy (CMSProcessableByteArray. (CU/bytesify "Hello"))
           bits (.getEncoded (.generate gen (CMSSignedGenerator/DATA)
                                        dummy false *BCProvider* false)) ]
      (FileUtils/writeByteArrayToFile fileOut bits))) )

(defn new-session  ^{ :doc "Creates a new java-mail session." }
  ([] (new-session "" nil))
  ([user ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj]
    (Session/getInstance (System/getProperties)
      (if (StringUtils/isEmpty user) nil (DefaultAuthenticator. user (SU/nsb pwdObj)) ))))

(defn new-mimeMsg ^{ :doc "Create a new MIME Message." }
  ([user ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj] (new-mimeMsg user pwdObj nil))
  ([] (new-mimeMsg "" nil nil))
  ([^InputStream inp] (new-mimeMsg "" nil inp))
  ([user ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj ^InputStream inp]
      (let [ s (new-session user pwdObj) ]
        (if (nil? inp) (MimeMessage. s) (MimeMessage. s inp)))) )

(defn is-signed?  ^{ :doc "Check if this stream-like object or message-part is signed." }
  [obj]
  (let [ inp (MM/maybe-stream obj) ]
    (if (nil? inp)
      (if (instance? Multipart obj)
        (let [ mp (cast Multipart obj) ] (MM/is-signed? (.getContentType mp)))
        (throw (IOException. (str "Invalid content: " (CU/get-classname obj)))))
      (try
        (is-signed? (.getContentType (new-mimeMsg "" "" inp)))
        (finally (IO/reset-stream! inp))))) )


(defn is-compressed?  ^{ :doc "Check if this stream-like object or message-part is compressed." }
  [obj]
  (let [ inp (MM/maybe-stream obj) ]
    (if (nil? inp)
      (cond
        (instance? Multipart obj) (MM/is-compressed? (.getContentType (cast Multipart obj)))
        (instance? BodyPart obj) (MM/is-compressed? (.getContentType (cast BodyPart obj)))
        :else (throw (IOException. (str "Invalid content: " (CU/get-classname obj)))))
      (try
        (MM/is-compressed? (.getContentType (new-mimeMsg "" "" inp)))
        (finally (IO/reset-stream! inp))))) )

(defn is-encrypted? ^{ :doc "Check if this stream-like object or message-part is encrypted." }
  [obj]
  (let [ inp (MM/maybe-stream obj) ]
    (if (nil? inp)
      (cond
        (instance? Multipart obj)(MM/is-encrypted? (.getContentType (cast Multipart obj)))
        (instance? BodyPart obj) (MM/is-encrypted? (.getContentType (cast BodyPart obj)))
        :else (throw (IOException. (str "Invalid content: " (CU/get-classname obj)))))
      (try
        (MM/is-encrypted? (.getContentType (new-mimeMsg "" "" inp)))
        (finally (IO/reset-stream! inp))))))

(defn get-charset ^{ :doc "Deduce the char-set from content-type." }
  ([cType]
    (if (SU/hgl? cType)
      (try
        (SU/nsb (MimeUtility/javaCharset (-> (ContentType. cType) (.getParameter "charset"))))
        (catch Throwable e (do (warn "" e) "")))
      ""))
  ([cType dft]
    (let [ cs (get-charset cType) ]
      (SU/nsb (if (SU/hgl? cs) cs (MimeUtility/javaCharset dft))))) )

(defn- make-signerGentor [^PrivateKey pkey certs algo]
  (let [ gen (SMIMESignedGenerator. "base64")
         lst (vec certs)
         caps (doto (SMIMECapabilityVector.)
                  (.addCapability SMIMECapability/dES_EDE3_CBC)
                  (.addCapability SMIMECapability/rC2_CBC, 128)
                  (.addCapability SMIMECapability/dES_CBC) )
         signedAttrs (doto (ASN1EncodableVector.)
                        (.add (SMIMECapabilitiesAttribute. caps)))
         x0 (cast X509Certificate (first lst))
         issuer (if (> (.size lst) 1) (nth lst 1) x0)
         issuerDN (.getSubjectX500Principal issuer)
         ;;
         ;; add an encryption key preference for encrypted responses -
         ;; normally this would be different from the signing certificate...
         ;;
         issAndSer (IssuerAndSerialNumber. (X500Name/getInstance (.getEncoded issuerDN)) (.getSerialNumber x0))
         dm1 (.add signedAttrs (SMIMEEncryptionKeyPreferenceAttribute. issAndSer))
         bdr (doto (JcaSignerInfoGeneratorBuilder.
                          (-> (JcaDigestCalculatorProviderBuilder.) (.setProvider *BCProvider*) (.build)) )
                (.setDirectSignature true))
         cs (-> (JcaContentSignerBuilder. (SU/nsb algo)) (.setProvider *BCProvider*) (.build pkey)) ]

    (.setSignedAttributeGenerator bdr (DefaultSignedAttributeTableGenerator. (AttributeTable. signedAttrs)))
    (.addSignerInfoGenerator gen (.build bdr cs, x0))
    (.addCertificates gen (JcaCertStore. lst))
    gen))

(defmulti ^{ :doc "Generates a MimeMultipart." } smime-digsig
  (fn [a b c d]
    (cond
      (instance? MimeMessage d) :mimemessage
      (instance? Multipart d) :multipart
      (instance? BodyPart d) :bodypart
      :else (throw (IllegalArgumentException. "wrong type")))))

(defmethod smime-digsig :mimemessage [^PrivateKey pkey certs algo ^MimeMessage mmsg]
  (do
    ;; force internal processing, just in case
    (.getContent mmsg)
    (-> (make-signerGentor pkey certs algo) (.generate mmsg *BCProvider*))) )

(defmethod smime-digsig :multipart [^PrivateKey pkey certs algo ^Multipart mp]
  (let [ mm (new-mimeMsg) ]
    (.setContent mm mp)
    (-> (make-signerGentor pkey certs algo) (.generate mm *BCProvider*))) )

(defmethod smime-digsig :bodypart [^PrivateKey pkey certs algo ^BodyPart bp]
  (-> (make-signerGentor pkey certs algo) (.generate (cast MimeBodyPart bp) *BCProvider*)))

(defn- smime-dec [^PrivateKey pkey env]
    ;;var  recId = new JceKeyTransRecipientId(cert.asInstanceOf[XCert])
  (let [ rec (-> (JceKeyTransEnvelopedRecipient. pkey) (.setProvider *BCProvider*))
         it (-> (.getRecipientInfos env) (.getRecipients) (.iterator)) ]
    (loop [ rc nil ]
      (if (or (not (nil? rc)) (not (.hasNext it)))
        rc
        (recur (.getContentStream (cast RecipientInformation (.next it)) rec))))) )

(defmulti ^{ :doc "SMIME decrypt this object." } smime-decrypt
  (fn [a b]
    (cond
      (instance? MimeMessage b) :mimemsg
      (instance? BodyPart b) :bodypart
      :else (throw (IllegalArgumentException. "wrong type")))))

(defn- smime-loopdec [pkeys ev]
  (let [ rc (some (fn [ k & _ ]
                    (let [ cms (smime-dec k ev) ]
                      (if (nil? cms) false (IOUtils/toByteArray (.getContentStream cms)))))
              pkeys) ]
    (when (nil? rc)
      (throw (GeneralSecurityException. "No matching decryption key")))
    rc))

(defmethod smime-decrypt :mimemsg [pkeys ^MimeMessage mimemsg]
  (smime-loopdec pkeys (SMIMEEnveloped. mimemsg)))

(defmethod smime-decrypt :bodypart [pkeys ^BodyPart part]
  (smime-loopdec pkeys (SMIMEEnveloped. (cast MimeBodyPart part))))

(defn peeksmime-signedContent ^{ :doc "Get the content ignoring the signing stuff." }
  [^Multipart mp]
  (-> (SMIMESignedParser. (BcDigestCalculatorProvider.)
      (cast MimeMultipart mp)
      (get-charset (.getContentType mp) "binary")) (.getContent) (.getContent)) )

(defn test-smimeDigSig ^{ :doc "Verify the signature and return content if ok." }
  ([^Multipart mp certs] (test-smimeDigSig mp certs ""))
  ([^Multipart mp certs ^String cte]
    (let [ mmp (cast MimeMultipart mp)
           sc (if (SU/hgl? cte) (SMIMESigned. mmp cte) (SMIMESigned. mmp))
           s (JcaCertStore. (vec certs))
           sns (-> (.getSignerInfos sc) (.getSigners) )
           rc (some (fn [i]
                  (let [ si (cast SignerInformation i)
                         c (.getMatches s (.getSID si))
                         it (.iterator c)
                         rc (loop [ ret [] stop false]
                              (if (or stop (not (.hasNext it)))
                                ret
                                (let [ bdr (-> (JcaSimpleSignerInfoVerifierBuilder.)
                                             (.setProvider *BCProvider*)) ]
                                  (if (.verify si (.build bdr (cast X509CertificateHolder (.next it))))
                                    (let [ digest (.getContentDigest si) ]
                                      (if (nil? digest)
                                        (recur ret false)
                                        (recur [sc digest] true )))
                                    (recur ret false))))) ]
                    (if (empty? rc) nil rc))
                  ) (seq sns)) ]
      (when (empty? rc)
        (throw (GeneralSecurityException. "Failed to verify signature: no matching cert.")) )
      [ (-> (.getContentAsMimeMessage (first rc) (new-session)) (.getContent)) (nth rc 1) ] )))


(defmulti ^{ :doc "Inflate the compressed content." } smime-decompress
  (fn [a]
    (cond
      (instance? InputStream a) :stream
      (instance? BodyPart a) :bodypart
      :else (throw (IllegalArgumentException. "wrong type")))))

(defmethod smime-decompress :bodypart [^BodyPart bp]
  (smime-decompress (if (nil? bp) nil (.getInputStream bp))))

(defmethod smime-decompress :stream [^InputStream inp]
  (if (nil? inp)
    (XData.)
    (let [ cms (-> (CMSCompressedDataParser. inp) (.getContent (ZlibExpanderProvider.))) ]
      (when (nil? cms) (throw (GeneralSecurityException. "Failed to decompress stream: corrupted content")))
      (XData. (IOUtils/toByteArray (.getContentStream cms))))))

(defmulti ^{ :doc "Generates a MimeBodyPart." } smime-encrypt
  (fn [a b c]
    (cond
      (instance? MimeMessage c) :mimemsg
      (instance? Multipart c) :multipart
      (instance? BodyPart c) :bodypart
      :else (throw (IllegalArgumentException. "wrong type")))))

(defmethod smime-encrypt :bodypart [^Certificate cert ^String algo ^BodyPart bp]
  (let [ gen (SMIMEEnvelopedGenerator.)
         mb (cast MimeBodyPart bp)
         xc (cast X509Certificate cert) ]
    (.addRecipientInfoGenerator gen
        (-> (JceKeyTransRecipientInfoGenerator. xc) (.setProvider *BCProvider*)))
    (.generate gen mb
        (-> (JceCMSContentEncryptorBuilder. algo) (.setProvider *BCProvider*) (.build)))))

(defmethod smime-encrypt :mimemsg [^Certificate cert ^String algo ^MimeMessage msg]
  (let [ gen (SMIMEEnvelopedGenerator.)
         xc (cast X509Certificate cert)
         g (-> (JceKeyTransRecipientInfoGenerator. xc) (.setProvider *BCProvider*)) ]
    ;; force message to be processed, just in case.
    (.getContent msg)
    (.addRecipientInfoGenerator gen g)
    (.generate gen msg
      (-> (JceCMSContentEncryptorBuilder. algo) (.setProvider *BCProvider*) (.build)))))

(defmethod smime-encrypt :multipart [^Certificate cert String algo ^Multipart mp]
  (let [ gen (SMIMEEnvelopedGenerator.)
         xc (cast X509Certificate cert)
         mm (doto (new-mimeMsg) (.setContent mp))
         g (-> (JceKeyTransRecipientInfoGenerator. xc) (.setProvider *BCProvider*)) ]
    (.addRecipientInfoGenerator gen g)
    (.generate gen mm
      (-> (JceCMSContentEncryptorBuilder. algo) (.setProvider *BCProvider*) (.build)))))

(defn smime-compress ^{ :doc "Generates a MimeBodyPart." }
  ([cType ^XData xdata]
    (let [ gen (SMIMECompressedGenerator.) bp (MimeBodyPart.)
           ds (if (.isDiskFile xdata)
                (SDataSource. (.fileRef xdata) cType)
                (SDataSource. (.javaBytes xdata) cType)) ]
      (.setDataHandler bp (DataHandler. ds))
      (.generate gen bp (SMIMECompressedGenerator/ZLIB))))

  ([^MimeMessage msg]
   (do 
    (.getContent msg) ;; make sure it's processed, just in case
    (-> (SMIMECompressedGenerator.) (.generate msg (SMIMECompressedGenerator/ZLIB)))) )

  ([cType contentLoc cid ^XData xdata]
    (let [ gen (SMIMECompressedGenerator.)
           bp (MimeBodyPart.)
           ds (if (.isDiskFile xdata) (SDataSource. (.fileRef xdata) cType)
                    (SDataSource. (.javaBytes xdata) cType)) ]
      (when (SU/hgl? contentLoc) (.setHeader bp "content-location" contentLoc))
      (when (SU/hgl? cid) (.setHeader bp "content-id" cid))
      (.setDataHandler bp (DataHandler. ds))
      (let [ zbp (.generate gen bp SMIMECompressedGenerator/ZLIB)
             pos (.lastIndexOf cid (int \>))
             cID (if (>= pos 0) (str (.substring cid 0 pos) "--z>") (str cid "--z")) ]
        (when (SU/hgl? contentLoc) (.setHeader zbp "content-location" contentLoc))
        (.setHeader zbp "content-id" cID)
        ;; always base64
        ;;cte="base64"
        (.setHeader zbp "content-transfer-encoding" "base64")
        zbp))) )

(defn pkcs-digsig ^{ :doc "SMIME sign some data." }
  [^PrivateKey pkey certs algo ^XData xdata]
    (let [ gen (CMSSignedDataGenerator.)
           cl (vec certs)
           cert (cast X509Certificate (first cl))
           cs (-> (JcaContentSignerBuilder. (SU/nsb algo)) (.setProvider *BCProvider*) (.build pkey))
           bdr (JcaSignerInfoGeneratorBuilder.
                  (-> (JcaDigestCalculatorProviderBuilder.) (.setProvider *BCProvider*) (.build))) ]
      (.setDirectSignature bdr true)
      (.addSignerInfoGenerator gen (.build bdr cs cert))
      (.addCertificates gen (JcaCertStore. cl))
      (let [ cms (if (.isDiskFile xdata) (CMSProcessableFile. (.fileRef xdata))
                      (CMSProcessableByteArray. (.javaBytes xdata))) ]
        (.getEncoded (.generate gen cms false) ))) )

(defn test-pkcsDigSig ^{ :doc "Verify the signed object with the signature." }
  [^Certificate cert ^XData xdata ^bytes signature]
    (let [ cproc (if (.isDiskFile xdata) (CMSProcessableFile. (.fileRef xdata))
                   (CMSProcessableByteArray. (.javaBytes xdata)))
           cms (CMSSignedData. cproc signature)
           s (JcaCertStore. [cert])
           sls (-> cms (.getSignerInfos) (.getSigners))
           rc (some (fn [i]
                       (let [ si (cast SignerInformation i)
                              c (.getMatches s (.getSID si))
                              it (.iterator c) ]
                         (loop [ digest nil stop false ]
                           (if (or stop (not (.hasNext it)))
                             digest
                             (let [ bdr (-> (JcaSimpleSignerInfoVerifierBuilder.) (.setProvider *BCProvider*))
                                    ok (.verify si (.build bdr (cast X509CertificateHolder (.next it))))
                                    dg (if ok (.getContentDigest si) nil) ]
                               (if (not (nil? dg)) (recur dg true) (recur nil false)))))))
                      (seq sls)) ]
      (when (nil? rc) (throw (GeneralSecurityException. "Failed to decode signature: no matching cert.")))
      rc))

(defn- str-signingAlgo [algo]
  (case algo
    "SHA-512" (SMIMESignedGenerator/DIGEST_SHA512)
    "SHA-1" (SMIMESignedGenerator/DIGEST_SHA1)
    "MD5" (SMIMESignedGenerator/DIGEST_MD5)
    (throw (IllegalArgumentException. (str "Unsupported signing algo: " algo)))))

(defn- finger-print [^bytes data algo]
  (let [ md (MessageDigest/getInstance (SU/nsb algo))
         ret (StringBuilder. (int 256))
         hv (.digest md data)
         tail (dec (alength hv)) ]
    (loop [ i 0 ]
      (if (>= i (alength hv))
        (.toString ret)
        (let [ n (.toUpperCase (Integer/toString (bit-and (aget hv i) 0xff) 16)) ]
          (-> ret (.append (if (= (.length n) 1) (str "0" n) n)) 
                (.append (if (= i tail) "" ":")))
          (recur (inc i)))))))


(defn fingerprint-sha1 ^{ :doc "Generate a fingerprint using SHA-1." }
  [^bytes data]
  (finger-print data *SHA_1*))

(defn fingerprint-md5 ^{ :doc "Generate a fingerprint using MD5." }
  [^bytes data]
  (finger-print data *MD_5*))

(defrecord CertDesc [ ^X500Principal subj ^X500Principal issuer ^Date notBefore ^Date notAfter ])

(defn desc-certificate ^{ :doc "Get some basic info from Certificate." }
  [^X509Certificate x509]
    (if (nil? x509)
      (->CertDesc nil nil nil nil)
      (->CertDesc (.getSubjectX500Principal x509) (.getIssuerX500Principal x509) (.getNotBefore x509) (.getNotAfter x509))))

(defn desc-cert ^{ :doc "Return a object" }

  ([^bytes privateKeyBits ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj]
    (let [ pkey (conv-pkey privateKeyBits pwdObj) ]
      (if (nil? pkey)
        (->CertDesc nil nil nil nil)
        (desc-certificate (.getCertificate pkey)) )))

  ([^bytes certBits]
    (let [ cert (conv-cert certBits) ]
      (if (nil? cert)
        (->CertDesc nil nil nil nil)
        (desc-certificate (.getTrustedCertificate cert))))) )

(defn valid-certificate? ^{ :doc "Validate this Certificate." }
  [^Certificate x509]
  (try
    (.checkValidity x509 (Date.))
    (catch Throwable e false)))

(defn valid-pkey?  ^{ :doc "Validate this Private Key." }
  [^bytes keyBits ^comzotohcljc.crypto.cryptors.PasswordAPI pwdObj]
    (let [ pkey (conv-pkey keyBits pwdObj) ]
      (if (nil? pkey)
        false
        (valid-certificate? (.getCertificate pkey)))) )

(defn valid-cert? ^{ :doc "Validate this Certificate." }
  [^bytes certBits]
  (let [ cert (conv-cert certBits) ]
    (if (nil? cert)
      false
      (valid-certificate? (.getTrustedCertificate cert)))) )

(defn intoarray-certs ^{ :doc "From a list of TrustedCertificateEntry(s)." }
  [certs]
  (if (empty? certs)
    []
    (map #(.getTrustedCertificate %) (seq certs))))

(defn intoarray-pkeys ^{ :doc "From a list of PrivateKeyEntry(s)." }
  [pkeys]
  (if (empty? pkeys)
    []
    (map #(.getPrivateKey %) (seq pkeys))))
























(def ^:private cryptoutils-eof nil)


