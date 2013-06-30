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
  comzotohcljc.crypto.cryptors)

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[clojure.math.numeric-tower :as math])
(import '(org.apache.commons.codec.binary Base64))
(import '(javax.crypto.spec SecretKeySpec))
(import '(org.jasypt.encryption.pbe StandardPBEStringEncryptor))
(import '(org.jasypt.util.text StrongTextEncryptor))
(import '(java.io ByteArrayOutputStream))
(import '(java.security SecureRandom))
(import '(javax.crypto Cipher))
(import '(org.bouncycastle.crypto.params DESedeParameters KeyParameter))
(import '(org.bouncycastle.crypto.paddings PaddedBufferedBlockCipher))
(import '(org.bouncycastle.crypto KeyGenerationParameters))
(import '(org.bouncycastle.crypto.engines DESedeEngine))
(import '(org.bouncycastle.crypto.generators DESedeKeyGenerator))
(import '(org.bouncycastle.crypto.modes CBCBlockCipher))
(import '(org.apache.commons.lang3 StringUtils))
(require '[ comzotohcljc.util.coreutils :as CU])
(require '[ comzotohcljc.util.ioutils :as IO])
(require '[ comzotohcljc.util.strutils :as SU])


(def ^:private VISCHS
  (.toCharArray (str " @N/\\Ri2}aP`(xeT4F3mt;8~%r0v:L5$+Z{'V)\"CKI_c>z.*"
       "fJEwSU7juYg<klO&1?[h9=n,yoQGsW]BMHpXb6A|D#q^_d!-")))
(def ^:private VISCHS_LEN (alength VISCHS))

;;(def ^:private PCHS "abcdefghijklmnopqrstuvqxyzABCDEFGHIJKLMNOPQRSTUVWXYZ`1234567890-_~!@#$%^&*()" )
(def ^:private PCHS "Ha$4Jep8!`g)GYkmrIRN72^cObZ%oXlSPT39qLMD&iC*UxKWhE#F5@qvV6j0f1dyBs-~tAQn(z_u" )
;;(def ^:private ACHS "abcdefghijklmnopqrstuvqxyz1234567890-_ABCDEFGHIJKLMNOPQRSTUVWXYZ" )
(def ^:private ACHS "nhJ0qrIz6FmtPCduWoS9x8vT2-KMaO7qlgApVX5_keyZDjfE13UsibYRGQ4NcLBH" )

(def ^:private s_asciiChars (.toCharArray ACHS))
(def ^:private s_pwdChars (.toCharArray PCHS))

(def ^:private PWD_PFX "CRYPT:" )
(def ^:private PWD_PFXLEN (.length PWD_PFX))

(def ^:private T3_DES "TripleDES" ) ;; AES/ECB/PKCS5Padding/TripleDES
(def ^:private C_KEY "ed8xwl2XukYfdgR2aAddrg0lqzQjFhbs" )
(def ^:private C_ALGO T3_DES) ;; default javax supports this
(def ^:private ALPHA_CHS 26)

(defn- ensure-key-size [keystr algo]
  (let [ len (alength (CU/bytesify keystr)) ]
    (when (and (= T3_DES algo) (< len 24))
      (CU/throw-badarg "Encryption key length must be 24, when using TripleDES"))
    keystr))

(defn- keyAsBits [^String pwd algo]
  (let [ bits (CU/bytesify pwd) ]
    (if (and (= T3_DES algo) (> (alength bits) 24))
      (into-array Byte/TYPE (take 24 bits)) ;; only 24 bits wanted
      bits)))

(defprotocol BaseCryptor
  (decrypt [ this pwdObj cipherText] [ this cipherText] )
  (encrypt [ this pwdObj clearText] [ this clearText] )
  (algo [this] ))

(defprotocol PasswordAPI
  (encoded [this] )
  (toCharArray [this] )
  (text [this] ) )

(declare pwdify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; caesar cipher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- identify-ch [pos] (aget VISCHS pos))

(defn- locate-ch [ch]
  (let [ idx (some (fn [i] (if (= ch (aget VISCHS i)) i nil)) (range VISCHS_LEN)) ]
    (if (nil? idx) -1 idx)))

(defn- slide-forward [delta cpos]
  (let [ ptr (+ cpos delta)
         np (if (>= ptr VISCHS_LEN) (- ptr VISCHS_LEN) ptr) ]
    (identify-ch np)))

(defn- slide-back [delta cpos]
  (let [ ptr (- cpos delta)
         np (if (< ptr 0) (+ VISCHS_LEN ptr) ptr) ]
    (identify-ch np)))

(defn- shiftenc [shiftpos delta cpos]
  (if (< shiftpos 0)
    (slide-forward delta cpos)
    (slide-back delta cpos)))

(defn- shiftdec [shiftpos delta cpos]
  (if (< shiftpos 0)
    (slide-back delta cpos))
    (slide-forward delta cpos) )

(defn caesar-encrypt ^{ :doc "Encrypt clear text by character rotation." }
  [^String text ^long shiftpos]
  (if (or (StringUtils/isEmpty text) (= shiftpos 0))
    text
    (let [ delta (mod (math/abs shiftpos) VISCHS_LEN)
           ca (.toCharArray text)
           out (amap ^chars ca pos ret
                  (let [ ch (aget ^chars ca pos)
                         p (locate-ch ch) ]
                    (if (< p 0)
                      ch
                      (shiftenc shiftpos delta p)))) ]
      (String. out))))

(defn caesar-decrypt ^{ :doc "Decrypt text which was encrypted by the caesar method." }
  [^String text ^long shiftpos]
  (if (or (StringUtils/isEmpty text) (= shiftpos 0))
    text
    (let [ delta (mod (math/abs shiftpos) VISCHS_LEN)
           ca (.toCharArray text)
           out (amap ^chars ca pos ret
                  (let [ ch (aget ^chars ca pos)
                         p (locate-ch ch) ]
                    (if (< p 0)
                      ch
                      (shiftdec shiftpos delta p)))) ]
      (String. out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jasypt cryptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- jaDecr [pwdObj text]
  (let [ ec (doto (StrongTextEncryptor.)
                  (.setPassword (.text pwdObj))) ]
    (.decrypt ec text)) )

(defn- jaEncr [pwdObj text]
  (let [ ec (doto (StrongTextEncryptor.)
                  (.setPassword (.text pwdObj))) ]
    (.encrypt ec text)) )

(deftype JasyptCryptor [] BaseCryptor
  (decrypt [this cipherText] (.decrypt this (pwdify C_KEY) cipherText))
  (decrypt [this pwdObj cipherText]
    (do
      (jaDecr pwdObj cipherText)) )
  (encrypt [this clearText] (.encrypt this (pwdify C_KEY) clearText))
  (encrypt [this pwdObj clearText]
    (do
      (jaEncr pwdObj clearText)) )
  (algo [this] "PBEWithMD5AndTripleDES") )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; java cryptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- getCipher [pwd mode algo]
  (let [ spec (SecretKeySpec. (keyAsBits pwd algo) algo)
         c (Cipher/getInstance algo) ]
    (.init c mode spec)
    c))

(defn- jcEncr [pwdObj text algo]
  (if (StringUtils/isEmpty text)
    text
    (let [ pwd (.text pwdObj) c (getCipher pwd (Cipher/ENCRYPT_MODE) algo )
           baos (IO/make-baos)
           p (CU/bytesify text)
           out (byte-array (max 4096 (.getOutputSize c (alength p))))
           n (.update c p 0 (alength p) out 0) ]
      (when (> n 0) (.write baos out 0 n))
      (let [ n2 (.doFinal c out 0) ]
        (when (> n2 0) (.write baos out 0 n2)))
      (Base64/encodeBase64URLSafeString (.toByteArray baos)))) )

(defn- jcDecr [pwdObj encoded algo]
  (if (StringUtils/isEmpty encoded)
    encoded
    (let [ pwd (.text pwdObj) c (getCipher pwd (Cipher/DECRYPT_MODE) algo )
           baos (ByteArrayOutputStream. (int 4096))
           p (Base64/decodeBase64 encoded)
           out (byte-array (max 4096 (.getOutputSize c (alength p))))
           n (.update c p 0 (alength p) out 0) ]
      (when (> n 0) (.write baos out 0 n))
      (let [ n2 (.doFinal c out 0) ]
        (when (> n2 0) (.write baos out 0 n2)))
      (CU/stringify (.toByteArray baos)))) )

(deftype JavaCryptor [] BaseCryptor
  (decrypt [this cipherText] (.decrypt this (pwdify C_KEY) cipherText))
  (decrypt [this pwdObj cipherText]
    (do
      (ensure-key-size (.text pwdObj) (.algo this))
      (jcDecr pwdObj cipherText (.algo this))) )
  (encrypt [this clearText] (.encrypt this (pwdify C_KEY) clearText))
  (encrypt [this pwdObj clearText]
    (do
      (ensure-key-size (.text pwdObj) (.algo this))
      (jcEncr pwdObj clearText (.algo this))) )
  (algo [this] T3_DES) )
;;PBEWithMD5AndDES
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BC cryptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- bcDecr [pwdObj text algo]
  (if (StringUtils/isEmpty text)
    text
    (let [ pwd (.text pwdObj) cipher (PaddedBufferedBlockCipher. (CBCBlockCipher. (DESedeEngine.)))
           dummy (.init cipher false (KeyParameter. (keyAsBits pwd algo)))
           p (Base64/decodeBase64 text)
           out (byte-array 1024)
           baos (IO/make-baos)
           c (.processBytes cipher p 0 (alength p) out 0) ]
      (when (> c 0) (.write baos out 0 c))
      (let [ c2 (.doFinal cipher out 0) ]
        (when (> c2 0) (.write baos out 0 c2)))
      (CU/stringify (.toByteArray baos)))) )

(defn- bcEncr [pwdObj text algo]
  (if (StringUtils/isEmpty text)
    text
    (let [ pwd (.text pwdObj) cipher (PaddedBufferedBlockCipher. (CBCBlockCipher. (DESedeEngine.)))
          ;; init the cipher with the key, for encryption
           dummy (.init cipher true (KeyParameter. (keyAsBits pwd algo)))
           out (byte-array 4096)
           baos (IO/make-baos)
           p (CU/bytesify text)
           c (.processBytes cipher p 0 (alength p) out 0) ]
      (when (> c 0) (.write baos out 0 c))
      (let [ c2 (.doFinal cipher out 0) ]
        (when (> c2 0) (.write baos out 0 c2)) )
      (Base64/encodeBase64String (.toByteArray baos)))) )

(deftype BouncyCryptor [] BaseCryptor
  (decrypt [this cipherText] (.decrypt this (pwdify C_KEY) cipherText))
  (decrypt [this pwdObj cipherText]
    (do
      (ensure-key-size (.text pwdObj) (.algo this))
      (bcDecr pwdObj cipherText (.algo this))) )
  (encrypt [this clearText] (.encrypt this (pwdify C_KEY) clearText))
  (encrypt [this pwdObj clearText]
    (do
      (ensure-key-size (.text pwdObj) (.algo this))
      (bcEncr pwdObj clearText (.algo this))) )
  (algo [this] T3_DES) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; passwords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- createXXX [len ^chars chArray]
  (cond
    (< len 0) nil
    (= len 0) ""
    :else (let [ r (SecureRandom/getInstance "SHA1PRNG")
                   ostr (char-array len)
                   bits (byte-array 4)
                   cl (alength chArray)
                   rc (amap ^chars ostr pos ret
                            (let [ n (mod (.nextInt r Integer/MAX_VALUE) cl) ]
                            (aget chArray n))) ]
            (String. rc))) )

(deftype Password [pwdStr]
  Object
  (equals [this obj] (and (instance? Password obj) (= (.pwdStr this) (.pwdStr obj))) )
  (hashCode [this] (.hashCode (SU/nsb pwdStr)))
  (toString [this] (.text this))
  PasswordAPI
  (toCharArray [this] (if (nil? pwdStr) (char-array 0) (.toCharArray pwdStr)))
  (encoded [this]
    (if (StringUtils/isEmpty pwdStr)
      ""
      (let [ cr (JasyptCryptor.) s (.encrypt cr pwdStr) ]
        (str PWD_PFX s))))
  (text [this] (SU/nsb pwdStr)))

(defn pwdify ^{ :doc "Create a password object." }
  [pwdStr]
  (cond
    (StringUtils/isEmpty pwdStr) (Password. "")
    (.startsWith pwdStr PWD_PFX) (let [ cr (JasyptCryptor.) s (.decrypt cr (.substring pwdStr PWD_PFXLEN)) ] (Password. s))
    :else (Password. pwdStr)) )

(defn create-random-string ^{ :doc "" }
  [len]
  (createXXX len s_asciiChars))

(defn create-strong-pwd ^{ :doc "" }
  [len]
  (pwdify (createXXX len s_pwdChars)))







(def ^:private cryptors-eof nil)



