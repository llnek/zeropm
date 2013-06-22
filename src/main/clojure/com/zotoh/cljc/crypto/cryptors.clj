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
  com.zotoh.cljc.crypto.cryptors
  (:use [clojure.tools.logging :only (info warn error debug)])  
  (:require [clojure.math.numeric-tower :as math])  
  (:import (org.apache.commons.codec.binary Base64))
  (:import (javax.crypto.spec SecretKeySpec))
  (:import (org.jasypt.encryption.pbe StandardPBEStringEncryptor))
  (:import (org.jasypt.util.text StrongTextEncryptor))
  (:import (java.io ByteArrayOutputStream))
  (:import (java.security SecureRandom))
  (:import (javax.crypto Cipher))
  (:import (org.bouncycastle.crypto.params DESedeParameters KeyParameter))
  (:import (org.bouncycastle.crypto.paddings PaddedBufferedBlockCipher))
  (:import (org.bouncycastle.crypto KeyGenerationParameters))
  (:import (org.bouncycastle.crypto.engines DESedeEngine))
  (:import (org.bouncycastle.crypto.generators DESedeKeyGenerator))
  (:import (org.bouncycastle.crypto.modes CBCBlockCipher))
  (:import (org.apache.commons.lang3 StringUtils))
  (:require [ com.zotoh.cljc.util.coreutils :as CU])
  (:require [ com.zotoh.cljc.util.ioutils :as IO])
  (:require [ com.zotoh.cljc.util.strutils :as SU])
  )

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

(defn- ensure-key-size [keystr algo]
  (let [ len (alength (CU/bytesify keystr)) ]
    (when (and (= T3_DES algo) (< len 24))
      (CU/throw-badarg "Encryption key length must be 24, when using TripleDES"))
    keystr))

(def ^:private DFTKEY (ensure-key-size C_KEY C_ALGO))
(def ^:private ALPHA_CHS 26)

(defn- keyAsBits [pwd algo]
  (let [ bits (CU/bytesify pwd) ]
    (if (and (= T3_DES algo) (> (alength bits) 24))
      (into-array Byte/TYPE (take 24 bits)) ;; only 24 bits wanted
      bits)))

(defprotocol BaseCryptor
  (decrypt [ this pwdStr cipherText] [ this cipherText] )
  (encrypt [ this pwdStr clearText] [ this clearText] )
  (algo [this] )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; caesar cipher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- shiftRight [width delta c head tail]
  (let [ ch  (+ c delta) ]
    (if (> ch tail)
      (char (- ch width))
      (char ch))) )

(defn- shiftLeft [width delta c head tail]
  (let [ ch (- c delta) ]
    (if (< ch head)
      (char (+ ch width))
      (char ch))) )

(defn- shift-enc [shiftpos delta ch headch tailch]
  (if (> shiftpos 0)
    (shiftRight ALPHA_CHS delta (int ch) (int headch) (int tailch))
    (shiftLeft ALPHA_CHS delta (int ch) (int headch) (int tailch))) )

(defn- shift-dec [shiftpos delta ch headch tailch]
  (if (< shiftpos 0)
    (shiftRight ALPHA_CHS delta (int ch) (int headch)  (int tailch))
    (shiftLeft ALPHA_CHS delta  (int ch)  (int headch)  (int tailch)) ))

(defn caesarEncode ^{ :doc "" }
  [text shiftpos]
  (if (or (= shiftpos 0) (StringUtils/isEmpty text))
    text
    (let [ delta (mod (math/abs shiftpos) ALPHA_CHS)
           ca (.toCharArray text)
           ;;out = java.util.Arrays.copyOf(ca, ca.length)
           ;;out (.clone ca)
           out (amap ^chars ca pos ret
                  (let [ ch (aget ^chars ca pos) ]
                    (cond
                      (and (>= (int ch) (int \A)) (<= (int ch) (int \Z))) (shift-enc shiftpos delta ch \A \Z)
                      (and (>= (int ch) (int \a)) (<= (int ch) (int \z))) (shift-enc shiftpos delta ch \a \z)
                      :else ch))) ]
      (String. out))) )

(defn caesarDecode ^{ :doc "" }
  [text shiftpos]
  (if (or (= shiftpos 0) (StringUtils/isEmpty text))
    text
    (let [ delta (mod (math/abs shiftpos) ALPHA_CHS)
           ca (.toCharArray text)
           out (amap ^chars ca pos ret
                  (let [ ch (aget ^chars ca pos) ]
                    (cond
                      (and (>= (int ch) (int \A)) (<= (int ch) (int \Z))) (shift-dec shiftpos delta ch \A \Z)
                      (and (>= (int ch) (int \a)) (<= (int ch) (int \z))) (shift-dec shiftpos delta ch \a \z)
                      :else ch))) ]
      (String. out))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jasypt cryptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- jaDecr [pwd text]
  (let [ ec (StrongTextEncryptor.) ]
    (.setPassword ec pwd)
    (.decrypt ec text)) )

(defn- jaEncr [pwd text]
  (let [ ec (StrongTextEncryptor.) ]
    (.setPassword ec pwd)
    (.encrypt ec text)) )

(deftype JasyptCryptor [] BaseCryptor
  (decrypt [this cipherText] (.decrypt this DFTKEY cipherText))
  (decrypt [this pwdStr cipherText]
    (do
      (jaDecr pwdStr cipherText)) )
  (encrypt [this clearText] (.encrypt this DFTKEY clearText))
  (encrypt [this pwdStr clearText]
    (do
      (jaEncr pwdStr clearText)) )
  (algo [this] "PBEWithMD5AndTripleDES") )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; java cryptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- getCipher [pwd mode algo]
  (let [ spec (SecretKeySpec. (keyAsBits pwd algo) algo)
         c (Cipher/getInstance algo) ]
    (.init c mode spec)
    c))

(defn- jcEncr [pwd text algo]
  (if (StringUtils/isEmpty text)
    text
    (let [ c (getCipher pwd (Cipher/ENCRYPT_MODE) algo )
           baos (IO/make-baos)
           p (CU/bytesify text)
           out (byte-array (max 4096 (.getOutputSize c (alength p))))
           n (.update c p 0 (alength p) out 0) ]
      (when (> n 0) (.write baos out 0 n))
      (let [ n2 (.doFinal c out 0) ]
        (when (> n2 0) (.write baos out 0 n2)))
      (Base64/encodeBase64URLSafeString (.toByteArray baos)))) )

(defn- jcDecr [pwd encoded algo]
  (if (StringUtils/isEmpty encoded)
    encoded
    (let [ c (getCipher pwd (Cipher/DECRYPT_MODE) algo )
           baos (ByteArrayOutputStream. (int 4096))
           p (Base64/decodeBase64 encoded)
           out (byte-array (max 4096 (.getOutputSize c (alength p))))
           n (.update c p 0 (alength p) out 0) ]
      (when (> n 0) (.write baos out 0 n))
      (let [ n2 (.doFinal c out 0) ]
        (when (> n2 0) (.write baos out 0 n2)))
      (CU/stringify (.toByteArray baos)))) )

(deftype JavaCryptor [] BaseCryptor
  (decrypt [this cipherText] (.decrypt this (.pkey this) cipherText))
  (decrypt [this pwdStr cipherText]
    (do
      (ensure-key-size pwdStr (.algo this))
      (jcDecr pwdStr cipherText (.algo this))) )
  (encrypt [this clearText] (.encrypt this (.pkey this) clearText))
  (encrypt [this pwdStr clearText]
    (do
      (ensure-key-size pwdStr (.algo this))
      (jcEncr pwdStr clearText (.algo this))) )
  (algo [this] T3_DES) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BC cryptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- bcDecr [pwd text algo]
  (if (StringUtils/isEmpty text)
    text
    (let [ cipher (PaddedBufferedBlockCipher. (CBCBlockCipher. (DESedeEngine.)))
           dummy (.init cipher false (KeyParameter. (keyAsBits pwd algo)))
           p (Base64/decodeBase64 text)
           out (byte-array 1024)
           baos (IO/make-baos)
           c (.processBytes cipher p 0 (alength p) out 0) ]
      (when (> c 0) (.write baos out 0 c))
      (let [ c2 (.doFinal cipher out 0) ]
        (when (> c2 0) (.write baos out 0 c2)))
      (CU/stringify (.toByteArray baos)))) )

(defn- bcEncr [pwd text algo]
  (if (StringUtils/isEmpty text)
    text
    (let [ cipher (PaddedBufferedBlockCipher. (CBCBlockCipher. (DESedeEngine.)))
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
  (decrypt [this cipherText] (.decrypt this (.pkey this) cipherText))
  (decrypt [this pwdStr cipherText]
    (do
      (ensure-key-size pwdStr (.algo this))
      (bcDecr pwdStr cipherText (.algo this))) )
  (encrypt [this clearText] (.encrypt this (.pkey this) clearText))
  (encrypt [this pwdStr clearText] 
    (do
      (ensure-key-size pwdStr (.algo this))
      (bcEncr pwdStr clearText (.algo this))) )
  (algo [this] T3_DES) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; passwords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol PasswordAPI
  (encoded [this] )
  (toCharArray [this] )
  (text [this] ) )

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

(defn createRandom "" [len] (createXXX len s_asciiChars))
(defn createStrong "" [len] (createXXX len s_pwdChars))

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

(defn createPassword ^{ :doc "Create a password object." }
  [pwdStr]
  (cond
    (StringUtils/isEmpty pwdStr) (Password. "")
    (.startsWith pwdStr PWD_PFX) (let [ cr (JasyptCryptor.) s (.decrypt cr (.substring pwdStr PWD_PFXLEN)) ] (Password. s))
    :else (Password. pwdStr)) )








(def ^:private cryptors-eof nil)



