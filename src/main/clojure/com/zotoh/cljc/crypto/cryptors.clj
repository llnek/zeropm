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
  (:import (org.apache.commons.codec.binary Base64))
  (:import (javax.crypto.spec SecretKeySpec))
  (:import (javax.crypto Cipher))
  (:import (java.io ByteArrayOutputStream))
  (:import (org.apache.commons.lang3 StringUtils))
  (:require [ com.zotoh.cljc.util.coreutils :as CU])
  )

(def ^:private C_KEY "ed8xwl2XukYfdgR2aAddrg0lqzQjFhbs" )
(def ^:private T3_DES "TripleDES" ) ;; AES/ECB/PKCS5Padding/TripleDES
(def ^:private C_ALGO T3_DES) ;; default javax supports this
(defn- ensure-key-size "" [keystr algo]
  (let [ len (alength (.getBytes keystr "utf-8")) ]
    (when (and (= T3_DES algo) (< len 24))
      (CU/errBadArg "Encryption key length must be 24, when using TripleDES"))
    keystr))
(def ^:private DFTKEY (ensure-key-size C_KEY C_ALGO))



(defn- keyAsBits "" [pwd algo]
  (let [ bits (.getBytes pwd "utf-8") ]
    (if (and (= T3_DES algo) (> (alength bits) 24))
      (into-array Byte/TYPE (take 24 bits)) ;; only 24 bits wanted
      bits)))

(defprotocol BaseCryptor
  (decrypt [ this pwdStr cipherText] [ this cipherText] "Decrypt some text." )
  (encrypt [ this pwdStr clearText] [ this clearText] "Encrypt some text." )
  (algo [this] "Return the algorithm used." )
  (pkey [this] "Return the internal key used for crypto operations." )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; java cryptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- getCipher "" [pwd mode algo]
  (let [ spec (SecretKeySpec. (keyAsBits pwd algo) algo)
         c (Cipher/getInstance algo) ]
    (.init c mode spec)
    c))

(defn- jcEncr "" [pwd text algo]
  (if (StringUtils/isEmpty text)
    text
    (let [ c (getCipher pwd (Cipher/ENCRYPT_MODE) algo )
           baos (ByteArrayOutputStream.)
           p (.getBytes text "utf-8")
           out (byte-array (max 4096 (.getOutputSize c (alength p))))
           n (.update c p 0 (alength p) out 0) ]
      (when (> n 0) (.write baos out 0 n))
      (let [ n2 (.doFinal c out 0) ]
        (when (> n2 0) (.write baos out 0 n2)))
      (Base64/encodeBase64URLSafeString (.toByteArray baos)))) )

(defn- jcDecr "" [pwd encoded algo]
  (if (StringUtils/isEmpty encoded)
    encoded
    (let [ c (getCipher pwd (Cipher/DECRYPT_MODE) algo )
           baos (ByteArrayOutputStream.)
           p (Base64/decodeBase64 encoded)
           out (byte-array (max 4096 (.getOutputSize c (alength p))))
           n (.update c p 0 (alength p) out 0) ]
      (when (> n 0) (.write baos out 0 n))
      (let [ n2 (.doFinal c out 0) ]
        (when (> n2 0) (.write baos out 0 n2)))
      (String. (.toByteArray baos) "utf-8"))) )

(deftype JavaCryptor [] BaseCryptor
  (decrypt [this cipherText] (.decrypt this (.pkey this) cipherText))
  (decrypt [this pwdStr cipherText] 
    (do
      (ensure-key-size pwdStr (.algo this))
      jcDecr pwdStr cipherText (.algo this)))
  (encrypt [this clearText] (.encrypt this (.pkey this) clearText))
  (encrypt [this pwdStr clearText] 
    (do
      (ensure-key-size pwdStr (.algo this))
      jcEncr pwdStr clearText (.algo this)))
  (algo [this] T3_DES)
  (pkey [this] DFTKEY) )







