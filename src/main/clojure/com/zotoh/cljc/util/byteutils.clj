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

(ns ^{ :doc "Utililties for handling byte[] conversions to/from numbers."
      :author "kenl" }
  com.zotoh.cljc.util.byteutils
  (:use [clojure.tools.logging :only (info warn error debug)])
  (:import (java.nio ByteBuffer CharBuffer) )
  (:import (java.nio.charset Charset) )
  (:import (java.io
    ByteArrayOutputStream ByteArrayInputStream DataOutputStream DataInputStream) )
  )

(defn toByteArray
  "Convert char[] to byte[]."
  [ chArray charSetObj ]
  (.array (.encode charSetObj (CharBuffer/wrap chArray)) ) )

(defn toCharArray
  "Convert byte[] to char[]."
  [ byteArray charSetObj ]
  (.array (.decode charSetObj (ByteBuffer/wrap byteArray)) ) )

(defn readLong
  "Return a long by scanning the byte[]."
  [ byteArray ]
  (.readLong (DataInputStream. (ByteArrayInputStream. byteArray)) ))

(defn readInt
  "Return an int by scanning the byte[]."
  [ byteArray ]
  (.readInt (DataInputStream. (ByteArrayInputStream. byteArray)) ))

(defmulti writeBytes class)
(defmethod ^{ :doc "Write this long value out as byte[]." } writeBytes Long
  [ num ]
    (with-open [ baos (ByteArrayOutputStream. (int 4096)) ]
      (let [ ds (DataOutputStream. baos ) ]
        (.writeLong ds num)
        (.flush ds ) 
        (.toByteArray baos ) )))

(defmethod ^{ :doc "Write this int value out as byte[]." } writeBytes  Integer
  [ num ]
    (with-open [ baos (ByteArrayOutputStream. (int 4096)) ]
      (let [ ds (DataOutputStream. baos ) ]
        (.writeInt ds num)
        (.flush ds )
        (.toByteArray baos ) )))




(def ^:private byteutils-eof nil)

