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

(ns ^{ :doc "One way to generate an unique id." :author "kenl" }
  com.zotoh.frwk.guid
  (:import (java.net InetAddress) )
  (:import (java.lang StringBuilder) )
  (:import (java.lang Math) )
  (:require [ com.zotoh.frwk.coreutils :as CU ] )
  (:require [ com.zotoh.frwk.strutils :as SU ] )
  (:require [ com.zotoh.frwk.byteutils :as BU ] )
  (:require [ com.zotoh.frwk.seqnumgen :as SQ ] )
  )

(declare splitHiLoTime)
(declare fmtXXX)
(declare fmt)

(def ^:private  _CHARS (.toCharArray "0123456789AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz"))
(def ^:private  _UUIDLEN 36)

(defn- maybeSetIP []
  (try
    (let [ neta (InetAddress/getLocalHost) b (.getAddress neta) ]
      (if (.isLoopbackAddress neta )
        (.nextLong (CU/newRandom))
        (if (= 4 (alength b)) (long (BU/readInt b)) (BU/readLong b) )
        ))
    (catch Throwable e (.printStackTrace e))) )

(def ^:private _IP (Math/abs (maybeSetIP)) )


(defn newUUID
  "rfc4122, version 4 form."
  []
  ;; At i==19 set the high bits of clock sequence as per rfc4122, sec. 4.1.5
  (let [ rc (char-array _UUIDLEN) rnd (CU/newRandom) ]
    (dotimes [ n (alength rc) ]
      (aset-char rc n (case n
        (8 13 18 23) \-
        (14) \4
        (let [ r (bit-or 0 (.intValue (* (.nextDouble rnd) 16)) )
               pos (if (= n 19) (bit-or (bit-and r 0x3) 0x8) (bit-and r 0xf) ) ]
          (aget _CHARS pos))) ))
    (String. rc)))

(defn newWWID
  "Return a new guid based on time and ip-address."
  []
  (let [ seed (.nextInt (CU/newRandom) (Integer/MAX_VALUE))
         ts (splitHiLoTime) ]
      (str (nth ts 0) (fmtXXX _IP) (fmtXXX seed) (fmtXXX (SQ/nextInt)) (nth ts 1)) ))

(defmulti ^:private fmtXXX  class )

(defmethod ^:private fmtXXX Long
  [num]
    (fmt "0000000000000000"  (Long/toHexString num)) )

(defmethod ^:private  fmtXXX Integer
  [num]
    (fmt "00000000"  (Integer/toHexString num)) )

(defn- fmt
  [pad mask]
  (let [ mlen (.length mask) plen (.length pad) ]
    (if (>= mlen plen)
      (.substring mask 0 plen)
      (.toString (.replace (StringBuilder. pad) (- plen mlen) plen mask ) ))) )

(defn- splitHiLoTime
  []
  (let [ s (fmtXXX (CU/nowMillis))
         n (.length s) ]
    [ (SU/left s (/ n 2)) (SU/right s (max 0 (- n (/ n 2 )) )) ] ))




(def ^:private guid-eof  nil)

