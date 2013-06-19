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

(ns ^{ :doc "Util functions related to stream/io."  :author "kenl" }
  com.zotoh.frwk.ioutils
  (:require [ com.zotoh.frwk.coreutils :as CU])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream DataInputStream))
  (:import (java.io File FileInputStream FileOutputStream CharArrayWriter OutputStreamWriter))
  (:import (java.io InputStream InputStreamReader OutputStream Reader Writer))
  (:import (java.util.zip GZIPInputStream GZIPOutputStream))
  (:import (com.zotoh.frwk.io XData XStream))
  (:import (org.apache.commons.lang3 StringUtils))
  (:import (org.apache.commons.codec.binary Base64))
  (:import (org.apache.commons.io IOUtils))
  (:import (org.xml.sax InputSource))
  (:import (java.nio.charset Charset))
  )

(def ^:private HEX_CHS [ \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \A \B \C \D \E \F ])
(def ^:private SZ_10MEG (* 1024 1024 10))

(defn mkTempFile
  "Create a temp file in the temp dir."
  ([] (mkTempFile "" ""))
  ([pfx sux]
    (File/createTempFile
      (if (StringUtils/isEmpty pfx) "temp-" pfx)
      (if (StringUtils/isEmpty sux) ".dat" sux)
      (com.zotoh.frwk.io.IOUtils/workDir)) ))

(defn newTempFile
  "Create a new temp file, optionally open it for write as stream."
  ([] (newTempFile false))
  ([open]
    (let [ f (mkTempFile) ]
      (if open [ f (FileOutputStream. f) ] [ f nil ]))) )

(defn toStream
  "Wrapped these bytes in an input-stream."
  [bits]
  (if (nil? bits)
    nil
    (ByteArrayInputStream. bits)) )

(defn bytesToHexChars
  "Turn bytes into hex chars."
  [bits]
  (let [ len (* 2 (if (nil? bits) 0 (alength bits)))
         out (char-array len) ]
    (when-not (<= len 0)
      (loop [  k 0 pos 0 ]
        (if (>= pos len)
          nil
          (let [ n (bit-and (aget bits k) 0xff) ]
            (aset-char out pos (nth HEX_CHS (bit-shift-right n 4))) ;; high 4 bits
            (aset-char out (+ pos 1) (nth HEX_CHS (bit-and n 0xf))) ;; low 4 bits
            (recur (inc k) (+ 2 pos)) ))))
    out))

(defn bytesToHexString
  "Turn bytes into hex string."
  [bits]
  (if (nil? bits) nil (String. (bytesToHexChars  bits))) )

(defn gzip
  "Gzip these bytes."
  [bits]
  (let [ baos (ByteArrayOutputStream. (int 4096)) ]
    (when (nil? bits) nil)
    (with-open [ g (GZIPOutputStream. baos) ]
          (.write g bits, 0, (alength bits)))
    (.toByteArray baos)) )

(defn gunzip
  "Gunzip these bytes."
  [bits]
  (if (nil? bits)
    nil
    (IOUtils/toByteArray (GZIPInputStream. (toStream bits))) ))

(defn reset-stream!
  "Call reset on this input stream."
  [inp]
  (try
    (if-not (nil? inp)  (.reset inp))
    (catch Throwable t nil)) )

(defmulti openFile class)

(defmethod ^{ :doc "Open this file path." } openFile String
  [ ^String fp]
  (if (nil? fp) nil (XStream. (File. fp))))

(defmethod ^{ :doc "Open this file." } openFile File
  [^File f]
  (if (nil? f) nil (XStream. f)))

(defn fromGZipedB64
  "Unzip content which is base64 encoded + gziped."
  [gzb64]
  (if (nil? gzb64) nil (gunzip (Base64/decodeBase64 gzb64))) )

(defn toGZipedB64
  "Zip content and then base64 encode it."
  [bits]
  (if (nil? bits) nil (Base64/encodeBase64String (gzip bits))) )

(defn available
  "Get the available bytes in this stream."
  [inp]
  (if (nil? inp) 0 (.available inp)) )

(defn copyStreamToFile
  "Copy content from this input-stream to a temp file."
  [inp]
  (let [ t (newTempFile true) ]
    (with-open [ os (nth t 1) ]
      (IOUtils/copy inp os))
    (nth t 0)))

(defn copyCountedBytes
  "Copy x number of bytes from the source input-stream."
  [src out bytesToCopy]
  (when-not (<= bytesToCopy 0)
    (IOUtils/copyLarge src out 0 bytesToCopy)) )

(defn reset-source!
  "Reset an input source."
  [inpsrc]
  (if-not (nil? inpsrc)
    (let [ rdr (.getCharacterStream inpsrc)
           ism (.getByteStream inpsrc) ]
      (try
        (if-not (nil? ism) (.reset ism))
        (catch Throwable t nil))
      (try
        (if-not (nil? rdr) (.reset rdr))
        (catch Throwable t nil)) )) )

(defn mkFileBackedXData
  "Return a newly created XData backed up a new temp file."
  []
  (.setDeleteFile (XData. (mkTempFile)) true) )

(defn- swap-bytes
  ""
  [inp baos]
  (let [ bits (.toByteArray baos) t (newTempFile true) os (nth t 1) ]
    (.write os bits)
    (.flush os)
    (.close baos)
    t) )

(defn- swap-read-bytes
  ""
  [inp baos]
  (let [ t (swap-bytes inp baos) bits (byte-array 4096) os (nth t 1) ]
    (try
      (loop [ c (.read inp bits) ]
        (if (< c 0)
          (XData. (nth t 0))
          (if (= c 0)
            (recur (.read inp bits))
            (do (.write os bits 0 c)
                (recur (.read inp bits))))))
      (finally
        (.close os)))) )

(defn- read-bytes
  ""
  [inp lmt]
  (let [ baos (ByteArrayOutputStream. (int 10000))
         bits (byte-array 4096) ]
    (loop [ c (.read inp bits) cnt 0 ]
      (if (< c 0)
        (XData. baos)
        (if (= c 0)
          (recur (.read inp bits) cnt)
          (do ;; some data
            (.write baos bits 0 c)
            (if (> (+ c cnt) lmt)
              (swap-read-bytes inp baos)
              (recur (.read inp bits) (+ c cnt)) )))))) )

(defn- swap-read-chars [ inp wtr ]
  (let [ bits (.toCharArray wtr) t (newTempFile true) w (nth t 1) ]
    (.write w bits)
    (.flush w)
    (.close wtr)
    t))

(defn- read-chars
  ""
  [inp lmt]
  (let [ wtr (CharArrayWriter. (int 10000)) bits (char-array 4096) ]
    (loop [ c (.read inp bits) cnt 0 ]
      (if (< c 0)
        (XData. wtr)
        (if (= c 0)
          (recur (.read inp bits) cnt)
          (do
            (.write wtr bits 0 c)
            (if (> (+ c cnt) lmt)
              (swap-read-chars inp wtr)
              (recur (.read inp bits) (+ c cnt)))))))) )

(defn readBytesAsXData
  "Read bytes and return a XData."
  ([inp useFile] read-bytes(inp (if useFile 1 (com.zotoh.frwk.io.IOUtils/streamLimit))))
  ([inp] (read-bytes inp (com.zotoh.frwk.io.IOUtils/streamLimit))) )

(defn readCharsAsXData
  "Read chars and return a XData."
  ([rdr] (read-chars (com.zotoh.frwk.io.IOUtils/streamLimit)))
  ([rdr useFile] (read-chars (if useFile 1 (com.zotoh.frwk.io.IOUtils/streamLimit)))))

(defn bytesToChars
  "Convert these bytes to chars."
  ([bits] (bytesToChars (Charset/forName "utf-8")) )
  ([bits charSet]
;;    (1 to min(b.length, count)).foreach { (i) =>
;;      val b1 = b(i-1)
;;      ch(i-1) = (if (b1 < 0) { 256 + b1 } else b1 ).asInstanceOf[Char]
;;    }
  (if (nil? bits)
    nil
    (IOUtils/toCharArray (toStream bits) charSet))) )

(defn diff?
  "Tests if both streams are the same or different at byte level."
  [inp1 inp2]
  (cond
    (and (nil? inp1) (nil? inp2)) false
    (or (nil? inp1) (nil? inp2)) true
    :else (not (IOUtils/contentEquals inp1 inp2))) )


(def ^:private ioutils-eof nil)

