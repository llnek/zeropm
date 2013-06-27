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

(ns ^{ :doc "This is a utility class that provides various MIME related functionality." :author "kenl" }
  comzotohcljc.util.mimeutils
  (:use [clojure.tools.logging :only (info warn error debug)])
  (:import (org.apache.commons.lang3 StringUtils))
  (:import (java.net URLDecoder URLEncoder))
  (:import (java.io InputStream File))
  (:import (java.net URL))
  (:import (java.util.regex Pattern))
  (:import (java.util Properties))
  (:import (javax.mail Message))
  (:require [comzotohcljc.util.coreutils :as CU])
  (:require [comzotohcljc.util.metautils :as MU])
  (:require [comzotohcljc.util.strutils :as SU])
  (:require [comzotohcljc.util.ioutils :as IO])
  )

(def ^:dynamic *CTE_QUOTED* "quoted-printable")
(def ^:dynamic *CTE_7BIT* "7bit")
(def ^:dynamic *CTE_8BIT* "8bit")
(def ^:dynamic *CTE_BINARY* "binary")
(def ^:dynamic *CTE_BASE64* "base64")

(def ^:dynamic *MIME_USER_PROP*  "mime.rfc2822.user")
(def ^:dynamic *MIME_USER_JAVAMAIL*   "javamail")
(def ^:dynamic *DEF_USER*  "popeye")
(def ^:dynamic *MIME_USER_PREFIX*   "zotoh")
(def ^:dynamic *DEF_HOST*  "localhost")
(def ^:dynamic *MIME_HEADER_MSGID*  "Message-ID")
(def ^:dynamic *MIME_MULTIPART_BOUNDARY*  "boundary")
(def ^:dynamic *DOT*   ".")
(def ^:dynamic *AT*  "@")
(def ^:dynamic *CH_DOT*   \. )
(def ^:dynamic *CH_AT*  \@)
(def ^:dynamic *STR_LT*   "<")
(def ^:dynamic *STR_GT*  ">")
(def ^:dynamic *ALL*   -1)
(def ^:dynamic *ALL_ASCII*   1)
(def ^:dynamic *MOSTLY_ASCII*   2)
(def ^:dynamic *MOSTLY_NONASCII*   3)

;; Capitalized MIME constants to use when generating MIME headers)
;; for messages to be transmitted.)
(def ^:dynamic *AS2_VER_ID*    "1.1")
(def ^:dynamic *UA*  "user-agent")
(def ^:dynamic *TO*   "to")
(def ^:dynamic *FROM*  "from")
(def ^:dynamic *AS2_VERSION*    "as2-version")
(def ^:dynamic *AS2_TO*   "as2-to")
(def ^:dynamic *AS2_FROM*  "as2-from")
(def ^:dynamic *SUBJECT*    "subject")
(def ^:dynamic *CONTENT_TYPE*  "content-type")
(def ^:dynamic *CONTENT*     "content")
(def ^:dynamic *CONTENT_NAME*   "content-name")
(def ^:dynamic *CONTENT_LENGTH*  "content-length")
(def ^:dynamic *CONTENT_LOC*  "content-Location")
(def ^:dynamic *CONTENT_ID*    "content-id")
(def ^:dynamic *CONTENT_TRANSFER_ENCODING*  "content-transfer-encoding")
(def ^:dynamic *CONTENT_DISPOSITION*   "content-disposition")
(def ^:dynamic *DISPOSITION_NOTIFICATION_TO*  "disposition-notification-to")
(def ^:dynamic *DISPOSITION_NOTIFICATION_OPTIONS*  "disposition-notification-options")
(def ^:dynamic *SIGNED_REC_MICALG* "signed-receipt-micalg")
(def ^:dynamic *MESSAGE_ID*   "message-id")
(def ^:dynamic *ORIGINAL_MESSAGE_ID*   "original-message-id")
(def ^:dynamic *RECEIPT_DELIVERY_OPTION*   "receipt-delivery-option")
(def ^:dynamic *DISPOSITION*  "disposition")
(def ^:dynamic *DATE*    "date")
(def ^:dynamic *MIME_VERSION*   "mime-version")
(def ^:dynamic *FINAL_RECIPIENT*   "final-recipient")
(def ^:dynamic *ORIGINAL_RECIPIENT*   "original-recipient")
(def ^:dynamic *RECV_CONTENT_MIC*   "received-content-mic")

(def ^:dynamic *RFC822* "rfc822")
(def ^:dynamic *RFC822_PFX* (str *RFC822* "; "))

(def ^:dynamic *APP_XML* "application/xml")
(def ^:dynamic *TEXT_PLAIN* "text/plain")
(def ^:dynamic *APP_OCTET* "application/octet-stream")
(def ^:dynamic *PKCS7SIG* "pkcs7-signature")
(def ^:dynamic *TEXT_HTML* "text/html")
(def ^:dynamic *TEXT_XML* "text/xml")
(def ^:dynamic *MSG_DISP* "message/disposition-notification")

(def ^:dynamic *ERROR*   "error")
(def ^:dynamic *FAILURE* "failure")
(def ^:dynamic *WARNING*  "warning")
(def ^:dynamic *HEADERS*  "headers")

(def ^:dynamic *ISO_8859_1* "iso-8859-1")
(def ^:dynamic *US_ASCII* "us-ascii")

(def ^:dynamic *CRLF* "\r\n")


(def ^:private _extRegex (Pattern/compile "^.*\\.([^.]+)$"))
(def ^:private _mime_cache (atom {}))

(defn- is-pkcs7mime? "" [s] (>= (.indexOf s "application/x-pkcs7-mime") 0))


(defn mime-cache { :doc "" }
  []
  @_mime_cache)

(defn get-charset ^{ :doc "Get charset from this content-type string." }
  [^String cType]
  (let [ pos (-> (SU/nsb cType) (.toLowerCase) (.indexOf "charset="))
       ;;rc "ISO-8859-1"
         rc "utf-8" ]
    (if (> pos 0)
      (let [ s (.substring cType (+ pos 8)) p (StringUtils/indexOfAny s "; \t\r\n") ]
        (if (> p 0) (.substring s 0 p) s))
      rc)) )

(defn is-signed? ^{ :doc "Returns true if this content-type indicates signed." }
  [^String cType]
  (let [ ct (.toLowerCase (SU/nsb cType)) ]
    (or (>= (.indexOf ct "multipart/signed") 0)
        (and (is-pkcs7mime? ct) (>= (.indexOf ct "signed-data") 0)))) )

(defn is-encrypted? ^{ :doc "Returns true if this content-type indicates encrypted." }
  [^String cType]
  (let [ ct (.toLowerCase (SU/nsb cType)) ]
    (and (is-pkcs7mime? ct) (>= (.indexOf ct "enveloped-data") 0))) )

(defn is-compressed? ^{ :doc "Returns true if this content-type indicates compressed." }
  [^String cType]
  (let [ ct (.toLowerCase (SU/nsb cType)) ]
    (and (>= (.indexOf ct "application/pkcs7-mime") 0) (>= (.indexOf ct "compressed-data") 0))) )

(defn is-mdn? ^{ :doc "Returns true if this content-type indicates MDN." }
  [^String cType]
  (let [ ct (.toLowerCase (SU/nsb cType)) ]
    (and (>= (.indexOf ct "multipart/report") 0) (>= (.indexOf ct "disposition-notification") 0))) )

(defn maybe-stream  ^{ :doc "Turn this object into some form of stream, if possible." }
  [obj]
  (cond
    (instance? String obj) (IO/streamify (CU/bytesify (cast String obj)))
    (instance? InputStream obj) (cast InputStream obj)
    (instance? (MU/bytes-class) obj) (IO/streamify obj)
    :else nil))

(defn url-decode  ^{ :doc "URL decode this string." }
  [^String u]
  (if (nil? u)
    nil
    (try
      (URLDecoder/decode u "utf-8")
      (catch Throwable e nil))))

(defn url-encode ^{ :doc "URL encode this string." }
  [^String u]
  (if (nil? u)
    nil
    (try
      (URLEncoder/encode u "utf-8")
      (catch Throwable e nil))))

(defn guess-mimetype ^{ :doc "Guess the MIME type of file." }
  ([^File file] (guess-mimetype file ""))
  ([^File file ^String dft]
    (let [ mc (.matcher _extRegex (.toLowerCase (.getName file)))
           ex (if (.matches mc) (.group mc 1) "")
           p (SU/nsb (get (mime-cache) ex)) ]
      (if (SU/hgl? p) p dft))) )

(defn guess-contenttype ^{ :doc "Guess the content-type of file." }
  ([^File file] (guess-contenttype file "utf-8" "application/octet-stream" ))
  ([^File file ^String enc ^String dft]
    (let [ mt (guess-mimetype file)
           ct (if (SU/hgl? mt) mt dft) ]
      (if (not (.startsWith ct "text/")) ct (str ct "; charset=" enc)))) )

(defn setup-cache  ^{ :doc "Load file mime-types as a map." }
  [^URL fileUrl]
  (with-open [ inp (.openStream fileUrl) ]
    (let [ ps (Properties.) ]
      (.load ps inp)
      (reset! _mime_cache (CU/into-map ps)))))



















(def ^:private mimeutils-eof nil)

