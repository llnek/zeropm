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

(ns ^{ :doc "Functions to load and query a .ini file." :author "kenl" }
  comzotohcljc.util.win32ini
  (:import (org.apache.commons.lang3 StringUtils))
  (:import (java.net URL))
  (:import (java.io File IOException InputStreamReader
    LineNumberReader PrintStream))
  (:import (com.zotoh.frwk.util NCMap))
  (:import (java.util LinkedHashMap))
  (:require [ comzotohcljc.util.coreutils :as CU ] )
  (:require [ comzotohcljc.util.fileutils :as FU ] )
  (:require [ comzotohcljc.util.strutils :as SU ] )
  )

(defprotocol IWin32Conf
  (getSectionAsMap [this sectionName] )
  (sectionKeys [this ] )
  (dbgShow [this])
  (getString [this sectionName property] )
  (getLong [this sectionName property] )
  (getBool [this sectionName property] )
  (getDouble [this sectionName property] )
  (optString [this sectionName property] )
  (optLong [this sectionName property] )
  (optBool [this sectionName property] )
  (optDouble [this sectionName property] ) )

(defn- throwBadIni [rdr] (throw (IOException. (str "Bad ini line: " (.getLineNumber rdr)))))
(defn- throwBadKey [k] (throw (Exception. (str "No such property " k "."))))

(defn- maybeSection
  [rdr ncmap line]
  (let [ s (StringUtils/trim (StringUtils/strip line "[]")) ]
    (when (StringUtils/isEmpty s) (throwBadIni rdr))
    (if-not (.containsKey ncmap s) (.put ncmap s (LinkedHashMap.)))
    s))

(defn- maybeLine
  [rdr ncmap section line]
  (let [ kvs (.get ncmap section) ]
    (when (nil? kvs) (throwBadIni rdr))
    (let [ pos (.indexOf line (int \=))
           nm (if (> pos 0) (.trim (.substring line 0 pos)) "" ) ]
        (when (StringUtils/isEmpty nm) (throwBadIni rdr))
        (.put kvs nm (.trim (.substring line (+ pos 1)))) )) )

(defn- evalOneLine
  [rdr ncmap line curSec]
  (let [ ln (.trim line) ]
    (cond
      (or (StringUtils/isEmpty ln) (.startsWith ln "#")) curSec
      (.matches ln "^\\[.*\\]$") (maybeSection rdr ncmap ln)
      :else (do (maybeLine rdr ncmap curSec ln) curSec)
      )) )

(defn- parseIniFile
  [fUrl]
  (with-open [ inp (.openStream fUrl) ]
    (let [ rdr (LineNumberReader. (InputStreamReader. inp "utf-8")) ncmap (NCMap.) ]
    (loop [ curSec "" line (.readLine rdr)  ]
      (if (nil? line)
        ncmap
        (recur (evalOneLine rdr ncmap line curSec) (.readLine rdr) )))) ))

(defn- hasKV
  [m k]
  (if (or (nil? k) (nil? m)) nil (.containsKey m k)) )

(defn- getKV
  [cf s k err]
  (let [ mp (.getSectionAsMap cf s)
         ok (if (or (nil? mp) (nil? k)) false true ) ]
    (if (and err (not ok))
      (throwBadKey k)
      (.get mp k))) )

(deftype Win32Conf [mapOfSections] IWin32Conf
  (getSectionAsMap [this sectionName] 
    (if (nil? sectionName)
      nil
      (let [ m (.get mapOfSections sectionName) ]
        (if (nil? m) nil (into {} m)))))     
  (sectionKeys [this] (.keySet mapOfSections))
  (getString [this section property]
    (let [ v (getKV this section property true) ]
      (SU/nsb v)))
  (optString [this section property]
    (let [ v (getKV this section property false) ]
      (SU/nsb v)))
  (getLong [this section property]
    (let [ v (getKV this section property true) ]
      (CU/conv-long (SU/nsb v) 0)) )
  (optLong [this section property]
    (let [ v (getKV this section property false) ]
      (CU/conv-long (SU/nsb v) 0)) )
  (getDouble [this section property]
    (let [ v (getKV this section property true) ]
      (CU/conv-double (SU/nsb v) 0.0)) )
  (optDouble [this section property]
    (let [ v (getKV this section property false) ]
      (CU/conv-double (SU/nsb v) 0.0)) )
  (getBool [this section property]
    (let [ v (getKV this section property true) ]
      (CU/conv-bool (SU/nsb v) false)) )
  (optBool [this section property]
    (let [ v (getKV this section property false) ]
      (CU/conv-bool (SU/nsb v) false)) )
  (dbgShow [this]
    (doseq [ [k v] (seq mapOfSections) ]
      (do
        (println (str "[" k "]"))
        (doseq [ [x y] (seq v) ]
          (println (str x "=" y)))
        (println))))
)

(defmulti ^{ :doc "Parse a INI config file, returning a Win32Conf object." } parse-inifile class)

(defmethod parse-inifile String
  [^String fpath]
  (if (nil? fpath)
    nil
    (parse-inifile (File. fpath))))
  
(defmethod parse-inifile File
  [^File file]
  (if (or (nil? file) (not (FU/file-read? file)))
    nil
    (parse-inifile (.toURL (.toURI file)))))

(defmethod parse-inifile URL
  [^URL fileUrl]
  (if (nil? fileUrl) 
    nil
    (Win32Conf. (parseIniFile fileUrl))))






(def ^:private win32ini-eof nil)

