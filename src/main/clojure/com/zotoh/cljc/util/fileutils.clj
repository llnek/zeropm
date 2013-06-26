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

(ns ^{ :doc "General file related utilities." :author "kenl" }
  com.zotoh.cljc.util.fileutils
  (:use [clojure.tools.logging :only (info warn error debug)])  
  (:import (org.apache.commons.lang3 StringUtils))
  (:import (java.io
    File FileInputStream FileOutputStream
    InputStream OutputStream ))
  (:import (java.util ArrayList))
  (:import (org.apache.commons.io FileUtils))
  (:import (org.apache.commons.io IOUtils))
  (:import (java.util.zip ZipFile ZipEntry))
  (:import (com.zotoh.frwk.io XData))
  (:require [ com.zotoh.cljc.util.coreutils :as CU])
  )

(defn file-readwrite? ^{ :doc "Returns true if file is readable & writable." }
  [fp]
  (if (and (not (nil? fp)) (.exists fp) (.isFile fp) (.canRead fp) (.canWrite fp))
    true
    false) )

(defn file-read? ^{ :doc "Returns true if file is readable." }
  [fp]
  (if (and (not (nil? fp)) (.exists fp) (.isFile fp) (.canRead fp))
    true
    false) )

(defn dir-readwrite? ^{ :doc "Returns true if directory is readable and writable." }
  [dir]
  (if (and (not (nil? dir)) (.exists dir) (.isDirectory dir) (.canRead dir) (.canWrite dir) )
    true
    false) )

(defn dir-read? ^{ :doc "Returns true if directory is readable." }
  [dir]
  (if (and (not (nil? dir)) (.exists dir) (.isDirectory dir) (.canRead dir) )
    true
    false) )

(defn can-exec? ^{ :doc "Returns true if file or directory is executable." }
  [fp]
  (if (and (not (nil? fp)) (.exists fp) (.canExecute fp))
    true
    false) )

(defn parent-path ^{ :doc "Get the path to the parent directory." }
  [path]
  (if (StringUtils/isEmpty path)
    path
    (.getParent (File. path))) )

(defn- jiggleZipEntryName
  [en]
  (do
    (.replaceAll (.getName en) "^[\\/]+","")) )

(defn- doOneEntry [src des en]
  (let [ f (File. des (jiggleZipEntryName en) ) ]
    (if (.isDirectory en)
      (.mkdirs f)
      (do
        (.mkdirs (.getParentFile f))
        (with-open [ inp (.getInputStream src en) ]
          (with-open [ os (FileOutputStream. f) ]
            (IOUtils/copy inp os)))))))

(defn unzip ^{ :doc "Unzip contents of zip file to a target folder." }
  [^File src ^File des]
  (let [ fpz (ZipFile. src)  ents (.entries fpz) dummy (.mkdirs des) ]
    (loop [ hasMore (.hasMoreElements ents) ]
      (if (false? hasMore)
        nil
        (do
          (doOneEntry fpz des (.nextElement ents))
          (recur (.hasMoreElements ents)))))))

(defn save-file ^{ :doc "Save a file to a directory." }
  [^File dir ^String fname ^XData xdata]
  (let [ fp (File. dir fname) ]
    (FileUtils/deleteQuietly fp)
    (if (.isDiskFile xdata)
      (FileUtils/moveFile (.fileRef xdata) fp)
      (FileUtils/writeByteArrayToFile fp (.javaBytes xdata)))
    (info "saved file: " (CU/nice-fpath fp) ", " (.length fp) " (bytes) - OK.")))

(defn get-file ^{ :doc "Get a file from a directory." }
  [^File dir ^String fname]
  (let [ fp (File. dir fname) rc (XData.) ]
    (if (and (.exists fp) (.canRead fp))
      (doto rc (.setDeleteFile false)
              (.resetContent fp) )
      nil)) )









(def ^:private fileutils-eof nil)

