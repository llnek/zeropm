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

(ns ^{ :doc "Locale resources." :author "kenl" }
  com.zotoh.cljc.i18n.i18nutils
  (:import (java.util PropertyResourceBundle ResourceBundle Locale))
  (:import (org.apache.commons.lang3 StringUtils))
  (:import (java.io FileInputStream))
  (:require [ com.zotoh.cljc.util.metautils :as MU])
  (:require [ com.zotoh.cljc.util.coreutils :as CU])
  (:require [ com.zotoh.cljc.util.strutils :as SU])
  )

(defn load-resbundle ^{ :doc "Load a properties file containing localized string." }
  [aFile]
  (do
    (with-open [ inp (FileInputStream. aFile) ]
      (PropertyResourceBundle. inp))) )

(defn get-resbundle ^{ :doc "Return a resource bundle." }
  [baseName locale cl]
  (if (or (nil? baseName)(nil? locale))
    nil
    (ResourceBundle/getBundle baseName locale (MU/get-cldr cl))) )

(defn get-string ^{ :doc "Return the string value for this key, pms may contain values for positional substitutions." }
  [bundle pkey pms]
  (let [ kv (SU/nsb (.getString bundle pkey)) ]
    (if (empty? pms)
      kv
      (loop [ src kv pos 0 ]
        (if (>= pos (.size pms))
          src
          (recur (StringUtils/replace src "{}" (SU/nsb (nth pms pos)) 1) (inc pos)))))))


(def ^:private i18n-eof nil)

