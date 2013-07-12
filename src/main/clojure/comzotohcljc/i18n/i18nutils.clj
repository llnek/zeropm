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

(ns ^{ :doc "Locale resources."
       :author "kenl" }
  comzotohcljc.i18n.i18nutils)

(import '(java.util PropertyResourceBundle ResourceBundle Locale))
(import '(org.apache.commons.lang3 StringUtils))
(import '(java.io File FileInputStream))
(import '(java.net URL))
(require '[ comzotohcljc.util.metautils :as MU])
(require '[ comzotohcljc.util.coreutils :as CU])
(require '[ comzotohcljc.util.strutils :as SU])


(defmulti ^{ :doc "Load a properties file containing localized string." } load-resource class)

(defmethod load-resource File
  [^File aFile]
  (load-resource (-> aFile (.toURI) (.toURL))))

(defmethod load-resource URL
  [^URL url]
  (with-open [ inp (.openStream url) ]
      (PropertyResourceBundle. inp)))

(defn get-resource ^{ :doc "Return a resource bundle." }
  ([^String baseName ^Locale locale] (get-resource baseName locale nil))
  ([^String baseName ^Locale locale ^ClassLoader cl]
    (if (or (nil? baseName)(nil? locale))
      nil
      (ResourceBundle/getBundle baseName locale (MU/get-cldr cl))) ))

(defn get-string ^{ :doc "Return the string value for this key, pms may contain values for positional substitutions." }
  [^ResourceBundle bundle ^String pkey pms]
  (let [ kv (SU/nsb (.getString bundle pkey)) ]
    (if (empty? pms)
      kv
      (loop [ src kv pos 0 ]
        (if (>= pos (.size pms))
          src
          (recur (StringUtils/replace src "{}" (SU/nsb (nth pms pos)) 1) (inc pos)))))))


(def ^:private i18nutils-eof nil)

