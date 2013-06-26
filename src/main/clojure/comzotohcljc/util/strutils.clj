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

(ns ^{ :doc "String utilities."
       :author "kenl" }
  comzotohcljc.util.strutils
  (:import (org.apache.commons.lang3 StringUtils))
  (:import (java.io CharArrayWriter File
    OutputStream OutputStreamWriter
    Reader Writer))
  (:import (java.util Arrays Collection
    Iterator StringTokenizer))
  (:import (java.lang StringBuilder))
  )

(defn has? ^{ :doc "Returns true if this character is inside this string." }
  [aStr ch]
  (do
    (>= (.indexOf aStr (int ch)) 0)))

(defn nsb ^{ :doc "Returns empty string if obj is null, or obj.toString." }
  [obj]
  (if (nil? obj) "" (.toString obj)))

(defn nsn ^{ :doc "Returns \"(null)\" if obj is null, or obj.toString." }
  [obj]
  (if (nil? obj) "(null)" (.toString obj)))

(defn same? ^{ :doc "Returns true if these 2 strings are the same." }
  [a b]
  (cond
    (and (nil? a) (nil? b)) true
    (or (nil? a) (nil? b)) false
    (not= (.length a) (.length b)) false
    :else (Arrays/equals (.toCharArray a) (.toCharArray b)) ) )

(defn hgl? ^{ :doc "Returns true if this string is not empty." }
  [s]
  (if (nil? s) false (> (.length s) 0)))

(defn strim ^{ :doc "Safely trim this string - handles null." }
  [s]
  (if (nil? s) "" (.trim s)))

(defn add-delim! ^{ :doc "Append to a string-builder, optionally inserting a delimiter if the buffer is not empty." }
  [buf delim item]
  (do
    (when-not (nil? item)
      (when (and (> (.length buf) 0) (not (nil? delim)))
        (.append buf delim))
      (.append buf item))
    buf))

(defn splunk ^{ :doc "Split a large string into chucks, each chunk having a specific length." }
  [largeString chunkLength]
  (if (nil? largeString)
    []
    (loop [ ret [] src largeString ]
      (if (<= (.length src) chunkLength)
        (if (> (.length src) 0) (conj ret src) ret)
        (recur (conj ret (.substring src 0 chunkLength)) (.substring src chunkLength)) ))))

(defn- lcs [s] (.toLowerCase s))
(defn- ucs [s] (.toUpperCase s))

(defn hasic-any? ^{ :doc "Tests String.indexOf() against a list of possible args. (ignoring case)." }
  [src substrs]
  (if (nil? src)
    false
    (some #(>= (.indexOf (lcs src) (lcs %)) 0) substrs)))

(defn has-any? ^{ :doc "Returns true if src contains one of these substrings." }
  [src substrs]
  (if (nil? src)
    false
    (some #(>= (.indexOf src %) 0) substrs)))

(defn swic-any? ^{ :doc "Tests startsWith (ignore-case)." }
  [src pfxs]
  (if (nil? src)
    false
    (some #(.startsWith (lcs src) (lcs %)) pfxs)))

(defn sw-any? ^{ :doc "Tests startWith(), looping through the list of possible prefixes." }
  [src pfxs]
  (if (nil? src)
    false
    (some #(.startsWith src %) pfxs)) )

(defn eqic-any? ^{ :doc "Tests String.equals() against a list of possible args. (ignore-case)." }
  [src strs]
  (if (nil? src)
    false
    (some #(.equalsIgnoreCase src %) strs)) )

(defn eq-any? ^{ :doc "Tests String.equals() against a list of possible args." }
  [src strs]
  (if (nil? src)
    false
    (some #(.equals src %) strs)))

(defn make-string ^{ :doc "Make a string of contain length." }
  [ch times]
  (let [ buf (StringBuilder.) ]
    (dotimes [ n times ]
      (.append buf ch))
    (.toString buf)) )

(defn right ^{ :doc "Gets the rightmost len characters of a String." }
  [src len]
  (if (nil? src)
    ""
    (StringUtils/right src len)) )

(defn left ^{ :doc "Gets the leftmost len characters of a String." }
  [src len]
  (if (nil? src)
    ""
    (StringUtils/left src len)) )




(def ^:private strutils-eof nil)

