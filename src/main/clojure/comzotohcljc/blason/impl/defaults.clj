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

(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.blason.impl.defaults )

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(com.zotoh.frwk.util CoreUtils))
(import '(com.zotoh.blason.core
  RegistryError ServiceError ConfigError AppClassLoader))
(import '(java.io File))

(require '[ comzotohcljc.util.coreutils :as CU ] )
(require '[ comzotohcljc.util.strutils :as SU ] )
(require '[ comzotohcljc.util.fileutils :as FU ] )

(use '[comzotohcljc.blason.core.constants])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn precondDir [d]
  (CU/test-cond (str "Directory " d " must be read-writable.") (FU/dir-readwrite? d)))

(defn precondFile [f]
  (CU/test-cond (str "File " f " must be readable.") (FU/file-read? f)))

(defn maybeDir [m kn]
  (let [ v (get m kn) ]
    (cond
      (instance? String v) (File. v)
      (instance? File v) v
      :else (throw (ConfigError. (str "No such folder for key: " kn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti comp-set-context (fn [a b] (class a)))
(defmulti comp-get-context class)

(defmulti comp-set-cache (fn [a b] (class a)))
(defmulti comp-get-cache class)

(defmulti comp-compose (fn [a rego] (class a)))
(defmulti comp-contextualize (fn [a b] (class a)))
(defmulti comp-configure (fn [a b] (class a)))
(defmulti comp-initialize class)

(defn synthesize-component [c options]
  (let [ rego (:rego options)
         ctx (:ctx options)
         props (:props options) ]
   (when-not (nil? rego) (comp-compose c rego))
   (when-not (nil? ctx) (comp-contextualize c ctx))
   (when-not (nil? props) (comp-configure c props))
   (comp-initialize c)
   c) )

(defprotocol Component
  (version [_] )
  (parent [_] )
  (id [_] ))

(defprotocol Registry
  (lookup [_ cid] )
  (has? [_ cid] )
  (reg [_ c] )
  (dereg [_ c] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype ComponentRegistry [id version parent ctxHolder cacheHolder]
  Component

  Registry

  (has? [_ cid]
    (let [ c (get @cacheHolder cid) ]
      (if (nil? c) false true)) )

  (lookup [_ cid]
    (let [ c (get @cacheHolder cid) ]
      (if (and (nil? c) (isa? ComponentRegistry parent))
        (.lookup parent cid)
        nil)) )

  (dereg [this c]
    (let [ cid (.id c) ]
      (when (has? this cid)
        (comp-set-cache this (dissoc @cacheHolder cid)))))

  (reg [this c]
    (when-not (nil? c)
      (let [ cid (.id c) ]
        (when (has? this cid)
          (throw (RegistryError. (str "Component \"" cid "\" already exists" ))))
        (comp-set-cache this (assoc @cacheHolder cid c)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-get-context :default [co] @(.ctxHolder co))

(defmethod comp-set-context :default [co x]
  (dosync (ref-set (.ctxHolder co) x)))

(defmethod comp-get-cache :default [co] @(.cacheHolder co))

(defmethod comp-set-cache :default [co x]
  (dosync (ref-set (.cacheHolder co) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-compose :default [co rego] co)

(defmethod comp-contextualize :default [co ctx]
  (do (comp-set-context co ctx) co))

(defmethod comp-configure :default [co props] co)
(defmethod comp-initialize :default [co] co)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private defaults-eof nil)

