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
  comzotohcljc.blason.impl.kernel )

(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(com.zotoh.blason.core AppClassLoader))
(import '(java.io File))


(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.blason.core.constants])
(use '[comzotohcljc.blason.impl.defaults])
(use '[comzotohcljc.blason.impl.container])

(require '[ comzotohcljc.util.coreutils :as CU ] )
(require '[ comzotohcljc.util.strutils :as SU ] )
(require '[ comzotohcljc.util.procutils :as PU ] )
(require '[ comzotohcljc.util.mimeutils :as MI ] )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol KernelAPI
  (start [_] )
  (stop [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- maybe-start-pod [knl pod]
  (try
    (let [ ctx (comp-get-context knl)
           root (get ctx K_COMPS)
           apps (.lookup root K_APPS)
           cid (.id pod)
           ctr (make-container pod) ]
      (if (not (nil? ctr))
        (do
          (.reg apps ctr)
        ;;_jmx.register(ctr,"", c.name)
          )
        (info "kernel: container " cid " disabled.")) )
    (catch Throwable e# (error e#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Kernel [id version parent ctxHolder cacheHolder] Component KernelAPI

  (start [this]
    (let [ ctx (comp-get-context this)
           root (get ctx K_COMPS)
           apps (.lookup root K_APPS)
           cs (comp-get-cache apps) ]
      ;; need this to prevent deadlocks amongst pods
      ;; when there are dependencies
      ;; TODO: need to handle this better
      (doseq [ [k v] (seq cs) ]
        (let [ r (-> (CU/new-random) (.nextLong 6)) ]
          (maybe-start-pod this v)
          (PU/safe-wait (* 1000 (Math/max 1 r)))))))

  (stop [this]
    (let [ cs (comp-get-cache this) ]
      (doseq [ [k v] (seq cs) ]
        (.stop v))
      (comp-set-cache this {} )))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype PODMeta [id version parent podType srcUrl] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-initialize PODMeta [co]
  (let [ ctx (comp-get-context co)
         rcl (get ctx K_ROOT_CZLR)
         cl  (AppClassLoader. rcl) ]
    (.configure cl (CU/nice-fpath (File. (.toURI (.srcUrl co)))) )
    (comp-set-context co (assoc ctx K_APP_CZLR cl))) )

(defmethod comp-compose Kernel [co rego]
  ;; get the jmx server from root
  co)

(defmethod comp-contextualize Kernel [co ctx]
  (let [ base (maybeDir ctx K_BASEDIR) ]
    (precondDir base)
    (precondDir (maybeDir ctx K_PODSDIR))
    (precondDir (maybeDir ctx K_PLAYDIR))
    (MI/setup-cache (-> (File. base (str DN_CFG "/app/mime.properties"))
                      (.toURI)(.toURL )))
    (comp-set-context co ctx)))


(defn make-kernel ^{ :doc "" }
  []
  (Kernel. K_KERNEL "1.0" nil (ref {}) (ref {})))






















(def ^:private kernel-eof nil)

