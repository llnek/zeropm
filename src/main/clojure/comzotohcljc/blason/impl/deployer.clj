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
  comzotohcljc.blason.impl.deployer )

(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(java.io File))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.blason.impl.defaults])
(use '[comzotohcljc.blason.core.constants])

(require '[ comzotohcljc.util.coreutils :as CU ] )
(require '[ comzotohcljc.util.fileutils :as FU ] )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol DeployerAPI
  (undeploy [_ app] )
  (deploy [_ src] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Deployer [id version parent ctxHolder] Component DeployerAPI

  ;; deploy the pod to target work-dir.
  (deploy [this src]
    (let [ app (FilenameUtils/getBaseName (CU/nice-fpath src))
           ctx (comp-get-context this)
           des (File. (get ctx K_PLAYDIR) app)
           pod (File. (.toURI src)) ]
      (if (not (.exists des))
        (FU/unzip pod des)
        (info "app: " app " has already been deployed."))))

  ;; clean out the app from the work-dir.
  (undeploy [this app]
    (let [ ctx (comp-get-context this)
           dir (File. (get ctx K_PLAYDIR) app) ]
      (if (.exists dir)
        (do
          (FileUtils/deleteDirectory dir)
          (info "app: " app " has been undeployed."))
        (warn "cannot undeploy app: " app ", doesn't exist - no operation taken."))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-contextualize Deployer [this ctx]
  (do
    (precondDir (maybeDir ctx K_BASEDIR))
    (precondDir (maybeDir ctx K_PODSDIR))
    (precondDir (maybeDir ctx K_PLAYDIR))
    (comp-set-context this ctx)))

(defmethod comp-initialize Deployer [this]
  (let [ ctx (comp-get-context this)
         py (get ctx K_PLAYDIR)
         pd (get ctx K_PODSDIR)
         fs (FileUtils/listFiles pd (into-array String ["pod"]) false) ]
    (doseq [f (seq fs)]
      (.deploy this (-> f (.toURI)(.toURL))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-deployer ^{ :doc "" }
  []
  (Deployer. K_DEPLOYER "1.0" nil (ref {}) ))




























(def ^:private deployer-eof nil)

