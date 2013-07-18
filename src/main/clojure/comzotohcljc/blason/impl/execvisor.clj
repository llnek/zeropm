;
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
  comzotohcljc.blason.impl.execvisor )

(import '(org.apache.commons.io.filefilter DirectoryFileFilter))
(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(java.io File FileFilter))
(import '(java.util Date))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.blason.core.constants])
(use '[comzotohcljc.blason.impl.defaults])
(use '[comzotohcljc.blason.impl.deployer])
(use '[comzotohcljc.blason.impl.kernel :only (make-kernel)   ])

(require '[ comzotohcljc.util.coreutils :as CU ] )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private START-TIME (.getTime (Date.)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ExecvisorAPI
  (start [_] )
  (stop [_] )
  (homeDir [_] )
  (confDir [_] )
  (podsDir [_] )
  (playDir [_] )
  (logDir [_] )
  (tmpDir [_] )
  (dbDir [_] )
  (blocksDir [_] )
  (getStartTime [_] )
  (kill9 [_] )
  (getUpTimeInMillis [_] ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- chkManifest [ctx app des mf]
  (let [ root (get ctx K_COMPS)
         apps (.lookup root :rego/apps)
         ps (CU/load-javaprops mf)
         ver (.getProperty ps "Implementation-Version" "")
         cz (.getProperty ps "Main-Class" "") ]

    (CU/test-nestr "POD-MainClass" cz)
    (CU/test-nestr "POD-Version" ver)

    ;;ps.gets("Manifest-Version")
    ;;.gets("Implementation-Title")
    ;;.gets("Implementation-Vendor-URL")
    ;;.gets("Implementation-Vendor")
    ;;.gets("Implementation-Vendor-Id")

    (.reg apps
      (-> (comzotohcljc.blason.impl.kernel.PODMeta. app ver nil cz (-> des (.toURI) (.toURL)))
        (synthesize-component { :ctx ctx }))) ))


(defn- inspect-pod [ctx des]
  (let [ app (FilenameUtils/getBaseName (CU/nice-fpath des))
         mf (File. des MN_FILE) ]
    (try
        (precondDir (File. des POD_INF))
        (precondDir (File. des POD_CLASSES))
        (precondDir (File. des POD_LIB))
        (precondDir (File. des META_INF))
        (precondFile (File. des CFG_APP_CF))
        (precondFile (File. des CFG_ENV_CF))
        (precondDir (File. des DN_CONF))
        (precondFile mf)
        (chkManifest ctx app des mf)
      (catch Throwable e#
        (error e#)))) )

(defn- inspect-pods [co]
  (let [ ctx (comp-get-context co)
         fs (-> (get ctx K_PLAYDIR)
                (.listFiles (cast FileFilter DirectoryFileFilter/DIRECTORY))) ]
    (doseq [ f (seq fs) ]
      (inspect-pod ctx f)) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Execvisor [id version parent ctxHolder] Component ExecvisorAPI

  (getStartTime [_] START-TIME)

  (getUpTimeInMillis [_]
    (- (System/currentTimeMillis) START-TIME))

  (homeDir [_] (maybeDir @ctxHolder K_BASEDIR))

  (confDir [_] (maybeDir @ctxHolder K_CFGDIR))

  (podsDir [_] (maybeDir @ctxHolder K_PODSDIR))

  (playDir [_] (maybeDir @ctxHolder K_PLAYDIR))

  (logDir [_] (maybeDir @ctxHolder K_LOGDIR))

  (tmpDir [_] (maybeDir @ctxHolder K_TMPDIR))

  (dbDir [_] (maybeDir @ctxHolder K_DBSDIR))

  (blocksDir [_] (maybeDir @ctxHolder K_BKSDIR))

  (kill9 [this]
    (let [ ctx (comp-get-context this) sh (get ctx K_CLISH) ]
      (when-not (nil? sh) (.stop sh))))

  (start [this]
    (let [ ctx (comp-get-context this) root (get ctx K_COMPS)
           k (.lookup root K_KERNEL) ]
      (inspect-pods this)
      (.start k)))

  (stop [this]
    (let [ ctx (comp-get-context this) root (get ctx K_COMPS)
           k (.lookup root K_KERNEL) ]
      (.stop k))) 

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- make-comp-rego [cid parent]
  (comzotohcljc.blason.impl.defaults.ComponentRegistry. cid "" parent (ref {}) (ref {})))

(defmethod comp-initialize Execvisor [this]
  (let [ ctx (comp-get-context this)
         cf (get ctx K_PROPS)
         comps (.getSectionAsMap cf K_COMPS)
         regs (.getSectionAsMap cf K_REGS)
         jmx  (.getSectionAsMap cf K_JMXMGM) ]
    (comp-set-context this (assoc ctx K_EXECV this))
    (CU/test-nonil "conf file: components" comps)
    (CU/test-nonil "conf file: jmx mgmt" jmx)
    (CU/test-nonil "conf file: registries" regs)
    (System/setProperty "file.encoding" "utf-8")
    (let [ ctx (comp-get-context this)
           homeDir (get ctx K_BASEDIR)
           sb (doto (File. (homeDir this) DN_BOXX)
                  (.mkdir))
           bks (doto (File. (homeDir this) DN_BLOCKS)
                  (.mkdir))
           tmp (doto (File. (homeDir this) DN_TMP)
                  (.mkdir))
           db (doto (File. (homeDir this) DN_DBS)
                  (.mkdir))
           log (doto (File. (homeDir this) DN_LOGS)
                  (.mkdir))
           pods (doto (File. (homeDir this) DN_PODS)
                  (.mkdir)) ]
      (precondDir pods)
      (precondDir sb)
      (precondDir log)
      (precondDir tmp)
      (precondDir db)
      (precondDir bks)
      (comp-set-context this (-> ctx
          (assoc K_PODSDIR pods)
          (assoc K_PLAYDIR sb)
          (assoc K_LOGDIR log)
          (assoc K_DBSDIR db)
          (assoc K_TMPDIR tmp)
          (assoc K_BKSDIR bks)) ))
    ;;(start-jmx)
    (let [ root (make-comp-rego K_COMPS this)
           bks (make-comp-rego K_BLOCKS nil)
           apps (make-comp-rego K_APPS nil)
           deployer (make-deployer)
           knl (make-kernel)
           ctx (comp-set-context this (-> (comp-get-context this)
                                        (assoc K_COMPS root)) )
           options { :ctx ctx } ]
      (.reg root deployer)
      (.reg root knl)
      (.reg root apps)
      (.reg root bks)
      (synthesize-component root options)
      (synthesize-component bks options)
      (synthesize-component apps options)
      (synthesize-component deployer options)
      (synthesize-component knl options))
    ))















(defn make-execvisor ^{ :doc "" }
  [parent]
  (Execvisor. K_EXECV "1.0" parent (ref {})) )













(def ^:private execvisor-eof nil)

