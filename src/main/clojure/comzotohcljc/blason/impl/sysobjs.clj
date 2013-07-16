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
  comzotohcljc.blason.impl.sysobjs )

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(com.zotoh.blason.core 
  RegistryError ServiceError ConfigError AppClassLoader))
(import '(java.util Date))
(import '(java.io File))
(import '(org.apache.commons.io.filefilter DirectoryFileFilter))
(import '(java.io FileFilter))
(import '(org.apache.commons.io FilenameUtils FileUtils))

(require '[ comzotohcljc.util.coreutils :as CU ] )
(require '[ comzotohcljc.util.strutils :as SU ] )
(require '[ comzotohcljc.util.fileutils :as FU ] )
(require '[ comzotohcljc.util.metautils :as MU ] )
(require '[ comzotohcljc.util.mimeutils :as MI ] )
(require '[ comzotohcljc.util.procutils :as PU ] )

(use '[comzotohcljc.blason.impl.execvisor :only (ExecvisorAPI) ])
(use '[comzotohcljc.blason.impl.deployer :only (DeployerAPI) ])
(use '[comzotohcljc.blason.impl.kernel :only (KernelAPI) ])

(use '[comzotohcljc.blason.core.constants])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- precondDir [d] 
  (CU/test-cond (str "Directory " d " must be read-writable.") (FU/dir-readwrite? d)))

(defn- precondFile [f] 
  (CU/test-cond (str "File " f " must be readable.") (FU/file-read? f)))

(defn- maybeDir [m kn]
  (let [ v (get m kn) ]
    (cond
      (instance? String v) (File. v)
      (instance? File v) v
      :else (throw (ConfigError. (str "No such folder for key: " kn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti comp-get-version class)
(defmulti comp-get-id class)
(defmulti comp-parent class)

(defmulti comp-set-context (fn [a b] (class a)))
(defmulti comp-get-context class)

(defmulti comp-compose (fn [a rego & more] (class a)))
(defmulti comp-contextualize (fn [a b] (class a)))
(defmulti comp-configure class)
(defmulti comp-initialize class)

(defmulti comp-rego-contains? (fn [a b] (class a)))
(defmulti comp-rego-find (fn [a b] (class a)))
(defmulti comp-rego-remove (fn [a b] (class a)))
(defmulti comp-rego-add (fn [a b] (class a)))
(defmulti comp-get-cache class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Component  )
(defprotocol Registry )
(deftype ComponentRegistry [cname parent ctx cache] Registry Component )

(deftype ServiceRegistry [cname parent ctx cache] Registry Component )

(deftype AppRegistry [cname parent ctx cache] Registry Component )

(deftype BlockRegistry [cname parent ctx cache] Registry Component )

(deftype Deployer [cid parent ctx] Component DeployerAPI
  (deploy [this src]
    (let [ app (FilenameUtils/getBaseName (CU/nice-fpath src))
           ctx (comp-get-context this)
           des (File. (:K_PLAYDIR ctx) app)
           fp (File. (.toURI src)) ]
      (if (not (.exists des))
        (FU/unzip fp des)
        (info "deployer: app: " app " has already been deployed."))))
  (undeploy [this app]
    (let [ ctx (comp-get-context this) dir (File. (K_PLAYDIR ctx) app) ]
      (if (.exists dir)
        (do
          (FileUtils/deleteDirectory dir)
          (info "deployer: undeployed app: " app) )
        (warn "deployer: cannot undeploy app: " app ", doesn't exist - no operation taken.")))))

(defn- make-container [c]
  nil)

(defn- maybe-start-pod [apps c]
  (try
    (let [ cid (.cid c) ctr (make-container c) ]
      (if true ;;(.isEnabled ctr)
        (do
          (comp-rego-add apps cid ctr)
        ;;_jmx.register(ctr,"", c.name)
          )
        (info "kernel: container " cid " is set off-line")) )
    (catch Throwable e# (error e#))))

(deftype Kernel [cid parent ctx cache] Component KernelAPI
  (start [_]
    (let [ root (:K_COMPS @ctx)
           apps (comp-rego-find root (:rego/apps))
           cs (comp-get-cache apps) ]
      ;; need this to prevent deadlocks amongst pods
      ;; when there are dependencies
      ;; TODO: need to handle this better
      (doseq [ [k v] (seq @cs) ]
        (let [ r (-> (CU/new-random) (.nextLong 6)) ]
          (maybe-start-pod apps v)
          (PU/safe-wait (* 1000 (Math/max 1 r)))))))
  (stop [this]
    (let [ cs (comp-get-cache this) ]
      (doseq [ [k v] (seq @cs) ]
        (.stop v)))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-get-id :default [c] (.cid c))
(defmethod comp-get-version :default [c] "")
(defmethod comp-parent :default [c] (.parent c))

(defmethod comp-set-context :default [obj x]
  (dosync (ref-set (.ctx obj) x)))

(defmethod comp-get-context :default [obj]
  @(.ctx obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-compose :default [co rego & more] nil)

(defmethod comp-contextualize :default [c ctx]
  (comp-set-context c ctx))

(defmethod comp-initialize :default [c]
  (do nil))

(defmethod comp-configure :default [c]
  (do nil))

(defn- deploy-pod [ctx src]
  (let [ app (FilenameUtils/getBaseName (CU/nice-fpath src))
         des (File. (:K_PLAYDIR ctx) app)
         fp (File. (.toURI src)) ]
    (if (not (.exists des))
      (FU/unzip fp des)
      (info "deployer: app: " app " has already been deployed."))))

(defn- undeploy-pod [ctx app]
  (let [ dir (File. (K_PLAYDIR ctx) app) ]
    (if (.exists dir)
      (do
        (FileUtils/deleteDirectory dir)
        (info "deployer: undeployed app: " app) )
      (warn "deployer: cannot undeploy app: " app ", doesn't exist - no operation taken."))))


(defmethod comp-initialize Deployer [this]
  (let [ ctx (comp-get-context this)
         py (:K_PLAYDIR ctx)
         pd (:K_PODSDIR ctx)
         fs (FileUtils/listFiles pd (into-array String ["pod"]) false) ]
    (doseq [f (seq fs)]
      (deploy-pod ctx (-> f (.toURI)(.toURL))))))

(defmethod comp-contextualize Deployer [this ctx]
  (do
    (precondDir (maybeDir ctx K_BASEDIR))
    (precondDir (maybeDir ctx K_PODSDIR))
    (precondDir (maybeDir ctx K_PLAYDIR))
    (comp-set-context this ctx)))

(defmethod comp-compose Kernel [this rego & more ]
  (do
    (when-not (empty? more)
      (when (instance? String (first more)) ;; JMXServer
        nil))))

(defmethod comp-contextualize Kernel [this ctx]
  (let [ base (maybeDir ctx K_BASEDIR) ]
    (precondDir base)
    (precondDir (maybeDir ctx K_PODSDIR))
    (precondDir (maybeDir ctx K_PLAYDIR))
    (MI/setup-cache (-> (File. base (str DN_CFG "/app/mime.properties"))
                      (.toURI)(.toURL )))
    (comp-set-context this ctx)))

(deftype PODMeta [cid version podType src] )

(defmethod comp-initialize PODMeta [this]
  (let [ ctx (comp-get-context this)
         rcl (get ctx K_ROOT_CZLR)
         cl  (AppClassLoader. rcl) ]
    (.configure cl { K_APPDIR (CU/nice-fpath (File. (.src this))) } )
    (comp-set-context this (assoc ctx K_APP_CZLR cl))) )

(defn- chkManifest [ctx app des mf]
  (let [ apps (-> (comp-get-cache (get ctx K_COMPS))
                  (get :rego/apps))
         ps (CU/load-javaprops mf)
         ver (.get ps "Implementation-Version")
         cz (.get ps "Main-Class") ]

    (CU/test-nestr "POD-MainClass" cz)
    (CU/test-nestr "POD-Version" ver)

    ;;ps.gets("Manifest-Version")
    ;;.gets("Implementation-Title")
    ;;.gets("Implementation-Vendor-URL")
    ;;.gets("Implementation-Vendor")
    ;;.gets("Implementation-Vendor-Id")

    (comp-rego-add apps (keyword app)
      (doto (PODMeta. app ver cz (-> des (.toURI) (.toURL)))
        (comp-contextualize ctx)
        (comp-initialize)) )))


(defn- inspect-pod [ctx des]
  (let [ appid (FilenameUtils/getBaseName  (CU/nice-fpath des))
         mf (File. des MN_FILE) ]
    (precondDir (File. des POD_INF))
    (precondDir (File. des POD_CLASSES))
    (precondDir (File. des POD_LIB))
    (precondDir (File. des META_INF))
    (precondFile (File. des CFG_APP_CF))
    (precondFile (File. des CFG_ENV_CF))
    (precondDir (File. des DN_CONF))
    (precondFile mf)
    (chkManifest ctx appid des mf)) )

(defmethod comp-initialize Kernel [this]
  (let [ ctx (comp-get-context this)
         fs (-> (get ctx K_PLAYDIR)
                (.listFiles (cast FileFilter DirectoryFileFilter/DIRECTORY))) ]
    (doseq [ f (seq fs) ]
      (inspect-pod ctx f)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-get-cache :default [rego] (.cache rego))

(defmethod comp-rego-contains? :default [rego cid]
  (let [ cs (comp-get-cache rego)
         c (get @cs cid) ]
    (if (nil? c) false true)) )

(defmethod comp-rego-find :default [rego cid]
  (let [ cs (comp-get-cache rego)
         par (comp-parent rego)
         c (get @cs cid) ]
    (if (and (nil? c) (not (nil? par))) (comp-rego-find par cid) nil)) )

(defmethod comp-rego-remove :default [rego c]
  (let [ cs (comp-get-cache rego)
         cid (comp-get-id c) ]
    (when (comp-rego-contains? rego cid)
      (dosync (ref-set cs (dissoc @cs cid))))) )

(defmethod comp-rego-add :default [rego c]
  (when-not (nil? c)
    (let [ cid (comp-get-id c) cs (comp-get-cache rego) ]
      (when (comp-rego-contains? rego cid)
        (throw (RegistryError. (str "Component \"" cid "\" already exists" ))))
      (dosync (ref-set cs (assoc @cs cid c))))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-service-rego ^{ :doc "" }
  [cid parent]
  (ServiceRegistry. cid  parent (ref {}) (ref {})))

(defn make-comp-rego ^{ :doc "" }
  [cid parent]
  (ComponentRegistry. cid parent (ref {}) (ref {})))

(defn make-app-rego ^{ :doc "" }
  [cid parent]
  (AppRegistry. cid parent (ref {}) (ref {})))

(defn make-block-rego ^{ :doc "" }
  [cid parent]
  (BlockRegistry. cid parent (ref {}) (ref {})))

(defn make-deployer ^{ :doc "" }
  [cid parent]
  (Deployer. cid parent (ref {}) ))

(defn make-kernel ^{ :doc "" }
  [cid parent]
  (Kernel. cid parent (ref {}) (ref {})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private START-TIME (.getTime (Date.)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- tweak [svcreg ctx c]
  ;;(when-not (nil? svcreg) (.compose c svcreg )) ;; jmx?
  (comp-contextualize c ctx)
  (comp-configure c)
  (comp-initialize c)
  c)

(deftype Execvisor [ctx] ExecvisorAPI
  (getStartTime [_] START-TIME)
  (getUpTimeInMillis [_]
    (- (System/currentTimeMillis) START-TIME))
  (homeDir [_] (maybeDir ctx K_BASEDIR))
  (confDir [_] (maybeDir ctx K_CFGDIR))
  (podsDir [_] (maybeDir ctx K_PODSDIR))
  (playDir [_] (maybeDir ctx K_PLAYDIR))
  (logDir [_] (maybeDir ctx K_LOGDIR))
  (tmpDir [_] (maybeDir ctx K_TMPDIR))
  (dbDir [_] (maybeDir ctx K_DBSDIR))
  (blocksDir [_] (maybeDir ctx K_BKSDIR))
  (kill9 [_]
    (let [ sh (get @ctx K_CLISH) ]
      (when-not (nil? sh) (.stop sh))))
  (start [_]
    (let [ root (get @ctx K_COMPS)
           k (comp-rego-find root K_KERNEL) ]
      (.start k)))
  (stop [_]
    (let [ root (get @ctx K_COMPS)
           k (comp-rego-find root K_KERNEL) ]
      (.stop k))) )

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
           homeDir (:K_BASEDIR ctx)
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
    (let [ root (make-comp-rego "rego/root" nil)
           ctx (comp-get-context this) ]

      (tweak nil ctx root)

      (comp-rego-add root
        (tweak root ctx (make-block-rego :rego/blocks nil)) )

      (comp-rego-add root
        (tweak root ctx (make-app-rego :rego/apps nil)) )

      (comp-rego-add root
        (tweak root ctx (make-deployer :co/deployer nil)) )

      (comp-rego-add root
        (tweak root ctx (make-kernel :co/kernel nil)) )

      (comp-set-context this (assoc ctx K_COMPS root)) )
  ))


(def ^:private impls-eof nil)

