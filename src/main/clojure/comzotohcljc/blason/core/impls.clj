(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.blason.core.impls )

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(com.zotoh.blason.core Constants))
(import '(java.util Date))
(import '(java.io File))

(require '[ comzotohcljc.util.coreutils :as CU ] )
(require '[ comzotohcljc.util.strutils :as SU ] )
(require '[ comzotohcljc.util.fileutils :as FU ] )
(require '[ comzotohcljc.util.metautils :as MU ] )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti comp-get-version class)
(defmulti comp-get-name class)
(defmulti comp-get-context class)
(defmulti comp-set-context class)
(defmulti comp-contextualize class)
(defmulti comp-initialize class)
(defmulti comp-rego-contains? class)
(defmulti comp-rego-find class)
(defmulti comp-rego-remove class)
(defmulti comp-rego-add class)
(defmulti comp-rego-getparent class)

(defmethod comp-get-version :default [c] (:version c))
(defmethod comp-get-name :default [c] (:name c))

(defmethod comp-set-context :default [obj x]
  (dosync (ref-set (.ctx obj) x)))
(defmethod comp-get-context :default [obj]
  @(.ctx obj))

(defmethod comp-rego-find :default [rego cid]
  (let [ par (:parent rego)
           cs (:cache rego)
           c (get @cs cid) ]
    (if (and (nil? c) (not (nil? par))) (comp-rego-find par cid) nil)) )

(defmethod comp-rego-contains? :default [rego cid]
  (let [ cs (:cache rego)
         c (get @cs cid) ]
    (if (nil? c) false true)) )

(defmethod comp-rego-remove :default [rego c]
  (let [ cid (comp-get-name c)
         cs (:cache rego) ]
    (when (comp-rego-contains? rego cid)
      (dosync (ref-set cs (dissoc @cs cid))))) )

(defmethod comp-rego-add :default [rego c]
  (when-not (nil? c)
    (let [ cid (comp-get-name c) cs (:cache rego) ]
      (when (comp-rego-contains? rego cid)
        (throw (RegistryError. (str "Component \"" cid "\" already exists" ))))
      (dosync (ref-set cs (assoc @cs cid c))))) )

(defmethod comp-rego-getparent :default [rego] (:parent rego))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private START-TIME (.getTime (Date.)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- precondDir [d] 
  (CU/test-cond (str "Directory " d " must be read-writable.") (FU/dir-readwrite? d)))

(defn- maybeDir [m kn]
  (let [ v (get m kn) ]
    (cond
      (instance? String v) (File. v)
      (instance? File v) v
      :else (throw (ConfigError. (str "No such folder for key: " kn))))))

(defn- tweak [svcreg ctx c]
  (when-not (nil? svcreg) (.compose c svcreg )) ;; jmx?
  (when (isa? Contextualizable  c)
      (.contextualize c ctx))
  (when (isa? Configurable c) nil) ;; TODO ?
  (when (isa? Initializable c)
      (.initialize c ctx))
  c)

(defprotocol ExecvisorAPI
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

(defmethod comp-start Execvisor [obj]
  (let [ svcs (get (comp-get-context obj) (Constants/PF_SVCS)) ]
    (-> (comp-rego-find svcs (Constants/PF_KERNEL))
      (comp-start))))

(defmethod comp-stop Execvisor [obj]
  (let [ svcs (get (comp-get-context obj) (Constants/PF_SVCS)) ]
    (-> (comp-rego-find svcs (Constants/PF_KERNEL))
      (comp-stop))))

(defmethod comp-contextualize Execvisor [obj x]
  (dosync (ref-set (.ctx obj) x)))

(defmethod comp-initialize Execvisor [obj]
  (let [ ctx (comp-get-context obj) 
         cf (get ctx (Constants/PF_PROPS))
         comps (.getSectionAsMap cf (Constants/PF_COMPS))
         regs (.getSectionAsMap cf (Constants/PF_REGS))
         jmx  (.getSectionAsMap cf (Constants/PF_JMXMGM)) ]
    (comp-set-context obj (assoc ctx (Constants/PF_EXECV) obj))
    (CU/test-nonil "conf file: components" comps)
    (CU/test-nonil "conf file: jmx mgmt" jmx)
    (CU/test-nonil "conf file: registries" regs)
    (System/setProperty "file.encoding" "utf-8")
    (let [ ctx (comp-get-context obj)
           sb (doto (File. (homeDir this) (Constants/DN_BOXX))
                  (.mkdir))
           bks (doto (File. (homeDir this) (Constants/DN_BLOCKS))
                  (.mkdir))
           tmp (doto (File. (homeDir this) (Constants/DN_TMP))
                  (.mkdir))
           db (doto (File. (homeDir this) (Constants/DN_DBS))
                  (.mkdir))
           log (doto (File. (homeDir this) (Constants/DN_LOGS))
                  (.mkdir))
           pods (doto (File. (homeDir this) (Constants/DN_PODS))
                  (.mkdir)) ]
      (precondDir pods)
      (precondDir sb)
      (precondDir log)
      (precondDir tmp)
      (precondDir db)
      (precondDir bks)
      (comp-set-context obj (-> ctx
          (assoc (Constants/K_PODSDIR) pods)
          (assoc (Constants/K_PLAYDIR) sb)
          (assoc (Constants/K_LOGDIR) log)
          (assoc (Constants/K_DBSDIR) db)
          (assoc (Constants/K_TMPDIR) tmp)
          (assoc (Constants/K_BKSDIR) bks)) ))
    ;;(start-jmx)
    (let [ ctx (comp-get-context obj) 
          svcs (MU/make-obj (get regs (Constants/PF_SVCS))) ]
      (tweak nil _ctx svcs)
      (.add svcs (Constants/PF_BLOCKS)
            (tweak svcs _ctx (MU/make-obj (get regs (Constants/PF_BLOCKS)))) )
      (.add svcs (Constants/PF_APPS) 
            (tweak svcs _ctx (MU/make-obj (get regs (Constants/PF_APPS)))) )
      (.add svcs (Constants/PF_DEPLOYER) 
            (tweak svcs _ctx (MU/make-obj (get comps (Constants/PF_DEPLOYER)))) )
      (.add svcs (Constants/PF_KERNEL)
            (tweak svcs _ctx (MU/make-obj (get comps (Constants/PF_KERNEL)))) )
      (comp-set-context obj (assoc ctx (Constants/PF_SVCS) svcs)) )
  ))

(deftype Execvisor [_ctx ] ExecvisorAPI
  (getStartTime [_] START-TIME)
  (getUpTimeInMillis [_]
    (- (System/currentTimeMillis) START-TIME))
  (homeDir [_] (maybeDir (Constants/K_BASEDIR)))
  (confDir [_] (maybeDir (Constants/K_CFGDIR)))
  (podsDir [_] (maybeDir (Constants/K_PODSDIR)))
  (playDir [_] (maybeDir (Constants/K_PLAYDIR)))
  (logDir [_] (maybeDir (Constants/K_LOGDIR)))
  (tmpDir [_] (maybeDir (Constants/K_TMPDIR)))
  (dbDir [_] (maybeDir (Constants/K_DBSDIR)))
  (blocksDir [_] (maybeDir (Constants/K_BKSDIR)))
  (kill9 [_]
    (let [ svcs (get @_ctx (Constants/PF_SVCS)) ]
      (-> (.lookup svcs (Constants/PF_CLISH))
        (.stop))))
  )





(def ^:private impls-eof nil)

