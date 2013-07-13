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
  comzotohcljc.blason.core.climain )

(require '[comzotohcljc.util.processutils :as PU])
(require '[comzotohcljc.util.metautils :as MU])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(require '[comzotohcljc.util.fileutils :as FU])
(require '[comzotohcljc.i18n.i18nutils :as LN])
(use '[comzotohcljc.blason.etc.constants])
(import '(java.io File))
(import '(org.apache.commons.io FileUtils))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def SHOW-STOPPER (agent 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- precondFile [f]
  (CU/test-cond (str f " must allow read-access.") (FU/file-read? f)) )

(defn- precondDir [d]
  (CU/test-cond (str d " must allow read-write.") (FU/dir-readwrite? d)) )

(defn- inizContext [baseDir]
  (let [ cfg (File. baseDir DN_CFG)
         f (File. cfg (str "app/" PF_PROPS))
         home (.getParentFile cfg) ]
    (precondDir home)
    (precondDir cfg)
    (precondFile f)
    (doto (DefaultContext.)
      (.put K_BASEDIR home)
      (.put K_CFGDIR cfg))) )

(defn- setupClassLoader [ctx root]
  (let [ cl (ExecClassLoader. root) ]
    (.put ctx K_EXEC_CZLR cl)
    (MU/set-cldr cl)))

(defn- setupClassLoaderAsRoot [ctx]
  (let [ root (RootClassLoader. (MU/get-cldr)) ]
    (.put ctx K_ROOT_CZLR root)
    root))

(defn- maybeInizLoaders [ctx]
  (let [ cz (MU/get-cldr) ]
    (if (instance? ExecClassLoader cz)
      (doto ctx
        (.put K_ROOT_CZLR (.getParent cz))
        (.put K_EXEC_CZLR cz))
      (setupClassLoader ctx (setupClassLoaderAsRoot)))) )

(defn- loadConf [ctx home]
  (let [ w (WI/parse-inifile (File. home  (str DN_CFG "/app/" PF_PROPS)))
         lg (.toLowerCase (.optString w K_LOCALE K_LANG "en"))
         cn (.toUpperCase (.optString w K_LOCALE K_COUNTRY ""))
         loc (if (SU/hgl? cn) (Locale. lg cn) (Locale. lg)) ]
    (.put ctx PF_CLISH this) ;;TODO
    (.put ctx PF_PROPS w)
    loc))

(defn- setupResources [loc]
  (let [ rc (LN/get-resource "comzotohcljc.blason.etc.Resources" loc) ]
    { :string-bundle rc :locale loc } ))

(defn- pre-parse [args]
  (let [ bh (File. (first args))
         cx (inizContext bh) ]
    (precondDir (File. bh DN_PATCH))
    (precondDir (File. bh DN_CORE))
    (precondDir (File. bh DN_LIB))
    (maybeInizLoaders)
    (merge { :home bh :context cx } (setupResources (loadConf bh)) )))

(defn- start-exec [env]
  (do
    (info "About to start Blason...")
    (-> (:exec-visor env) (.start))
    (info "Blason started.")
    env))

(defn- primodial [env]
  (let [ ctx (:context env)
         cl (.get ctx K_EXEC_CZLR)
         wc (.get ctx PF_PROPS)
         cz (.optString wc PF_COMPS PF_EXECV "") ]
    (CU/test-nestr "conf file:exec-visor" cz)
    (let [ exec (MU/make-obj cz cl) ]
      (when (nil? exec)
        (throw (ConfigError. "Execvisor class undefined.")))
      ;; order is important!
      (.contextualize exec ctx)
      (.initialize exec)
      (assoc env :exec-visor exec))))

(defn- enableRemoteShutdown []
  (let [ port (CU/conv-long (System/getProperty "blason.kill.port") 4444) ]
    nil))

(defn- stop-main [env]
  (let [ pid (:pid-file env) exec (:exec-visor env) ]
    (when-not (nil? pid) (FileUtils/deleteQuietly pid))
    (info "About to stop Blason...")
    (.stop exec)
    (info "Blason stopped.")
    (send SHOW-STOPPER inc)))

(defn- hookShutdown [env]
  (let [ rt (Runtime/getRuntime) ]
    (.addShutdownHook rt (Thread. (reify Runnable
                                    (run [_] (CU/TryC (stop-main env))))))
    (enableRemoteShutdown)
    (debug "Added shutdown hook.")
    env))

(defn- writePID [env]
  (let [ fp (File. (:home env) "blason.pid") ]
    (FileUtils/writeStringToFile fp (PU/pid) "utf-8")
    (assoc env :pid-file fp)))

(defn- pause-main [env]
  (let [ ctx (:context env) ]
    (.dbgShow ctx)
    (info "Applications are now running...")
    (loop []
      (if (> @SHOW-STOPPER 0)
        nil
        (do (PU/safe-wait 6666) (recur))))
    (info "Bye.")
    (System/exit 0)))

(defn start-main ^{ :doc "" }
  [ & args ]
  (do
    (when (< (.size args) 1) (throw (CmdHelpError. "Blason Home not defined.")))
    (info "set blason-home= " (first args))
    (-> (pre-parse args)
        (primodial)
        (start-exec)
        (hookShutdown)
        (writePID)
        (pause-main))))


(def ^:private climain-eof nil)

