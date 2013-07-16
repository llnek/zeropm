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

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.util.procutils :as PU])
(require '[comzotohcljc.util.metautils :as MU])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(require '[comzotohcljc.util.fileutils :as FU])
(require '[comzotohcljc.i18n.i18nutils :as LN])
(require '[comzotohcljc.util.win32ini :as WI])
(use '[comzotohcljc.blason.core.constants])
(import '(com.zotoh.blason.core ConfigError AppClassLoader RootClassLoader ExecClassLoader))
(import '(com.zotoh.blason.etc CmdHelpError))
(import '(java.util Locale))
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
         f (File. cfg (str "app/" (name K_PROPS)))
         home (.getParentFile cfg) ]
    (precondDir home)
    (precondDir cfg)
    (precondFile f)
    (-> {}
      (assoc K_BASEDIR home)
      (assoc K_CFGDIR cfg))) )

(defn- setupClassLoader [ctx]
  (let [ root (get ctx K_ROOT_CZLR)
         cl (ExecClassLoader. root) ]
    (MU/set-cldr cl)
    (assoc ctx K_EXEC_CZLR cl)))

(defn- setupClassLoaderAsRoot [ctx]
  (let [ root (RootClassLoader. (MU/get-cldr)) ]
    (assoc ctx K_ROOT_CZLR root)))

(defn- maybeInizLoaders [ctx]
  (let [ cz (MU/get-cldr) ]
    (if (instance? ExecClassLoader cz)
      (-> ctx
        (assoc K_ROOT_CZLR (.getParent cz))
        (assoc K_EXEC_CZLR cz))
      (setupClassLoader (setupClassLoaderAsRoot ctx)))) )

(defn- loadConf [ctx home]
  (let [ w (WI/parse-inifile (File. home  (str DN_CFG "/app/" (name K_PROPS) )))
         lg (.toLowerCase (.optString w K_LOCALE K_LANG "en"))
         cn (.toUpperCase (.optString w K_LOCALE K_COUNTRY ""))
         loc (if (SU/hgl? cn) (Locale. lg cn) (Locale. lg)) ]
    (assoc ctx K_CLISH nil) ;;TODO
    (assoc ctx K_PROPS w)
    (assoc ctx K_L10N loc)))

(defn- setupResources [ctx]
  (let [ rc (LN/get-resource "comzotohcljc.blason.etc.Resources" (get ctx K_LOCALE)) ]
    (assoc ctx K_L10N) rc))

(defn- pre-parse [args]
  (let [ bh (File. (first args))
         ctx (inizContext bh) ]
    (precondDir (File. bh DN_PATCH))
    (precondDir (File. bh DN_CORE))
    (precondDir (File. bh DN_LIB))
    (->  ctx
      (maybeInizLoaders)
      (loadConf bh)
      (setupResources ))))

(defn- start-exec [ctx]
  (do
    (info "About to start Blason...")
    (-> (get ctx K_EXECV) (.start))
    (info "Blason started.")
    ctx))

(defn- primodial [ctx]
  (let [ cl (get ctx K_EXEC_CZLR)
         wc (get ctx K_PROPS)
         cz (.optString wc K_COMPS K_EXECV "") ]
    (CU/test-nestr "conf file:exec-visor" cz)
    (let [ exec (MU/make-obj cz cl) ]
      (when (nil? exec)
        (throw (ConfigError. "Execvisor class undefined.")))
      ;; order is important!
      (.contextualize exec ctx)
      (.initialize exec)
      (assoc ctx K_EXECV exec))))

(defn- enableRemoteShutdown []
  (let [ port (CU/conv-long (System/getProperty "blason.kill.port") 4444) ]
    nil))

(defn- stop-main [ctx]
  (let [ pid (get ctx K_PIDFILE)
         exec (get ctx K_EXECV) ]
    (when-not (nil? pid) (FileUtils/deleteQuietly pid))
    (info "About to stop Blason...")
    (.stop exec)
    (info "Blason stopped.")
    (send SHOW-STOPPER inc)))

(defn- hookShutdown [ctx]
  (let [ rt (Runtime/getRuntime) ]
    (.addShutdownHook rt (Thread. (reify Runnable
                                    (run [_] (CU/TryC (stop-main ctx))))))
    (enableRemoteShutdown)
    (debug "Added shutdown hook.")
    ctx))

(defn- writePID [ctx]
  (let [ fp (File. (get ctx K_BASEDIR) "blason.pid") ]
    (FileUtils/writeStringToFile fp (PU/pid) "utf-8")
    (assoc ctx K_PIDFILE fp)))

(defn- debug-context [ctx]
  (do 
    (info ctx)))

(defn- pause-main [ctx]
  (do
    (debug-context ctx)
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

