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
(import '(com.zotoh.blason.core ConfigError Constants AppClassLoader RootClassLoader ExecClassLoader))
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
  (let [ cfg (File. baseDir (Constants/DN_CFG))
         f (File. cfg (str "app/" (Constants/PF_PROPS)))
         home (.getParentFile cfg) ]
    (precondDir home)
    (precondDir cfg)
    (precondFile f)
    (-> {}
      (assoc (Constants/K_BASEDIR) home)
      (assoc (Constants/K_CFGDIR) cfg))) )

(defn- setupClassLoader [ctx]
  (let [ root (get ctx (Constants/K_ROOT_CZLR))
         cl (ExecClassLoader. root) ]
    (MU/set-cldr cl)
    (assoc ctx (Constants/K_EXEC_CZLR) cl)))

(defn- setupClassLoaderAsRoot [ctx]
  (let [ root (RootClassLoader. (MU/get-cldr)) ]
    (assoc ctx (Constants/K_ROOT_CZLR) root)))

(defn- maybeInizLoaders [ctx]
  (let [ cz (MU/get-cldr) ]
    (if (instance? ExecClassLoader cz)
      (-> ctx
        (assoc (Constants/K_ROOT_CZLR) (.getParent cz))
        (assoc (Constants/K_EXEC_CZLR) cz))
      (setupClassLoader (setupClassLoaderAsRoot ctx)))) )

(defn- loadConf [ctx home]
  (let [ w (WI/parse-inifile (File. home  (str (Constants/DN_CFG) "/app/" (Constants/PF_PROPS))))
         lg (.toLowerCase (.optString w (Constants/K_LOCALE) (Constants/K_LANG) "en"))
         cn (.toUpperCase (.optString w (Constants/K_LOCALE) (Constants/K_COUNTRY) ""))
         loc (if (SU/hgl? cn) (Locale. lg cn) (Locale. lg)) ]
    (assoc ctx (Constants/PF_CLISH) nil) ;;TODO
    (assoc ctx (Constants/PF_PROPS) w)
    (assoc ctx (Constants/PF_L10N) loc)))

(defn- setupResources [ctx]
  (let [ rc (LN/get-resource "comzotohcljc.blason.etc.Resources" (get ctx (Constants/PF_LOCALE))) ]
    (assoc ctx (Constants/PF_L10N) rc)))

(defn- pre-parse [args]
  (let [ bh (File. (first args))
         ctx (inizContext bh) ]
    (precondDir (File. bh (Constants/DN_PATCH)))
    (precondDir (File. bh (Constants/DN_CORE)))
    (precondDir (File. bh (Constants/DN_LIB)))
    (->  ctx
      (maybeInizLoaders)
      (loadConf bh)
      (setupResources ))))

(defn- start-exec [ctx]
  (do
    (info "About to start Blason...")
    (-> (get ctx (Constants/PF_EXECV)) (.start))
    (info "Blason started.")
    ctx))

(defn- primodial [ctx]
  (let [ cl (get ctx (Constants/K_EXEC_CZLR))
         wc (get ctx (Constants/PF_PROPS))
         cz (.optString wc (Constants/PF_COMPS) (Constants/PF_EXECV) "") ]
    (CU/test-nestr "conf file:exec-visor" cz)
    (let [ exec (MU/make-obj cz cl) ]
      (when (nil? exec)
        (throw (ConfigError. "Execvisor class undefined.")))
      ;; order is important!
      (.contextualize exec ctx)
      (.initialize exec)
      (assoc ctx (Constants/PF_EXECV) exec))))

(defn- enableRemoteShutdown []
  (let [ port (CU/conv-long (System/getProperty "blason.kill.port") 4444) ]
    nil))

(defn- stop-main [ctx]
  (let [ pid (get ctx (Constants/PF_PIDFILE))
         exec (get ctx (Constants/PF_EXECV)) ]
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
  (let [ fp (File. (get ctx (Constants/K_BASEDIR)) "blason.pid") ]
    (FileUtils/writeStringToFile fp (PU/pid) "utf-8")
    (assoc ctx (Constants/PF_PIDFILE) fp)))

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

