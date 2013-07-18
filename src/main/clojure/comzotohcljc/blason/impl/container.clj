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
  comzotohcljc.blason.impl.container )

(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(java.io File))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.blason.impl.defaults])
(use '[comzotohcljc.blason.core.constants])

(require '[ comzotohcljc.util.coreutils :as CU ] )
(require '[ comzotohcljc.util.strutils :as SU ] )
(require '[ comzotohcljc.util.metautils :as MU ] )
(require '[ comzotohcljc.util.procutils :as PU ] )
(require '[clojure.data.json :as JS])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ContainerAPI
  (reifyServices [_] )
  (reifyOneService [_ sid cfg] )
  (reifyService [_ svc cfg] )
  (enabled? [_] ))

(deftype Scheduler [])
(deftype JobCreator [])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Container [id version parent pod cache] Component ContainerAPI

  (enabled? [_]
    (let [ env (get @cache K_ENVCONF)
           c (get env :container) ]
      (if (nil? c)
        false
        (let [ v (get c :enabled) ]
          (if (nil? v) true v)))))

  (reifyServices [this]
    (let [ env (get @cache K_ENVCONF)
           s (get env :services) ]
      (if (empty? s)
          (warn "No system service \"depend\" found in env.conf.")
          (doseq [ [k v] (seq s) ]
            (reifyOneService this k v)))))

  (reifyOneService [this k cfg]
    (let [ svc (SU/nsb (get cfg :service))
           b (get cfg :enabled) ]
      (if (or (and (not (nil? b)) (not b)) (SU/nichts? svc))
        (info "System service \"" svc "\" is disabled.")
        (reifyService this svc cfg))))

        ;;_svcReg.add(key, rc._2 )
  (reifyService [this svc cfg] nil)

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-configure Container [this props]
  (let [ appDir (get props K_APPDIR)
         cfgDir (File. appDir DN_CONF)
         cs (comp-get-cache this)
         mf (CU/load-javaprops (File. appDir MN_FILE))
         envConf (JS/read-str (FileUtils/readFileToString (File. cfgDir "env.conf"))
                              :key-fn keyword)
         appConf (JS/read-str (FileUtils/readFileToString (File. cfgDir "app.conf"))
                              :key-fn keyword) ]
    ;;WebPage.setup(new File(appDir))
    ;;maybeLoadRoutes(cfgDir)
    ;;_ftlCfg = new FTLCfg()
    ;;_ftlCfg.setDirectoryForTemplateLoading( new File(_appDir, DN_PAGES+"/"+DN_TEMPLATES))
    ;;_ftlCfg.setObjectWrapper(new DefaultObjectWrapper())
    (comp-set-cache this (-> cs
                            (assoc K_ENVCONF envConf)
                            (assoc K_APPCONF appConf)
                            (assoc K_MFPROPS mf)))
    (info "container: configured app: " (.id this))))


(defmethod comp-initialize Container [this]
  (let [ cache (comp-get-cache this)
         env (get cache K_ENVCONF)
         app (get cache K_APPCONF)
         mf (get cache K_MFPROPS)
         mCZ (SU/nsb (.get mf "Main-Class"))
         mObj (atom nil)
         jc (JobCreator.)
         sc (Scheduler.)
         c (get env :container) ]
    (CU/test-nestr "Main-Class" mCZ)
    ;;(.compose jc r this)
    ;;_svcReg.setParent( r.getParent.getOrElse(null) )
    (let [ mainCZ (MU/load-class mCZ) ]
      (try
          (reset! mObj (MU/make-obj mainCZ))
          (when-not (nil? @mObj)
            (.contextualize @mObj this)
            (.configure @mObj app)
            (.initialize @mObj))
        (catch Throwable e#
          (warn "Main.Class: No ctor() found." e#))) )

    (let [ svcs (get env :services) ]
      (if (empty? svcs)
          (warn "No system service \"depend\" found in env.conf.")
          (.reifyServices this)))

    (info "Composed app: {}" (.id this))

    (synthesize-component sc { :props c })
    (.start sc)

    (info "Initialized app: " (.id this))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-container [pod]
  (let [ c (Container. (.id pod) "1.0" nil pod (ref {}))
         ctx (comp-get-context pod)
         root (get ctx K_COMPS)
         cl (get ctx K_APP_CZLR)
         apps (.lookup root K_APPS)
         ps { K_APPDIR (File. (-> (.srcUrl pod) (.toURI))) } ]
    ;;(synthesize-component c { :rego apps :ctx ctx :props ps } )
    (comp-compose c apps)
    (comp-contextualize c ctx)
    (comp-configure c ps)
    (if (.enabled? c)
      (do (PU/coroutine (fn []
                          (do
                            (comp-initialize c)
                            (.start c))) cl) c)
      nil)))








(def ^:private container-eof nil)

