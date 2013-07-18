(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.blason.impl.jobcreator )

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.blason.util.metautils :as MU])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- maybeMkPipe [cz job]
  (try
    (let [ jz (class comzotohcljc.blason.kernel.sysobjs.Job)
           z (MU/load-class cz)
           c (.getConstructor z (into-array Class [jz]))
           obj (.newInstance c job) ]
      (if (instance? comzotohcljc.blason.wflow.core.Pipeline obj)
        obj
        nil))
    (catch Throwable e#
      (do (error e#) nil))))

;; parent === Container
(deftype JobCreator [parent]
  (update [target & more]
    (let [ ev (nth more 0)
           cz (if (.hasRouter ev) 
                (.routerClass ev)
                (nth more 1))
           job (Job. (SN/next-long) parent ev) ]
      (try
        (let [ p (maybeMkPipe cz job)
               q (if (nil? p) (OrphanFlow. job) p) ]
          (.start q))
        (catch Throwable e#
          (-> (FatalErrorFlow. job) (.start))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private jobcreator-eof nil)

