(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.wflow.pipeline )

(require '[comzotohcljc.frwk.util.seqnumgen :as SN])
(require '[comzotohcljc.frwk.util.coreutils :as CU])

(defprotocol Pipeline
  (container [_] )
  (job [_] )
  (active? [_] )
  (next-activity-id [_] )
  (core [this] )
  (pid [_] )
  (onStart [_] )
  (onEnd [_] )
  (onError [_ e cur nxt] )
  (start [_] )
  (stop [_] )
)

(deftype Job [id parent data])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-job ^{ :doc "" }
  [id parent]
  (Job. id parent (ref {})) )

(defn make-pipeline ^{ :doc "" }
  [^Job jb options]
  (let [ state (atom false) id (SN/next-long) ]
    (reify
      Object
      (toString [this]
        (str (.getSimpleName (.getClass this)) "(" id ")"))
      Pipeline
      (container [_] (.parent jb))
      (job [_] jb)
      (active? [_] @state)
      (next-activity-id [_] (SN/next-long))
      (core [this] (.scheduler (container this)))
      (pid [_] id)
      (onStart [_]
        (let [ f (:on-start options) ]
          (when (fn? f) (apply f))))
      (onEnd [_]
        (let [ f (:on-end options) ]
          (when (fn? f) (apply f))
          (debug "pipeline: " id " => end called.")))
      (onError [this e cur nxt]
        (let [ f (:on-error options) ]
          (if (fn? f)
            (apply f e cur nxt)
            (do
              (error e)
              (reifyZero this)))))
      (start [this]
        (let [ ps (:pre-start options)
               os (:on-start options)
               s1 (reifyZero this) ]
          (debug "pipeline: " id " => starting...")
          (when (fn? ps) (apply ps))
          (let [ a1 (if (fn? os) (apply os) nil)
                 a2 (cond
                      (instance? Nihil a1) (reifyZero this)
                      (nil? a1) (reifyZero this)
                      :else (.reify a1 s1)) ]
            (try
              (.run (core this) a2)
              (catch Throwable e#
                (onError this e# a2 (.nextStep a2)))
              (finally (reset! state true))))))
      (stop [this]
        (onEnd this))
      )))






(def ^:private pipeline-eof nil)

