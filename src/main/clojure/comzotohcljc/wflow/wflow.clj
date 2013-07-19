(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.middleware.wflow)

(require '[comzotohcljc.util.seqnumgen :SN])

(defmacro activity [proto pid]
  `(let []
    (with-meta (reify
      Activity
      ~proto) { :typeid ~pid } )) )

(defmacro flowstep [pipe proto]
  `(let [pid# (SN/next-long) ]
    (reify
      FlowStep
      (nextPtr [me#] me#)
      (pipeline [me#] ~pipe)
      (id [me#] pid#)
      ~proto
      Runnable
      (run [me#]
        (fw-run ~pipe me#)))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(deftype FlowStep [^Pipeline pipeline ^FlowStep nextPtr ^Activity template ^Any closure ^long pid ] )
(defn meta-flowstep [fw id a]
  (with-meta fw { :typeid id :activity a :closure nil } ))

(defn make-activity []
  { :typeid :activity } )

;; returns FlowStep
(defmulti fw-evalulate (fn [a job] (:typeid a)))
(defmulti fw-realize (fn [a] (:typeid a)))
(defmulti fw-rerun (fn [a] (:typeid a)))
(defmulti fw-run (fn [a] (:typeid a)))

(defmulti ac-realize (fn [a] (:typeid a)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- runAfter [pipe fw]
  (let [ k (:typeid fw) ]
    (cond
      (= k :NihilStep) (pl-stop pipe)
      (= k :AsyncWaitStep) (-> pipe (.core) (.hold (:nextptr fw)))
      (= k :DelayStep) (-> pipe (.core) (.postpone (:nextptr fw) (:delayMillis fw)))
      (:else (-> f (.core)(.run fw))))))

(defmethod fw-realize :flowstep [fs]
  (ac-realize (:activity fs) fs))

(defmethod fw-rerun :flowstep [fs]
  (-> fs
    (:pipeline) (:core) (.reschedule fs)))

(defn- make-runnable [fs]
  (reify
    FlowStep
    (id [_] (:pid fs))
    Runnable
    (run [this]
      (with-local-vars [ n (:nextptr fs) f (:pipeline fs)
             err nil rc nil ]
        (-> @f (.core) (.dequeue this))
        (try
          (var-set rc (fw-evaluate fs (:job @f)))
          (catch Throwable e#
            (var-set err (pl-onerror e# this @n))))
        (if-not (nil? @err) (var-set rc @err))
        (if (nil? @rc)
          (debug "FlowStep: rc==null => skip.") ;; indicate skip, happens with joins
          (fw-run-after f @rc))))))


















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







(def ^:private wflow-eof nil)

