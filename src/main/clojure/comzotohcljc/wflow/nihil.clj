
(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.wflow.nihil )

(defprotocol NihilStep)
(defprotocol Nihil)

(defn make-nihilstep [pipe]
  (let [pid (SN/next-long) ]
    (reify
      FlowStep
      (nextPtr [this] this)
      (pipeline [_] pipe)
      (id [_] pid)
      NihilStep
      Runnable
      (run [this]
        (fw-run pipe this)))) )

(defn make-nihil []
  (with-meta (reify Activity Nihil) { :typeid :Nihil } ))

(defn reifyZero [pipe]
  (let [ f (flowstep pipe NihilStep)
         a (activity Nihil :Nihil) ]
    (meta-flowstep f :Nihilstep a)))

(defmethod ac-realize :Nihil [ac fw] (reifyZero pipe))
(defmethod fw-evaluate :NihilStep [fw job] fw)


(def ^:private nihil-eof nil)


