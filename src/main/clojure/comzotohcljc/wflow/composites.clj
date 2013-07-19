(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.wflow.composites )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- reifyInnerSteps [outer children]
    (IterWrapper. outer cache)
  )

(defprotocol MutableVecAPI
  (content [_] )
  (add [_ a]))

(deftype MutableVec [ ^:unsynchronized-mutable cache] MutableVecAPI
  (content [_] cache)
  (add [_ a]
    (set! cache (conj cache a))) )

(defprotocol BlockStep)
(defprotocol Block
  (chain [_ a ] ))

(defn make-block []
  (let [ impl (MutableVec. []) ]
    (meta-activity  (reify
      Activity
      Block
      (children [_] (.content impl))
      (chain [_ a] (.add impl a))) :Block )))

(defn reifyBlock [cur a]
   (let [ pipe (.pipeline cur)
          f (flowstep pipe BlockStep) ]
    (meta-flowstep f :BlockStep a { :steps nil } )))
 
(defmethod ac-reify :Block [ac cur]
  (reifyBlock cur ac))

(defmethod ac-realize :Block [ac fw]
  (let [ w (reifyInnerSteps fw (.children ac)) ]
    (vary-meta fw merge { :step w } )))

(defmethod fw-evaluate :BlockStep [fw job]
  (let [ hint (meta fw)
         steps (:step hint)
         c (:closure hint) ]
         ;; c === data pass back from previous async call?
    (with-local-vars [ rc nil ]
      (if (or (nil? steps) (.isEmpty steps))
        (do
          (debug "BlockStep has no more elements")
          (var-set rc (.nextPtr fw))
          (when (and (not (nil? @rc)) (not (nil? c)))
            (var-set rc (vary-meta @rc merge { :closure c } )) )
          (fw-realize @rc))
        (do
          (debug "BlockStep has " (.size steps) " elements")
          (var-set rc (.shift steps))
          (when-not (nil? c)
            (var-set rc (vary-meta @rc merge { :closure c }) ))
          (fw-evaluate @rc job))) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private composites-eof nil)


