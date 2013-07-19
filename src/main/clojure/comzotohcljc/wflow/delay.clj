(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.wflow.delay)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol DelayStep)
(defprotocol Delay)

(defn make-delay [delayMillis]
  (meta-activity
    (reify Activity Delay) :Delay { :delayMillis delayMillis } ))

(defn- reifyDelay [cur a]
   (let [ pipe (.pipeline cur)
          f (flowstep pipe DelayStep) ]
    (meta-flowstep f :DelayStep a { :delayMillis 0 } )))
 
(defmethod ac-reify :Delay [ac cur]
  (reifyDelay cur ac))

(defmethod ac-realize :Delay [ac fw]
  (let [ hint (meta ac)
         d (:delayMillis hint) ]
    (vary-meta fw merge { :delayMillis d } )))

(defmethod fw-evaluate :DelayStep [fw job]
  fw)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private delay-eof nil)

