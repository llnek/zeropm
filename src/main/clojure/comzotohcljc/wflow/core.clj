(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.wflow.core )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Activity
  (chain-another [_ act] )
  (reify*  [_ cur] )
  (realize! [_ cur] ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Composite [options cache] Activity
  (chain-another [_ act] )
  (reify*  [_ cur] )
  (realize! [_ cur] )
  CompositeAPI
  (size [_]  (count @cache))
  (add [this act]
    (let [ f (:on-add options) ]
      (dosync (ref-set cache (conj @cache act)))
      (when (fn? f) (apply f act))))
  (reifyInnerSteps [outer]
    (IterWrapper. outer @cache))
)

(defn make-composite [a & more]
  (Group. a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Nihil [] Activity
  (chain-another [this act] (make-composite this act))
  (reify* [_ cur] (reifyZero (.flow cur)))
  (realize! [_ cur] nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






(def ^:private core-eof nil)

