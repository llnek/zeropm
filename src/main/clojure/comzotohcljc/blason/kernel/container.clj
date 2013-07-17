(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.blason.kernel.container )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ContainerAPI
  (reifyServices [_] )
  (reifyOneService [_ sid cfg] )
  (reifyService [_ svc cfg] )
  (enabled? [_] ))


(def ^:private container-eof nil)

