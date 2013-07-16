(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.blason.kernel.container )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ContainerAPI
  (enabled? [_] ))


(def ^:private container-eof nil)

