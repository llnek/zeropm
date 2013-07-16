(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.blason.impl.execvisor )

(defprotocol ExecvisorAPI
  (start [_] )
  (stop [_] )
  (homeDir [_] )
  (confDir [_] )
  (podsDir [_] )
  (playDir [_] )
  (logDir [_] )
  (tmpDir [_] )
  (dbDir [_] )
  (blocksDir [_] )
  (getStartTime [_] )
  (kill9 [_] )
  (getUpTimeInMillis [_] ) )


(def ^:private execvisor-eof nil)

