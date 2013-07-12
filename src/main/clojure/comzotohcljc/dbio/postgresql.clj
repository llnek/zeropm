(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.postgresql)

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(use '[comzotohcljc.dbio.dbdrivers])
(use '[comzotohcljc.dbio.dbutils])

(deftype Postgresql [] DBDriver
  (getId [_] :postgresql)
  (getTestString [_] "select 1" ))

;; Postgresql
(defmethod getTSKeyword Postgresql [db] "TIMESTAMP WITH TIME ZONE")
(defmethod getBlobKeyword Postgresql [db] "BYTEA")
(defmethod getDoubleKeyword Postgresql [db] "DOUBLE PRECISION")
(defmethod getFloatKeyword Postgresql [db] "REAL")

(defmethod genCal Postgresql [db fld] (genTimestamp db fld))

(defmethod genAutoInteger Postgresql [db table fld]
  (genColDef db (genCol fld) "SERIAL" false nil))

(defmethod genAutoLong Postgresql [db table fld]
  (genColDef db (genCol fld) "BIGSERIAL" false nil))

(defmethod genDrop Postgresql [db table]
  (str "DROP TABLE IF EXISTS " table " CASCADE" (genExec db) "\n\n"))

;;(def XXX (.getMetas (make-MetaCache testschema)))
;;(println (getDDL (Postgresql.) (make-MetaCache testschema)))


(def ^:private postgresql-eof nil)

