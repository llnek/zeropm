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

(defmethod genCal Postgresql [db fld] (genTimestamp fld))

(defmethod genAutoInteger Postgresql [db table fld]
  (str (getPad db) (:column fld) " " "SERIAL"))

(defmethod genAutoLong Postgresql [db table fld]
  (str (getPad db) (:column fld) " " "BIGSERIAL"))

(defmethod genDrop Postgresql [db table]
  (str "DROP TABLE IF EXISTS " table " CASCADE" (genExec db) "\n\n"))


(println (getDDL (Postgresql.) (make-MetaCache testschema)))


(def ^:private postgresql-eof nil)

