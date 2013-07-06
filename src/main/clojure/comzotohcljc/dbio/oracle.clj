(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.oracle)

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(use '[comzotohcljc.dbio.dbdrivers])
(use '[comzotohcljc.dbio.dbutils])

(deftype Oracle [] DBDriver
  (getId [_] :oracle)
  (getTestString [_] "select 1 from DUAL" ))

;; Oracle
(defmethod getStringKeyword Oracle [db] "VARCHAR2")
(defmethod getTSDefault Oracle [db] "DEFAULT SYSTIMESTAMP")
(defmethod getLongKeyword Oracle [db] "NUMBER(38)")
(defmethod getDoubleKeyword Oracle [db] "BINARY_DOUBLE")
(defmethod getFloatKeyword Oracle [db] "BINARY_FLOAT")
(defmethod genAutoInteger Oracle [db table fld]
  ;;_ids.put(table, fld)
  (genInteger db fld))
(defmethod genAutoLong Oracle [db table fld]
  ;;_ids.put(table, fld)
  (genLong db fld))
(defmethod genEndSQL Oracle [db] "")
  ;;(str (create_sequence t._1) (create_sequence_trigger t._1, t._2.getId)))
(defmethod genDrop Oracle [db table]
  (str "DROP TABLE " table " CASCADE CONSTRAINTS PURGE" (genExec db) "\n\n"))



(def ^:private oracle-eof nil)

