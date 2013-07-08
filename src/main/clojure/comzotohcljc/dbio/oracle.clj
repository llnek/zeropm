(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.oracle)

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(use '[comzotohcljc.dbio.dbdrivers])
(use '[comzotohcljc.dbio.dbutils])


(defn- create_sequence_trigger [db table col]
  (str "CREATE OR REPLACE TRIGGER TRIG_" table "\n"
        "BEFORE INSERT ON " table "\n"
        "REFERENCING NEW AS NEW\n"
        "FOR EACH ROW\n"
        "BEGIN\n"
        "SELECT SEQ_" table ".NEXTVAL INTO :NEW."
        col " FROM DUAL;\n"
        "END" (genExec db) "\n\n"))

(defn- create_sequence [db table]
  (str "CREATE SEQUENCE SEQ_" table
       " START WITH 1 INCREMENT BY 1"
          (genExec db) "\n\n"))

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
  (do
    (.put *DDL_BVS* table (:column fld))
    (genInteger db fld)))

(defmethod genAutoLong Oracle [db table fld]
  (do
    (.put *DDL_BVS* table (:column fld))
    (genLong db fld)))

(defmethod genEndSQL Oracle [db]
  (let [ m (into {} *DDL_BVS*) bf (StringBuilder.) ]
    (doseq [ en (seq m) ]
      (doto bf
        (.append (create_sequence db (first en)))
        (.append (create_sequence_trigger db (first en) (last en)))))
    (.toString bf)))

(defmethod genDrop Oracle [db table]
  (str "DROP TABLE " table " CASCADE CONSTRAINTS PURGE" (genExec db) "\n\n"))


(println (getDDL (Oracle.) (make-MetaCache testschema)))

(def ^:private oracle-eof nil)

