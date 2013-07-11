(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.sqlserver)

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(use '[comzotohcljc.dbio.dbdrivers])
(use '[comzotohcljc.dbio.dbutils])

(deftype SQLServer [] DBDriver
  (getId [_] :sqlserver)
  (getTestString [_] "select count(*) from sysusers" ))

;; SQLServer
(defmethod getBlobKeyword SQLServer [db] "IMAGE")
(defmethod getTSKeyword SQLServer [db] "DATETIME")
(defmethod getDoubleKeyword SQLServer [db] "FLOAT(53)")
(defmethod getFloatKeyword SQLServer [db] "FLOAT(53)")

(defmethod genAutoInteger SQLServer [db table fld]
  (str (getPad db) (genCol fld) " " (getIntKeyword db)
    (if (:pkey fld) " IDENTITY (1,1) " " AUTOINCREMENT ")))

(defmethod genAutoLong SQLServer [db table fld]
  (str (getPad db) (genCol fld) " " (getLongKeyword db)
    (if (:pkey fld) " IDENTITY (1,1) " " AUTOINCREMENT ")))

(defmethod genDrop SQLServer [db table]
  (str "IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id=object_id('"
       table "')) DROP TABLE " table (genExec db) "\n\n"))


;;(println (getDDL (SQLServer.) (make-MetaCache testschema)))


(def ^:private sqlserver-eof nil)

