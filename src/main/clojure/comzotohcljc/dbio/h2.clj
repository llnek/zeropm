(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.h2)

(use '[clojure.tools.logging :only (info warn error debug)])
(import '(com.zotoh.frwk.dbio DBIOError))
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(use '[comzotohcljc.dbio.dbdrivers])
(use '[comzotohcljc.dbio.dbutils])

(deftype H2 [] DBDriver
  (getId [_] :h2)
  (getTestString [_] "select 1" ))

;; H2
(defmethod getDateKeyword H2 [db] "TIMESTAMP")
(defmethod getDoubleKeyword H2 [db] "DOUBLE")
(defmethod getBlobKeyword H2 [db] "BLOB")
(defmethod getFloatKeyword H2 [db] "FLOAT")
(defmethod genAutoInteger H2 [db table fld]
  (str (getPad db) (:column fld) " " (getIntKeyword db)
            (if (:pkey fld) " IDENTITY(1) " " AUTO_INCREMENT(1) ")))
(defmethod genAutoLong H2 [db table fld]
  (str (getPad db) (:column fld) " " (getLongKeyword db)
            (if (:pkey fld) " IDENTITY(1) " " AUTO_INCREMENT(1) ")))
(defmethod genBegin H2 [db table]
  (str "CREATE CACHED TABLE " table "\n(\n" ))
(defmethod genDrop H2 [db table]
  (str "DROP TABLE " table " IF EXISTS CASCADE" (genExec db) "\n\n"))

(def ^:private h2-eof nil)

