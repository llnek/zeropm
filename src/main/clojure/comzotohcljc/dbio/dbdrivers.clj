(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.dbdrivers)

(import '(com.zotoh.frwk.dbio DBIOError))
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(use '[comzotohcljc.dbio.dbutils])

(def ^:dynamic *USE_DDL_SEP* true)
(def ^:dynamic *DDL_SEP* "-- :")

(defprotocol DBDriver 
  (getId [_] )
  (getTestString [_] ))

(deftype POSTGRESQL [] DBDriver
  (getId [_] :postgresql)
  (getTestString [_] "select 1" ))

(deftype MYSQL [] DBDriver
  (getId [_] :mysql)
  (getTestString [_] "select version()" ))

(deftype ORACLE [] DBDriver
  (getId [_] :oracle)
  (getTestString [_] "select 1 from DUAL" ))

(deftype SQLSERVER [] DBDriver
  (getId [_] :sqlserver)
  (getTestString [_] "select count(*) from sysusers" ))

(deftype H2 [] DBDriver
  (getId [_] :h2)
  (getTestString [_] "select 1" ))


(defn- getNotNull [] "NOT NULL")
(defn- getNull [] "NULL")
(defn- getPad [] "    ")

(defn- nullClause [opt?]
  (if opt? (getNull ) (getNotNull )))


(defn- genSep []
  (if *USE_DDL_SEP* *DDL_SEP* ""))

(defmulti genExec class)
(defmethod genExec :default [] (str ";\n" (genSep)))

(defmulti genDrop class)
(defmethod genDrop :default [table]
  (str "DROP TABLE " table (genExec ) "\n\n"))

(defmulti genBegin class)
(defmethod genBegin :default [table]
  (str "CREATE TABLE " table "\n(\n"))

(defmulti genEnd class)
(defmethod genEnd :default [] (str "\n)" (genExec) "\n\n"))

(defmulti genGrant class)
(defmethod genGrant :default [ table] "")

(defmulti genEndSQL class)
(defmethod genEndSQL :default [] "")

(defn- genColDef [col ty opt? dft]
  (str (getPad) col " " ty " " (nullClause  opt?)
       (if (SU/hgl? dft) (str " DEFAULT "  dft) "")))


(defmulti getFloatKeyword class)
(defmethod getFloatKeyword :default [] "FLOAT")

(defmulti getIntKeyword class)
(defmethod getIntKeyword :default [] "INTEGER")

(defmulti getTSKeyword class)
(defmethod getTSKeyword :default [] "TIMESTAMP")

(defmulti getDateKeyword class)
(defmethod getDateKeyword :default [] "DATE")

(defmulti getBoolKeyword class)
(defmethod getBoolKeyword :default [] "INTEGER")

(defmulti getLongKeyword class)
(defmethod getLongKeyword :default [] "BIGINT")

(defmulti getDoubleKeyword class)
(defmethod getDoubleKeyword :default [] "DOUBLE PRECISION")

(defmulti getStringKeyword class)
(defmethod getStringKeyword :default [] "VARCHAR")

(defmulti getBlobKeyword class)
(defmethod getBlobKeyword :default [] "BLOB")

(defmulti genBytes class)
(defmethod genBytes :default [fld]
  (genColDef (:column fld) (getBlobKeyword) (:null fld) ""))

(defmulti genString class)
(defmethod genString :default [fld]
  (genColDef  (:column fld)
    (str (getStringKeyword) "(" (:size fld) ")")
    (:null fld)
    (if (:default fld) (:default-value fld) "")))

(defmulti genInteger class)
(defmethod genInteger :default [fld]
  (genColDef  (:column fld) (getIntKeyword) (:null fld)
    (if (:default fld) (:default-value fld) "")))

(defmulti genAutoInteger class)
(defmethod genAutoInteger :default [table fld] "")

(defmulti genDouble class)
(defmethod genDouble :default [fld]
  (genColDef  (:column fld) (getDoubleKeyword) (:null fld)
    (if (:default fld) (:default-value fld) "")))

(defmulti genFloat class)
(defmethod genFloat :default [fld]
  (genColDef (:column fld) (getFloatKeyword) (:null fld)
    (if (:default fld) (:default-value fld) "")))

(defmulti genLong class)
(defmethod genLong :default [fld]
  (genColDef (:column fld) (getLongKeyword) (:null fld)
    (if (:default fld) (:default-value fld) "")))

(defmulti genAutoLong class)
(defmethod genAutoLong :default [table fld] "")

(defmulti getTSDefault class)
(defmethod getTSDefault :default [] "CURRENT_TIMESTAMP")

(defmulti genTimestamp class)
(defmethod genTimestamp :default [fld]
  (genColDef (:column fld) (getTSKeyword) (:null fld)
    (if (:default fld) (getTSDefault) "")))

(defmulti genDate class)
(defmethod genDate :default [fld]
  (genColDef (:column fld) (getDateKeyword) (:null fld)
    (if (:default fld) (getTSDefault) "")))

(defmulti genCal class)
(defmethod genCal :default [fld] (genTimestamp fld))

(defmulti genBool class)
(defmethod genBool :default [fld]
  (genColDef (:column fld) (getBoolKeyword) (:null fld)
      (if (:default fld) (:default-value fld) "")))

(defn- genExIndexes [table zm]
  (let [ m (:indexes zm) bf (StringBuilder.) ]
    (doseq [ en (seq m) ]
      (let [ nm (first en) cols (last en) ]
        (when (empty? cols) (throw (DBIOError. (str "Cannot have empty index: " nm))))
        (.append bf (str "CREATE INDEX " (.toLowerCase (str table "_" nm)) " ON " table
                    " ( " (clojure.string/join "," cols) " )" (genExec ) "\n\n" ))))
    (.toString bf)))

(defn- genUniques [zm]
  (let [ m (:uniques zm) bf (StringBuilder.) ]
    (doseq [ en (seq m) ]
      (let [ nm (first en) cols (last en) ]
        (when (empty? cols) (throw (DBIOError. (str "Cannot have empty unique: " nm))))
        (SU/add-delim! bf ",\n"
            (str (getPad ) "UNIQUE(" (clojure.string/join "," cols) ")"))))
    (.toString bf)))

(defn- genPrimaryKey [pkeys]
  (str (getPad ) "PRIMARY KEY(" (clojure.string/join "," pkeys) ")"))

(defn- genBody [table zm]
  (let [ inx (StringBuilder.) bf (StringBuilder.) pkeys (atom #{})
         cols (seq (:fields zm))
         iix (atom 1) ]
    ;; 1st do the columns
    (doseq [ fld (seq (:fields zm)) ]
      (let [ cn (.toUpperCase (:column fld))
             dt (:domain fld)
             col(case dt
                  :boolean (genBool  fld)
                  :timestamp (genTimestamp  fld)
                  :date (genDate  fld)
                  :calendar (genCal  fld)
                  :int (if (:auto fld) (genAutoInteger  table fld) (genInteger  fld))
                  :long (if (:auto fld) (genAutoLong  table fld) (genLong  fld))
                  :double (genDouble  fld)
                  :float (genFloat  fld)
                  :string (genString  fld)
                  :bytes (genBytes  fld)
                  (throw (DBIOError. (str "Unsupported domain type " dt)))) ]
        (when (:pkey fld) (reset! pkeys (conj @pkeys cn)))
        (SU/add-delim! bf ",\n" col)))
    ;; now do the assocs
    (doseq [ soc (seq (:assocs zm)) ]
      (let [ cn (.toUpperCase (:fkey soc)) 
             pos @iix
             col (genColDef  cn (getLongKeyword ) true "") ]
        (SU/add-delim! bf ",\n" col)
        (.append inx (str "CREATE INDEX " table "_x" pos " ON " table
                        " ( "  cn " )" (genExec ) "\n\n"))
        (reset! iix (inc pos))))
    ;; now explicit indexes
    (-> inx (.append (genExIndexes  table zm)))
    ;; now uniques, primary keys and done.
    (when (> (.length bf) 0)
      (when (> (.size pkeys) 0)
        (.append bf (str ",\n" (genPrimaryKey  pkeys))))
      (let [ s (genUniques  zm) ]
        (when (SU/hgl? s)
          (.append bf (str ",\n" s)))))

    [ (.toString bf) (.toString inx) ] ))

(defn- genOneTable [zm]
  (let [ table (.toUpperCase (:table zm))
           b (genBegin table)
           d (genBody table zm)
           e (genEnd)
           s1 (str b (first d) e)
           inx (last d) ]
      (str s1 (if (SU/hgl? inx) inx "") (genGrant table))))

(defn getDDL  ^{ :doc "" }
  [db metaCache]
  (let [ drops (StringBuilder.)
         body (StringBuilder.)
         ms (.getMetas metaCache) ]
    (doseq [ en (seq ms) ]
      (let [ m (last en) id (:id m) tbl (:table m) ]
        (when (SU/hgl? tbl)
          (-> drops (.append (genDrop (.toUpperCase tbl) )))
          (-> body (.append (genOneTable m))))))
    (str "" drops body (genEndSQL))))

;; H2
(defmethod getDateKeyword H2 [] "TIMESTAMP")
(defmethod getDoubleKeyword H2 [] "DOUBLE")
(defmethod getBlobKeyword H2 [] "BLOB")
(defmethod getFloatKeyword H2 [] "FLOAT")
(defmethod genAutoInteger H2 [table fld]
  (str (getPad) (:column fld) " " (getIntKeyword)
            (if (:pkey fld) " IDENTITY(1) " " AUTO_INCREMENT(1) ")))
(defmethod genAutoLong H2 [table fld]
  (str (getPad) (:column fld) " " (getLongKeyword)
            (if (:pkey fld) " IDENTITY(1) " " AUTO_INCREMENT(1) ")))
(defmethod genBegin H2 [table]
  (str "CREATE CACHED TABLE " table "\n(\n" ))
(defmethod genDrop H2 [table]
  (str "DROP TABLE " table " IF EXISTS CASCADE" (genExec) "\n\n"))

;; MYSQL
(defmethod getBlobKeyword MYSQL [] "LONGBLOB")
(defmethod getTSKeyword MYSQL [] "TIMESTAMP")
(defmethod getDoubleKeyword MYSQL [] "DOUBLE")
(defmethod getFloatKeyword MYSQL []  "DOUBLE")

(defmethod genEnd MYSQL [table]
  (str "\n) Type=InnoDB" (genExec) "\n\n"))
(defmethod genAutoInteger MYSQL [table fld]
  (str (getPad) (:column fld) " " (getIntKeyword) " NOT NULL AUTO_INCREMENT"))
(defmethod genAutoLong MYSQL [table fld]
  (str (getPad) (:column fld) " " (getLongKeyword) " NOT NULL AUTO_INCREMENT"))

(defmethod genDrop MYSQL [table]
  (str "DROP TABLE IF EXISTS " table (genExec) "\n\n"))

;; ORACLE
(defmethod getStringKeyword ORACLE [] "VARCHAR2")
(defmethod getTSDefault ORACLE [] "DEFAULT SYSTIMESTAMP")
(defmethod getLongKeyword ORACLE [] "NUMBER(38)")
(defmethod getDoubleKeyword ORACLE [] "BINARY_DOUBLE")
(defmethod getFloatKeyword ORACLE [] "BINARY_FLOAT")
(defmethod genAutoInteger ORACLE [table fld]
  ;;_ids.put(table, fld)
  (genInteger fld))
(defmethod genAutoLong ORACLE [table fld]
  ;;_ids.put(table, fld)
  (genLong fld))
(defmethod genEndSQL ORACLE [] "")
  ;;(str (create_sequence t._1) (create_sequence_trigger t._1, t._2.getId)))
(defmethod genDrop ORACLE [table]
  (str "DROP TABLE " table " CASCADE CONSTRAINTS PURGE" (genExec) "\n\n"))

;; PostgreSQL
(defmethod getTSKeyword POSTGRESQL [] "TIMESTAMP WITH TIME ZONE")
(defmethod getBlobKeyword POSTGRESQL [] "BYTEA")
(defmethod getDoubleKeyword POSTGRESQL [] "DOUBLE PRECISION")
(defmethod getFloatKeyword POSTGRESQL [] "REAL")

(defmethod genCal POSTGRESQL [fld] (genTimestamp fld))

(defmethod genAutoInteger POSTGRESQL [table fld]
  (str (getPad) (:column fld) " " "SERIAL"))

(defmethod genAutoLong POSTGRESQL [table fld]
  (str (getPad) (:column fld) " " "BIGSERIAL"))

(defmethod genDrop POSTGRESQL [table]
  (str "DROP TABLE IF EXISTS " table " CASCADE" (genExec) "\n\n"))

;; SQLserver
(defmethod getBlobKeyword SQLSERVER [] "IMAGE")
(defmethod getTSKeyword SQLSERVER [] "DATETIME")
(defmethod getDoubleKeyword SQLSERVER [] "FLOAT(53)")
(defmethod getFloatKeyword SQLSERVER [] "FLOAT(53)")

(defmethod genAutoInteger SQLSERVER [table fld]
  (str (getPad) (:column fld) " " (getIntKeyword)
    (if (:pkey fld) " IDENTITY (1,1) " " AUTOINCREMENT ")))

(defmethod genAutoLong SQLSERVER [table fld]
  (str (getPad) (:column fld) " " (getLongKeyword)
    (if (:pkey fld) " IDENTITY (1,1) " " AUTOINCREMENT ")))

(defmethod genDrop SQLSERVER [table]
  (str "IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id=object_id('"
       table "')) DROP TABLE " table (genExec) "\n\n"))


(getDDL (POSTGRESQL.) (make-MetaCache testschema))



(def ^:private dbdrivers-eof nil)

