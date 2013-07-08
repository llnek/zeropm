(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.dbdrivers)

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(use '[comzotohcljc.dbio.dbutils])

(import '(com.zotoh.frwk.dbio DBIOError))


(def ^:dynamic *USE_DDL_SEP* true)
(def ^:dynamic *DDL_SEP* "-- :")

(defprotocol DBDriver
  (getTestString [_] )
  (getId [_] ))

(defn- getcolname [flds fid] 
  (let [ c (:column (get flds fid)) ]
    (if (SU/hgl? c) (.toUpperCase c) c)))

(defn- getNotNull [db] "NOT NULL")

(defn- getNull [db] "NULL")

(defn getPad [db] "    ")

(defn- nullClause [db opt?]
  (if opt? (getNull db) (getNotNull db)))

(defn- genSep [db]
  (if *USE_DDL_SEP* *DDL_SEP* ""))

(defmulti genExec (fn [a & more] (class a)))
(defmethod genExec :default [db] (str ";\n" (genSep db)))

(defmulti genDrop (fn [a & more] (class a)))
(defmethod genDrop :default [db table]
  (str "DROP TABLE " table (genExec db) "\n\n"))

(defmulti genBegin (fn [a & more] (class a)))
(defmethod genBegin :default [db table]
  (str "CREATE TABLE " table "\n(\n"))

(defmulti genEnd (fn [a & more] (class a)))
(defmethod genEnd :default [db] (str "\n)" (genExec db) "\n\n"))

(defmulti genGrant (fn [a & more] (class a)))
(defmethod genGrant :default [db table] "")

(defmulti genEndSQL (fn [a & more] (class a)))
(defmethod genEndSQL :default [db] "")

(defn genColDef [db col ty opt? dft]
  (str (getPad db) (.toUpperCase col) " " ty " " (nullClause db opt?)
       (if (nil? dft) "" (str " DEFAULT " dft))))

(defmulti getFloatKeyword (fn [a & more] (class a)))
(defmethod getFloatKeyword :default [db] "FLOAT")

(defmulti getIntKeyword (fn [a & more] (class a)))
(defmethod getIntKeyword :default [db] "INTEGER")

(defmulti getTSKeyword (fn [a & more] (class a)))
(defmethod getTSKeyword :default [db] "TIMESTAMP")

(defmulti getDateKeyword (fn [a & more] (class a)))
(defmethod getDateKeyword :default [db] "DATE")

(defmulti getBoolKeyword (fn [a & more] (class a)))
(defmethod getBoolKeyword :default [db] "INTEGER")

(defmulti getLongKeyword (fn [a & more] (class a)))
(defmethod getLongKeyword :default [db] "BIGINT")

(defmulti getDoubleKeyword (fn [a & more] (class a)))
(defmethod getDoubleKeyword :default [db] "DOUBLE PRECISION")

(defmulti getStringKeyword (fn [a & more] (class a)))
(defmethod getStringKeyword :default [db] "VARCHAR")

(defmulti getBlobKeyword (fn [a & more] (class a)))
(defmethod getBlobKeyword :default [db] "BLOB")

(defmulti genBytes (fn [a & more] (class a)))
(defmethod genBytes :default [db fld]
  (genColDef db (:column fld) (getBlobKeyword db) (:null fld) nil))

(defmulti genString (fn [a & more] (class a)))
(defmethod genString :default [db fld]
  (genColDef  db (:column fld)
    (str (getStringKeyword db) "(" (:size fld) ")")
    (:null fld)
    (if (:default fld) (:default-value fld) nil)))

(defmulti genInteger (fn [a & more] (class a)))
(defmethod genInteger :default [db fld]
  (genColDef db (:column fld) (getIntKeyword db) (:null fld)
    (if (:default fld) (:default-value fld) nil)))

(defmulti genAutoInteger (fn [a & more] (class a)))
(defmethod genAutoInteger :default [db table fld] "")

(defmulti genDouble (fn [a & more] (class a)))
(defmethod genDouble :default [db fld]
  (genColDef db (:column fld) (getDoubleKeyword db) (:null fld)
    (if (:default fld) (:default-value fld) nil)))

(defmulti genFloat (fn [a & more] (class a)))
(defmethod genFloat :default [db fld]
  (genColDef db (:column fld) (getFloatKeyword db) (:null fld)
    (if (:default fld) (:default-value fld) nil)))

(defmulti genLong (fn [a & more] (class a)))
(defmethod genLong :default [db fld]
  (genColDef db (:column fld) (getLongKeyword db) (:null fld)
    (if (:default fld) (:default-value fld) nil)))

(defmulti genAutoLong (fn [a & more] (class a)))
(defmethod genAutoLong :default [db table fld] "")

(defmulti getTSDefault (fn [a & more] (class a)))
(defmethod getTSDefault :default [db] "CURRENT_TIMESTAMP")

(defmulti genTimestamp (fn [a & more] (class a)))
(defmethod genTimestamp :default [db fld]
  (genColDef db (:column fld) (getTSKeyword db) (:null fld)
    (if (:default fld) (getTSDefault db) nil)))

(defmulti genDate (fn [a & more] (class a)))
(defmethod genDate :default [db fld]
  (genColDef db (:column fld) (getDateKeyword db) (:null fld)
    (if (:default fld) (getTSDefault db) nil)))

(defmulti genCal (fn [a & more] (class a)))
(defmethod genCal :default [db fld] (genTimestamp db fld))

(defmulti genBool (fn [a & more] (class a)))
(defmethod genBool :default [db fld]
  (genColDef db (:column fld) (getBoolKeyword db) (:null fld)
      (if (:default fld) (:default-value fld) nil)))

(defn- genExIndexes [db cache table flds zm]
  (let [ m (collect-db-indexes cache zm) bf (StringBuilder.) ]
    (doseq [ en (seq m) ]
      (let [ nm (first en) cols (map #(getcolname flds %) (last en)) ]
        (when (empty? cols) (throw (DBIOError. (str "Cannot have empty index: " nm))))
        (.append bf (str "CREATE INDEX " 
                         (.toLowerCase (str table "_" (name nm)))
                         " ON " table
                    " ( " (clojure.string/join "," cols) " )" (genExec db) "\n\n" ))))
    (.toString bf)))

(defn- genUniques [db cache flds zm]
  (let [ m (collect-db-uniques cache zm) bf (StringBuilder.) ]
    (doseq [ en (seq m) ]
      (let [ nm (first en) cols (map #(getcolname flds %) (last en)) ]
        (when (empty? cols) (throw (DBIOError. (str "Cannot have empty unique: " (name nm)))))
        (SU/add-delim! bf ",\n"
            (str (getPad db) "UNIQUE(" (clojure.string/join "," cols) ")"))))
    (.toString bf)))

(defn- genPrimaryKey [db zm pks]
    (str (getPad db) "PRIMARY KEY(" 
         (.toUpperCase (SU/nsb (clojure.string/join "," pks)) )
         ")"))

(defn- genBody [db cache table zm]
  (let [ inx (StringBuilder.) bf (StringBuilder.) pkeys (atom #{})
         iix (atom 1)
         flds (collect-db-fields cache zm) ]
    ;; 1st do the columns
    (doseq [ en (seq flds) ]
      (let [ fld (last en) cn (.toUpperCase (:column fld))
             dt (:domain fld)
             col (case dt
                  :boolean (genBool db fld)
                  :timestamp (genTimestamp db fld)
                  :date (genDate db fld)
                  :calendar (genCal db fld)
                  :int (if (:auto fld) (genAutoInteger db table fld) (genInteger db fld))
                  :long (if (:auto fld) (genAutoLong db table fld) (genLong db fld))
                  :double (genDouble db fld)
                  :float (genFloat db fld)
                  :string (genString db fld)
                  :bytes (genBytes db fld)
                  (throw (DBIOError. (str "Unsupported domain type " dt)))) ]
        (when (:pkey fld) (reset! pkeys (conj @pkeys cn)))
        (SU/add-delim! bf ",\n" col)))
    ;; now do the assocs
    ;; now explicit indexes
    (-> inx (.append (genExIndexes db cache table flds zm)))
    ;; now uniques, primary keys and done.
    (when (> (.length bf) 0)
      (when (> (.size @pkeys) 0)
        (.append bf (str ",\n" (genPrimaryKey db zm @pkeys))))
      (let [ s (genUniques db cache flds zm) ]
        (when (SU/hgl? s)
          (.append bf (str ",\n" s)))))

    [ (.toString bf) (.toString inx) ] ))

(defn- genOneTable [db ms zm]
  (let [ table (.toUpperCase (:table zm))
           b (genBegin db table)
           d (genBody db ms table zm)
           e (genEnd db)
           s1 (str b (first d) e)
           inx (last d) ]
      (str s1 (if (SU/hgl? inx) inx "") (genGrant db table))))

(defn getDDL  ^{ :doc "" }
  [db metaCache]
  (let [ ms (.getMetas metaCache)
         drops (StringBuilder.)
         body (StringBuilder.) ]
    (doseq [ en (seq ms) ]
      (let [ tdef (last en) id (first en) tbl (:table tdef) ]
        (when (and (not (:abstract tdef)) (SU/hgl? tbl))
          (debug "model id: " (name id) " table: " tbl)
          (-> drops (.append (genDrop db (.toUpperCase tbl) )))
          (-> body (.append (genOneTable db ms tdef))))))
    (str "" drops body (genEndSQL db))))


(def ^:private dbdrivers-eof nil)

