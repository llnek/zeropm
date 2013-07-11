(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.sqlops )

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.metautils :as MU])
(require '[comzotohcljc.util.strutils :as SU])
(require '[comzotohcljc.util.ioutils :as IO])
(require '[comzotohcljc.dbio.dbutils :as DU])
(use '[comzotohcljc.dbio.sqlserver])
(use '[comzotohcljc.dbio.postgresql])
(use '[comzotohcljc.dbio.mysql])
(use '[comzotohcljc.dbio.oracle])
(use '[comzotohcljc.dbio.h2])

(import '(java.util Calendar GregorianCalendar TimeZone))
(import '(java.sql Types SQLException))
(import '(java.math BigDecimal BigInteger))
(import '(java.sql Date Timestamp Blob Clob Statement PreparedStatement Connection))
(import '(java.io Reader InputStream))
(import '(com.zotoh.frwk.dbio DBIOError OptLockError))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- lc-ent [ent] (.toLowerCase (name ent)))

(defn ese ^{ :doc "Escape string entity for sql." }
  ([ent] (uc-ent ent))
  ([ch ent] (str ch (uc-ent ent) ch))
  ([c1 ent c2] (str c1 (uc-ent ent) c2)))

(defn table-name
  ([mdef] (:table mdef))
  ([mid cache] (table-name (get cache mid))))

(defn col-name
  ([fdef] (:column fdef))
  ([fid zm] (col-name (get zm fid))))

(defn- merge-meta [m1 m2] (merge m1 m2))

(defn- fmtUpdateWhere [lock zm]
  (str (ese (col-name :rowid zm)) "=?"
       (if lock
          (str " AND " (ese (col-name :verid zm)) "=?")
          "")))

(defn- lockError [opcode cnt table rowID]
  (when (= cnt 0)
    (throw (OptLockError. opcode table rowID))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol DBAPI
  (supportsOptimisticLock [_] )
  (vendor [_]  )
  (finz [_] )
  (open [_] )
  (newCompositeSQLr [_] )
  (newSimpleSQLr [_] ) )

(deftype SimpleDB [jdbc options] DBAPI
  (supportsOptimisticLock [_]
    (if (contains? options :opt-lock) (:opt-lock options) true))
  (vendor [_]  (DU/get-vendor jdbc))
  (finz [_] nil)
  (open [_] (-> nil (.getPool)(.nextFree))) ;;TODO
  (newCompositeSQLr [this] nil)
  (newSimpleSQLr [this] nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sql-filter-clause [filters]
  (let [ wc (reduce (fn [sum en]
                (SU/add-delim! sum " AND "
                   (str (ese (first en)) (if (nil? (last en)) " IS NULL " " = ? "))))
                (StringBuilder.)
                (seq filters)) ]
    [ (SU/nsb wc) (CU/flatten-nil (vals filters)) ] ))

(defn- readCol [sqlType pos rset]
  (let [ obj (.getObject rset (int pos))
         inp (cond
                  (instance? Blob obj) (.getBinaryStream obj)
                  (instance? InputStream obj) obj
                  :else nil)
         rdr (cond
                  (instance? Clob obj) (.getCharacterStream obj)
                  (instance? Reader obj) obj
                  :else nil) ]
    (cond
      (not (nil? rdr)) (with-open [r rdr] (IO/read-chars r))
      (not (nil? inp)) (with-open [p inp] (IO/read-bytes p))
      :else obj)))

(defn- readOneCol [sqlType pos rset]
  (let [ cv (case sqlType
                Types/TIMESTAMP (.getTimestamp rset (int pos) *GMT-CAL*)
                Types/DATE (.getDate rset (int pos) *GMT-CAL*)
                (readCol sqlType pos rset)) ]
    cv))

(defn- model-injtor [cache zm row cn ct cv]
  (let [ info (meta zm) cols (:columns info) 
         fdef (get  cols cn) ]
    (if (nil? fdef)
      row
      (assoc row (:id fdef) cv))))

(defn- std-injtor [row cn ct cv]
  (assoc row (keyword (.toUpperCase cn)) cv))

(defn- row2obj [finj rs rsmeta]
  (let [ cc (.getColumnCount rsmeta)
         row (atom {})
         rr (range 1 (inc cc)) ]
    (doseq [ pos (seq rr) ]
      (let [ cn (.getColumnName rsmeta (int pos))
             ct (.getColumnType rsmeta (int pos))
             cv (readOneCol ct pos rs) ]
        (reset! row (finj @row cn ct cv))))
    @row))

(defn- insert? [sql]
  (.startsWith (.toLowerCase (SU/strim sql)) "insert"))

(defn- setBindVar [ps pos p]
  (cond
    (instance? String p) (.setString ps pos p)
    (instance? Long p) (.setLong ps pos p)
    (instance? Integer p) (.setInt ps pos p)
    (instance? Short p) (.setShort ps pos p)

    (instance? BigDecimal p) (.setBigDecimal ps pos p)
    (instance? BigInteger p) (.setBigDecimal ps pos (BigDecimal. p))

    (instance? InputStream p) (.setBinaryStream ps pos p)
    (instance? Reader p) (.setCharacterStream ps pos p)
    (instance? Blob p) (.setBlob ps pos p)
    (instance? Clob p) (.setClob ps pos p)

    (instance? (MU/chars-class) p) (.setString ps pos (String. p))
    (instance? (MU/bytes-class) p) (.setBytes ps pos p)

    (instance? Boolean p) (.setInt ps pos (if p 1 0))
    (instance? Double p) (.setDouble ps pos p)
    (instance? Float p) (.setFloat ps pos p)

    (instance? Timestamp p) (.setTimestamp ps pos p *GMT-CAL*)
    (instance? Date p) (.setDate ps pos p *GMT-CAL*)
    (instance? Calendar p) (.setTimestamp ps pos (Timestamp. (.getTimeInMillis p)) *GMT-CAL*)

    :else (throw (DBIOError. (str "Unsupported param type: " (type p))))))

(defn- mssql-tweak-sqlstr [sqlstr token cmd]
  (loop [ stop false sql sqlstr ]
    (if stop
      sql
      (let [ lcs (.toLowerCase sql) pos (.indexOf lcs (name token))
             rc (if (< pos 0)
                       []
                       [(.substring sql 0 pos) (.substring sql pos)]) ]
        (if (empty? rc)
          (recur true sql)
          (recur false (str (first rc) " WITH (" cmd ") " (last rc)) ))))))

(defn- jiggleSQL [db sqlstr]
  (let [ v (.vendor db)  sql (SU/strim sqlstr) lcs (.toLowerCase sql) ]
    (when (instance? comzotohcljc.dbio.sqlserver.SQLServer v)
      (cond
        (.startsWith lcs "select") (mssql-tweak-sqlstr sql :where "NOLOCK")
        (.startsWith lcs "delete") (mssql-tweak-sqlstr sql :where "ROWLOCK")
        (.startsWith lcs "update") (mssql-tweak-sqlstr sql :set "ROWLOCK")
        :else sql))))

(defn- build-stmt [db conn sqlstr params]
  (let [ sql (jiggleSQL db sqlstr)
         ps (if (insert? sql)
              (.prepareStatement conn sql Statement/RETURN_GENERATED_KEYS)
              (.prepareStatement conn sql)) ]
    (debug "SQL: {}" sql)
    (doseq [n (seq (range 0 (.size params))) ]
      (setBindVar ps (inc n) (nth params n)))))

(defn- handleGKeys [rs cnt options]
  (let [ rc (cond
                (= cnt 1) (.getObject rs 1)
                :else (.getLong rs (:pkey options))) ]
    { :1 rc }))

(defprotocol ^:private SQueryAPI
  (sql-select [_ sql pms row-provider-func] [_ sql pms] )
  (sql-executeWithOutput [_  sql pms options] )
  (sql-execute [_  sql pms] ) )

(deftype ^:private SQuery [_db _metaCache _conn] SQueryAPI

  (sql-executeWithOutput [this sql pms options]
    (with-open [ stmt (build-stmt _db _conn sql pms) ]
      (with-open [ rc (.executeUpdate stmt) ]
          (with-open [ rs (.getGeneratedKeys stmt) ]
            (let [ cnt (if (nil? rs)
                            0
                            (-> (.getMetaData rs) (.getColumnCount))) ]
              (if (and (> cnt 0) (.next rs))
                (handleGKeys rs cnt options)
                {}
                ))))))

  (sql-select [this sql pms ] (sql-select this sql pms (partial row2obj std-injtor)))
  (sql-select [this sql pms func]
    (with-open [ stmt (build-stmt _db _conn sql pms) ]
      (with-open [ rs (.executeQuery stmt) ]
        (let [ rsmeta (.getMetaData rs) ]
          (loop [ sum [] ok (.next rs) ]
            (if (not ok)
              sum
              (recur (conj sum (apply func rs rsmeta)) (.next rs))))))))

  (sql-execute [this sql pms]
    (with-open [ stmt (build-stmt _db _conn sql pms) ]
      (.executeUpdate stmt)))  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol SQLProcAPI
  (doExecuteWithOutput [_ conn sql pms options] )
  (doExecute [_ conn sql pms] )
  (doQuery [_ conn sql pms model] [_ conn sql pms] )
  (doCount [_  conn model] )
  (doPurge [_  conn model] )
  (doDelete [_  conn pojo] )
  (doInsert [_  conn pojo] )
  (doUpdate [_  conn pojo] ) )

(deftype SQLProc [_db _metaCache] SQLProcAPI

  (doQuery [_ conn sql pms model]
    (let [ zm (get _metaCache model) ]
      (when (nil? zm)
        (throw (DBIOError. (str "Unknown model " model))))
      (let [ px (partial model-injtor _metaCache zm) 
             pf (partial row2obj px) ]
        (-> (SQuery. _db _metaCache conn) (.sql-select sql pms pf )))))

  (doQuery [_ conn sql pms]
    (let [ pf (partial row2obj std-injtor) ]
      (-> (SQuery. _db _metaCache conn) (.sql-select sql pms pf ))) )

  (doCount [this conn model]
    (let [ rc (doQuery this conn
                (str "SELECT COUNT(*) FROM " 
                     (ese (table-name model _metaCache))) [] ) ]
      (if (empty? rc)
        0
        (val (first rc)))))

  (doPurge [_ conn model]
    (let [ sql (str "DELETE FROM " (ese (table-name model _metaCache))) ]
      (do (-> (SQuery. _db _metaCache conn) (.sql-execute sql [])) nil)))

  (doDelete [this conn obj]
    (let [ info (meta obj) model (:typeid info) zm (get _metaCache model) ]
      (when (nil? zm) (throw (DBIOError. (str "Unknown model " model))))
      (let [ lock (.supportsOptimisticLock _db)
             table (table-name zm)
             rowid (:rowid info)
             verid (:verid info)
             p (if lock [rowid verid] [rowid] )
             w (fmtUpdateWhere lock zm)
             cnt (doExecute this conn (str "DELETE FROM " (ese table) " WHERE " w) p) ]
        (when lock (lockError "delete" cnt table rowid))
        cnt)))

  (doInsert [this conn obj]
    (let [ info (meta obj) model (:typeid info) zm (get _metaCache model) ]
      (when (nil? zm) (throw (DBIOError. (str "Unknown model " model))))
      (let [ lock (.supportsOptimisticLock _db)
             table (table-name zm)
             flds (:fields (meta zm))
             pms (atom [])
             now (CU/now-jtstamp)
             s2 (StringBuilder.) s1 (StringBuilder.) ]
        (doseq [ [k v] (seq obj) ]
          (let [ fdef (get flds k) cn (:column fdef) ]
            (when (and (not (nil? fdef))
                       (not (:auto fdef))
                       (not (:system fdef)))
              (SU/add-delim! s1 "," (ese cn))
              (SU/add-delim! s2 "," (if (nil? v) "NULL" "?"))
              (when-not (nil? v)
                (reset! pms (conj @pms v))))))

        (if (= (.length s1) 0)
          nil
          (let [ out (doExecuteWithOutput this conn
                        (str "INSERT INTO " (ese table) "(" s1 ") VALUES (" s2 ")" ) 
                        @pms { :pkey (col-name :rowid zm) } ) ]
            (when (empty? out)
              (throw (DBIOError. (str "Insert requires row-id to be returned."))))
            (let [ wm { :rowid (:pkey out) :verid 0 } ]
              (when-not (instance? Long (:rowid wm) )
                (throw (DBIOError. (str "RowID data-type must be Long."))))
              (vary-meta obj merge-meta wm))))
      )))

  (doUpdate [this conn obj]
    (let [ info (meta obj) model (:typeid info) zm (get _metaCache model) ]
      (when (nil? zm) (throw (DBIOError. (str "Unknown model " model))))
      (let [ lock (.supportsOptimisticLock _db)
             cver (CU/nnz (:verid info))
             table (table-name zm)
             rowid (:rowid info)
             flds (:fields (meta zm))
             sb1 (StringBuilder.)
             nver (inc cver)
             pms (atom [])
             now (CU/now-jtstamp) ]
        (doseq [ [k v] (seq obj) ]
          (let [ fdef (get flds k) cn (col-name fdef) ]
            (when (and (not (nil? fdef))
                       (:updatable fdef)
                       (not (:auto fdef)) (not (:system fdef)) )
              (doto sb1
                (SU/add-delim! "," (ese cn))
                (.append (if (nil? v) "=NULL" "=?")))
              (when-not (nil? v)
                (reset! pms (conj @pms v))))))
        (if (= (.length sb1) 0)
          nil
          (do
            (-> (SU/add-delim! sb1 "," (ese (col-name :last-modify zm)))
                (.append "=?"))
            (reset! pms (conj @pms now))
            (when lock ;; up the version
              (-> (SU/add-delim! sb1 "," (ese (col-name :verid zm)))
                  (.append "=?"))
              (reset! pms (conj @pms nver)))
            ;; for the where clause
            (reset! pms (conj @pms rowid))
            (when lock (reset! pms (conj @pms cver)))
            (let [ cnt (doExecute this conn (str "UPDATE " (ese table) " SET " sb1 " WHERE "
                                            (fmtUpdateWhere lock zm)) @pms) ]
              (when lock (lockError "update" cnt table rowid))
              (vary-meta obj merge-meta { :verid nver :last-modify now })
              )))
      )))

  (doExecuteWithOutput [this conn sql pms options]
    (-> (SQuery. _db _metaCache conn)
        (.sql-executeWithOutput sql pms options)))

  (doExecute [this conn sql pms]
    (-> (SQuery. _db _metaCache conn) (.sql-execute sql pms))) )

(defprotocol Transactable
  (execWith [_ func] )
  (begin [_] )
  (commit [_ conn] )
  (rollback [_ conn] ))

(defprotocol SQLr
  (findSome [_  model filters ordering] [_   model filters]  )
  (findAll [_ model ordering] [_ model] )
  (findOne [_  model filters] )
  (update [_  obj] )
  (delete [_  obj] )
  (insert [_  obj] )
  (select [_  sql params] )
  (executeWithOutput [_  sql pms] )
  (execute [_  sql pms] )
  (count* [_  model] )
  (purge [_  model] ) )

(def ^:private sqlops-eof nil)

