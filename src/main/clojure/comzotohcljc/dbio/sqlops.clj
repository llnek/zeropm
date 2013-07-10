(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.sqlops )

(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(require '[comzotohcljc.util.ioutils :as IO])
(import '(java.util GregorianCalendar TimeZone))
(import '(java.sql Types))
(import '(java.math BigDecimal BigInteger))
(import '(java.sql Date Timestamp Blob Clob))
(import '(java.io Reader InputStream))
(import '(com.zotoh.frwk.dbio DBIOError OptLockError))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *GMT-CAL* (GregorianCalendar. (TimeZone/getTimeZone "GMT")) )
(defrecord JDBCInfo [driver url user pwdObj] )

(defn- uc-ent [ent] (.toUpperCase (name ent)))

(defn ese
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
  (let [ s1 (str (ese (col-name :rowid zm)) "=?") ]
    (if lock
      (str s1 " AND " (ese (col-name :verid zm)) "=?")
      s1)))

(defn- lockError [opcode cnt table rowID]
  (when (= cnt 0)
    (throw (OptLockError. opcode table rowID))))


(defprotocol SQLProcAPI
  (doCount [_  conn model] )
  (doPurge [_  conn model] )
  (doDelete [_  conn pojo] )
  (doInsert [_  conn pojo] )
  (doUpdate [_  conn pojo] )
)

(deftype SQLProc [_db _metaCache _implr] SQLProcAPI

  (doCount [_   conn model]
    (let [ sql (str "SELECT COUNT(*) FROM " (ese (table-name model _metaCache)))
           rc (-> (SQuery. conn sql) (.select)) ]
      (if (empty? rc) 0 (first rc))))

  (doPurge [_   conn model]
    (let [ sql (str "DELETE FROM " (ese (table-name model _metaCache))) ]
      (do (-> (SQuery. conn sql) (.execute)) nil)))

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
             flds (:fields zm)
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
          (let [ [rc out] (doExecuteWithOutput this conn
                    (str "INSERT INTO " (ese table) "(" s1 ") VALUES (" s2 ")" ) @pms) ]
            (when (empty? out)
              (throw (DBIOError. (str "Insert requires row-id to be returned."))))
            (let [ wm { :rowid (first out) :verid 0 } ]
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
             flds (:fields zm)
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
              (when lock (lockError "update" cnt tbl rowid))
              (vary-meta obj merge-meta { :verid nver :last-modify now })
              )))
      )))

  (doExecuteWithOutput [this conn sql pms]
    (let [ s (SQuery. conn sql pms) ]
      [ (.execute s) (.getOutput s) ] ))

  (doExecute [this conn sql pms]
    (-> (SQuery. conn sql pms) (.execute)))

)




(deftype SimpleDB [jdbc options] DBAPI
  (supportsOptimisticLock [_]
    (if (contains? options :opt-lock) (:opt-lock options) true))
  (vendor [_]  (DU/vendor jdbc))
  (finz [_] nil)
  (open [_] (-> x (.getPool)(.nextFree)))
  (newCompositeSQLProc [this] (CompositeSQLr. this))
  (newSimpleSQLProc [this] (SimpleSQLr. this))
)


(defprotocol SQueryAPI
  )

(defn- to-filter-clause [filters]
  (let [ wc (reduce (fn [sum en]
                (SU/add-delim! sum " AND "
                   (str (ese (first en)) (if (nil? (last en)) " IS NULL " " = ? "))))
                (StringBuilder.)
                (seq filters)) ]
    [ (SU/nsb wc) (CU/flatten-nil (vals filters)) ] ))



(defn- readCol [sqlType cn pos row rset]
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
      (not (nil? inp)) (with-open [p inp] (IO/read-bytes p))
      (not (nil? rdr)) (with-open [r rdr] (IO/read-chars r))
      :else obj)))


(defn- readOneCol [sqlType cn pos row rset]
  (let [ cv (case sqlType
                Types/TIMESTAMP (.getTimestamp rset (int pos) *GMT-CAL*)
                Types/DATE (.getDate rset (int pos) *GMT-CAL*)
                (readCol sqlType cn pos row rset)) ]
    (assoc @row (.toUpperCase cn) cv)))


(defn- row-built! [pojo row]
  (let [ rc (atom {}) ]
    (doseq [ [k v] (seq pojo) ]
      (let [ cn (.toUpperCase (:column v)) ]
        (when (contains? row cn)
          (reset! rc (assoc @rc k (get row cn))))))
    @rc))

(defn- row2obj [rs rsmeta]
  (let [ cc (.getColumnCount rsmeta)
         row (atom {})
         rr (range 1 (inc cc)) ]
    (doseq [ pos (seq rr) ]
      (reset! row (readOneCol
        (.getColumnType rsmeta (int pos))
        (.getColumnName rsmeta (int pos))
        pos
        row
        rs)))
    (row-built! @row)))


(defn- insert? [sql]
  (.startsWith (.toLowerCase (SU/trim sql)) "insert"))

(defn- setBindVar [ps pos p]
  (cond
    (instance? String p) (.setString ps pos p)
    (instance? Long p) (.setLong ps pos p)
    (instance? Int p) (.setInt ps pos p)
    (instance? Short p) (.setShort ps pos p)

    (instance? BigDecimal p) (.setBigDecimal ps pos p)
    (instance? BigInteger p) (.setBigDecimal ps pos (BigDecimal p))

    (instance? InputStream p) (.setBinaryStream ps pos p)
    (instance? Reader p) (.setCharacterStream ps pos p)
    (instance? Blob p) (.setBlob ps pos p)
    (instance? Clob p) (.setClob ps pos p)

    (instance? (CU/chars-class) p) (.setString ps pos (String. p))
    (instance? (CU/bytes-class) p) (.setBytes ps pos p)

    (instance? Boolean p) (.setInt ps pos (if p 1 0))
    (instance? Double p) (.setDouble ps pos p)
    (instance? Float p) (.setFloat ps pos p)

    (instance? Timestamp p) (.setTimestamp ps pos t *GMT-CAL*)
    (instance? Date p) (.setDate ps pos p *GMT-CAL*)
    (instance? Calendar p) (.setTimestamp ps pos (Timestamp. (.getTimeInMillis p)) *GMT-CAL*)

    :else (throw (SQLException. (str "Unsupported param type: " (type p))))))

(defn- mssql-tweak-sqlstr [sql token cmd]
  (let [ lcs (.toLowerCase sql) pos (.indexOf lcs (name token))
         [head tail] (if (< pos 0)
                       [sql ""]
                       [(.substring sql 0 pos) (.substring sql pos)]) ]
    (if (SU/hgl? sql)
      (str head " WITH (" cmd ") " tail)
      sql)))

(defn- jiggleSQL [db sqlstr]
  (let [ v (.getVendor db)  sql (SU/trim sqlstr) lcs (.toLowerCase sql) ]
    (when (= v *SQL-SERVER*)
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

(defprotocol SQueryAPI
  (executeWithOutput [_  sql pms options] )
  (select [_ sql pms] )
  (execute [_  sql pms] ) )

(deftype SQuery [_db _metaCache _conn] SQueryAPI

  (executeWithOutput [this sql pms options]
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

  (select [this sql pms]
    (with-open [ stmt (build-stmt _db _conn sql pms) ]
      (with-open [ rs (.executeQuery stmt) ]
        (let [ rsmeta (.getMetaData rs) ]
          (loop [ sum [] ok (.next rs) ]
            (if (not ok)
              sum
              (recur (conj sum (row2obj rs rsmeta)) (.next rs))))))))

  (execute [this sql pms]
    (with-open [ stmt (build-stmt _db _conn sql pms) ]
      (.executeUpdate stmt)))

)





(def ^:private sqlops-eof nil)

