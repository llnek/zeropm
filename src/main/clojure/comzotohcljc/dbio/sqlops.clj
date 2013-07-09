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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *GMT-CAL* (GregorianCalendar. (TimeZone/getTimeZone "GMT")) )
(defrecord JDBCInfo [driver url user pwdObj] )

(defn- doUpdate [obj cols]
    val (all,none) = if (updates.size==0) (false,true) else (updates.head=="*",false)
    if (none) { return 0 }
    val obj= pojo.asInstanceOf[AbstractModel]
    val ds= obj.getDirtyFields
    val lst= mutable.ArrayBuffer[Any]()
    val sb1= new StringBuilder(1024)
    val cz= throwNoCZMeta(obj.getClass)
    val flds= cz.getFldMetas
    val cver= pojo.getVerID
    val nver= cver+1
    val lock= getDB().supportsOptimisticLock()
    val cols= updates.map( _.toUpperCase )

    obj.setLastModified(nowJTS )
    ds.filter( all || cols.contains(_) ).foreach { (dn) =>
      val go= flds.get(dn) match {
        case Some(fld) =>
          if ( ! fld.isUpdatable|| fld.isAutoGen ) false else true
        case _ => true
      }
      if (go) {
        addAndDelim(sb1, ",", dn)
        obj.get(dn) match {
          case Some(Nichts.NICHTS) | None => sb1.append("=NULL")
          case Some(v) =>
            sb1.append("=?")
            lst += v
        }
      }
    }

    if (sb1.length > 0) {
      addAndDelim(sb1, ",", obj.dbio_getLastModified_column.uc).append("=?")
      lst += obj.getLastModified
      if (lock) {
        addAndDelim(sb1, ",", COL_VERID).append("=?")
        lst += nver
      }
      // for where clause
      lst += obj.getRowID
      if (lock) { lst += cver }
      val tbl= cz.getTable.uc
      val cnt= execute("UPDATE " + tbl + " SET " + sb1 + " WHERE " +
        fmtUpdateWhere(lock) , lst:_*)
      if (lock) {
        lockError(cnt, tbl, obj.getRowID )
      }
      cnt
    } else {
      0
    }
  }


(deftype SimpleSQLr [db]
  (execute [this]
    (throw (UnsupportedOperationException. "no transactions.")) )
  (update [this obj cols]
    (let [ rc (doUpdate obj cols) ]
      (reset this obj)
          rc))
  (delete [this obj]
    (let [ rc (doDelete obj) ]
      (reset this obj)
      rc))
  (insert [this obj]
    (let [ rc (doInsert obj)  ]
      (reset this obj)
      rc))
  (select [this sql pms]
    (let [ c (.open db) ]
      (try
        (-> (SQuery. c sql pms) (.select))
      (finally (.close db c)))))
  (executeWithOutput [this sql pms]
    (let [ c (.open db) ]
      (try
        (.setAutoCommit c true)
        (doExecuteWithOutput c sql pms)
      (finally (.close db c)))))
  (execute [this sql pms]
    (let [ c (.open db) ]
      (try
        (.setAutoCommit c true)
        (doExecute c sql pms)
      (finally (.close db c)))))
  (count* [this model] (doCount model))
  (count* [ this sql]
    (let [ c (.open db) ]
      (try
        (let [ rc (-> (SQuery. c sql) (.select)) ]
          (if (empty? rc) 0 (first rc)))
      (finally (.close db c)))))
  (purge [this model] (doPurge model))
  (reset [this obj]
    ;;obj.asInstanceOf[AbstractModel].commit()
  )
)

(deftype Transaction [conn db pojos]
  (insert [this obj]
    (let [ rc (doInsert obj) ]
      (rego this obj)
      rc))

  (select [this sql pms]
    (> (SQuery. conn sql pms) (.select)))

  (executeWithOutput [this sql pms]
    (doExecuteWithOutput conn sql pms))

  (execute [this sql pms]
    (doExecute conn sql pms))

  (delete [this obj]
    (let [ rc  (doDelete obj) ]
      (rego this obj)
      rc))

  (update [this obj cols]
    (let [ rc (doUpdate obj cols) ]
      (rego this obj)
      rc))

  (count* [this sql]
    (let [ rc  (-> (SQuery. conn sql) (.select)) ]
      (if (empty? rc) 0 (first rc))))

  (purge [this sql]
    (execute this sql))

  (rego [this obj] (reset! pojo (conj @pojos obj)))

  (reset [this]
    ;;_items.foreach(_.asInstanceOf[AbstractModel].commit() )
    ;;_items.clear
    nil
  )

)

(deftype CompositeSQLr [db]

  (execWith [this func]
    (let [ c (begin this) tx (Transaction. c db) rc (atom nil) ]
      (try
        (reset! rc (apply func tx))
        (commit this c)
        (.reset tx)
        @rc
        (catch Throwable e#
          (do (rollback this c) (warn e#) (throw e#)))
        (finally (close this c)))))

  (rollback [this c] (CU/TryC (.rollback c)))
  (commit [this c] (.commit c))
  (begin [this]
    (doto (.open db)
      (.setAutoCommit false)))
  (close [this c] (CU/TryC (.close c)))

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




(defn- uc-ent [ent] (.toUpperCase (name ent)))
(defn- ese
  ([ent] (uc-ent ent))
  ([ch ent] (str ch (uc-ent ent) ch))
  ([c1 ent c2] (str c1 (uc-ent ent) c2)))

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

(defn- build-stmt [conn sql params]
  (let [ ps (if (insert? sql)
              (.prepareStatement c sql Statement/RETURN_GENERATED_KEYS)
              (.prepareStatement c sql)) ]
    (debug "SQL: {}" sql)
    (doseq [n (seq (range 0 (.size params))) ]
      (setBindVar ps (inc n) (nth params n)))))

(deftype SQuery [db metaCache] SQueryAPI

  (select [sql pms]
    (with-open [ stmt (build-stmt sql pms) ]
      (with-open [ rs (.executeQuery stmt) ]
        (let [ rsmeta (.getMetaData rs) ]
          (loop [ sum [] ok (.next rs) ]
            (if (not ok)
              sum
              (recur (conj sum (row2obj rs rsmeta)) (.next rs))))))))

  (findAll [model ordering] (findSome model {} ordering))

  (findOne [model filters]
    (let [ rset (findSome model filters ordering) ]
      (if (empty? rset) nil (first rset))))

  (findSome [model filters ordering]
    (let [ zm (get metaCache model)
           tbl (ese (:table zm))
           s (str "SELECT * FROM " tbl)
           [wc pms] (to-filter-clause filters)
           extra (if (SU/hgl? ordering) (str " ORDER BY " ordering) "") ]
      (if (SU/hgl? wc)
        (select (str s " WHERE " wc extra) pms)
        (select (str s extra) []))))
  )





(def ^:private sqlops-eof nil)

