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

(defn- fmtUpdateWhere [lock zm]
  (let [ s1 (str (ese (col-name :rowid zm)) "=?") ]
    (if lock
      (str s1 " AND " (ese (col-name :verid zm)) "=?")
      s1)))

(defn- lockError [opcode cnt table rowID]
  (when (= cnt 0)
    (throw (OptLockError. opcode table rowID))))

(deftype SQLProc [_db _metaCache _implr]

  (count* [_ model]
    (.doCount implr (str "SELECT COUNT(*) FROM " (ese (:table model)))) )

  (purge [_ model]
    (.doPurge implr (str "DELETE FROM " (ese (:table model)))))

  (doDelete [this obj]
    (let [ info (meta obj) model (:typeid info) zm (get _metaCache model)
           lock (.supportsOptimisticLock _db)
           table (table-name zm)
           rowid (:rowid info)
           verid (:verid info)
           p (if lock [rowid verid] [rowid] )
           w (fmtUpdateWhere lock zm)
           cnt (doExecute this (str "DELETE FROM " table " WHERE " w p)) ]
      (when lock (lockError cnt tbl rowid))
      cnt))

  (doInsert [this obj]
    (let [ info (meta obj) model (:typeid info) zm (get _metaCache model)
           lock (.supportsOptimisticLock _db)
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
            (when (> (.length s2) 0) (.append s2 ","))
            (if (nil? v)
              (.append s2 "NULL")
              (do
                (.append s2 "?")
                (reset! pms (conj @pms v)))))))

      (if (= (.length s1) 0)
        nil
        (let [ [rc out] (doExecuteWithOutput this
                  (str "INSERT INTO " (ese table) "(" s1 ") VALUES (" s2 ")" ) @pms) ]
          (when (empty? out)
            (throw (SQLException. (str "Insert requires row-id to be returned."))))
          (let [ rowid (first out) verid 0  wm { :rowid rowid :verid verid } ]
            (when-not (instance? Long rowid)
              (throw (SQLException. (str "RowID data-type must be Long."))))
            (vary-meta obj (fn [m1 m2] (merge m1 m2)) wm))))
      ))

  (doUpdate [this obj]
    (let [ info (meta obj) model (:typeid info) zm (get _metaCache model)
           flds (if (nil? zm) {} (:fields zm))
           table (table-name zm)
           rowid (:rowid info)
           sb1 (StringBuilder.)
           cver (CU/nnz (:verid info))
           nver (inc cver)
           pms (atom [])
           now (CU/now-jtstamp)
           lock (.supportsOptimisticLock _db) ]

      (doseq [ [k v] (seq obj) ]
        (let [ fdef (get flds k) cn (col-name fdef) ]
          (when (and (not (nil? fdef))
                     (:updatable fdef)
                     (not (:auto fdef)) (not (:system fdef)) )
            (SU/add-delim! sb1 "," (ese cn))
            (if (nil? v)
              (.append sb1 "=NULL")
              (do
                (.append sb1 "=?")
                (reset! pms (conj @pms v)))))))

      (if (= (.length sb1) 0)
        nil
        (do
          (SU/add-delim! sb1 "," (ese (col-name :last-modify zm)))
          (.append db1 "=?")
          (reset! pms (conj @pms now))
          ;; update to new version (+1)
          (when lock
            (SU/add-delim! sb1 "," (ese (col-name :verid zm)))
            (.append db1 "=?")
            (reset! pms (conj @pms nver)))
          ;; for the where clause
          (reset! pms (conj @pms rowid))
          (when lock
            (reset! pms (conj @pms cver)))
          (let [ cnt (doExecute this (str "UPDATE " tbl " SET " sb1 " WHERE "
                                  (fmtUpdateWhere lock zm) ) pms) ]
            (when lock (lockError cnt tbl rowid))
            (vary-meta obj (fn [m1 m2] (merge m1 m2))
               { :verid nver :last-modify now })
            )))
      ))


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

