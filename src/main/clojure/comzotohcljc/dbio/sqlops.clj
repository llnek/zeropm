(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.sqlops )

(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(require '[comzotohcljc.util.ioutils :as IO])
(import '(java.util GregorianCalendar TimeZone))
(import '(java.sql Types))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *GMT-CAL*
    (GregorianCalendar. (TimeZone/getTimeZone "GMT")) )


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


(defn- row-built! [row]


  )

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

