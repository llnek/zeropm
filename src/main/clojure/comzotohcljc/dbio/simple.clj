(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.simple )

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(require '[comzotohcljc.util.ioutils :as IO])
(require '[comzotohcljc.dbio.dbutils :as DU])

(use '[comzotohcljc.dbio.sqlops])

(import '(java.util GregorianCalendar TimeZone))
(import '(java.sql Types))
(import '(java.math BigDecimal BigInteger))
(import '(java.sql Date Timestamp Blob Clob))
(import '(java.io Reader InputStream))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype SimpleSQLr [_db _metaCache _proc] SQLr

  (findAll [this model ordering] (findSome this model {} ordering))
  (findAll [this model] (findAll this model ""))

  (findOne [this model filters]
    (let [ rset (findSome this model filters "") ]
      (if (empty? rset) nil (first rset))))

  (findSome [this  model filters] (findSome this model filters ""))
  (findSome [this model filters ordering]
    (let [ conn (.open _db) ]
      (try
        (let [ zm (get _metaCache model)
               tbl (table-name zm)
               s (str "SELECT * FROM " (ese tbl))
               [wc pms] (sql-filter-clause filters)
               extra (if (SU/hgl? ordering) (str " ORDER BY " ordering) "") ]
          (if (SU/hgl? wc)
            (.doQuery _proc conn (str s " WHERE " wc extra) pms model)
            (.doQuery _proc conn (str s extra) [] model)))
        (finally
          (.close _db conn)))))

  (update [this obj]
    (let [ conn (.open _db) ]
      (try
        (.setAutoCommit conn true)
        (.doUpdate _proc conn obj)
        (finally (.close _db conn)))))

  (delete [this obj]
    (let [ conn (.open _db) ]
      (try
        (.setAutoCommit conn true)
        (.doDelete _proc conn obj)
        (finally (.close _db conn)))))

  (insert [this obj]
    (let [ conn (.open _db) ]
      (try
        (.setAutoCommit conn true)
        (.doInsert _proc conn obj)
        (finally (.close _db conn)))))

  (select [this sql params]
    (let [ conn (.open _db) ]
      (try
        (.doQuery _proc conn sql params)
      (finally (.close _db conn)))))

  (executeWithOutput [this sql pms]
    (let [ conn (.open _db) ]
      (try
        (.setAutoCommit conn true)
        (.doExecuteWithOutput _proc conn sql pms)
      (finally (.close _db conn)))))

  (execute [this sql pms]
    (let [ conn (.open _db) ]
      (try
        (.setAutoCommit conn true)
        (doExecute _proc conn sql pms)
      (finally (.close _db conn)))))

  (count* [this model]
    (let [ conn (.open _db) ]
      (try
        (.doCount _proc conn model)
      (finally (.close _db conn)))))

  (purge [this model]
    (let [ conn (.open _db) ]
      (try
        (.doPurge _proc conn model)
      (finally (.close _db conn)))))   )

(def ^:private simple-eof nil)

