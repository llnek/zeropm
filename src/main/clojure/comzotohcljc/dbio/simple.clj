(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.simple )

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

  (update [this obj] 
    (let [ conn (.open _db) ]
      (try
        (.setAutoCommit conn true)
        (.doUpdate conn _proc obj)
        (finally (.close _db conn)))))

  (delete [this obj]
    (let [ conn (.open _db) ]
      (try
        (.setAutoCommit conn true)
        (.doDelete conn _proc obj)
        (finally (.close _db conn)))))

  (insert [this obj]
    (let [ conn (.open _db) ]
      (try
        (.setAutoCommit conn true)
        (.doInsert conn _proc obj)
        (finally (.close _db conn)))))

  (select [this sql params]
    (let [ conn (.open _db) ]
      (try
        (-> (SQuery. conn sql params) (.select))
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
        (.setAutoCommit c true)
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
      (finally (.close _db conn)))))

  )


(def ^:private simple-eof nil)

