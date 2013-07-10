(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.composite )

(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(require '[comzotohcljc.util.ioutils :as IO])

(use '[comzotohcljc.dbio.dbutils :as DU])
(use '[comzotohcljc.dbio.sqlops])

(import '(java.util GregorianCalendar TimeZone))
(import '(java.sql Types))
(import '(java.math BigDecimal BigInteger))
(import '(java.sql Date Timestamp Blob Clob))
(import '(java.io Reader InputStream))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Transaction [_db _metaCache _proc _conn] SQLr

  (insert [this obj] (.doInsert _proc _conn obj))

  (select [this sql pms]
    (-> (SQuery. _conn sql pms) (.select)))

  (executeWithOutput [this sql pms]
    (.doExecuteWithOutput _proc _conn sql pms))

  (execute [this sql pms]
    (.doExecute _proc _conn sql pms))

  (delete [this obj] (.doDelete _proc _conn obj))

  (update [this obj] (.doUpdate _proc _conn obj))

  (count* [this model] (.doCount _proc _conn model))

  (purge [this model] (.doPurge _proc _conn model))

)

(deftype CompositeSQLr [_db _metaCache]

  (execWith [this func]
    (let [ proc (SQLProc. _db _metaCache)
           conn (begin this)
           rc (atom nil)
           tx (Transaction. _db _metaCache proc conn) ]
      (try
        (reset! rc (apply func tx))
        (commit this conn)
        @rc
        (catch Throwable e#
          (do (rollback this conn) (warn e#) (throw e#)))
        (finally (.close _db conn)))))

  (rollback [this conn] (CU/TryC (.rollback conn)))

  (commit [this conn] (.commit conn))

  (begin [this]
    (doto (.open _db)
      (.setAutoCommit false)))

  )


(def ^:private composite-eof nil)

