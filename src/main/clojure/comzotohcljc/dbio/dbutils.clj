(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.dbutils )

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[clojure.set])

(import '(java.sql DatabaseMetaData Connection Driver DriverManager))
(import '(java.util GregorianCalendar TimeZone Properties))
(import '(java.lang Math))
(import '(com.zotoh.frwk.dbio DBIOError))
(import '(com.jolbox.bonecp BoneCP BoneCPConfig))

(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.metautils :as MU])
(require '[comzotohcljc.util.strutils :as SU])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *GMT-CAL* (GregorianCalendar. (TimeZone/getTimeZone "GMT")) )
(defrecord JDBCInfo [driver url user pwdObj] )

(def ^:dynamic *DBTYPES* {
    :sqlserver { :test-string "select count(*) from sysusers" }
    :postgresql { :test-string "select 1" }
    :mysql { :test-string "select version()" }
    :h2  { :test-string "select 1" }
    :oracle { :test-string "select 1 from DUAL" }
  })

(defn- maybeGetVendor [product]
  (let [ lp (.toLowerCase product) ]
    (cond
      (SU/has-nocase? lp "microsoft") :sqlserver
      (SU/has-nocase? lp "postgres") :postgresql
      (SU/has-nocase? lp "h2") :h2
      (SU/has-nocase? lp "oracle") :oracle
      (SU/has-nocase? lp "mysql") :mysql
      :else (throw (DBIOError. (str "Unknown db product " product))))))

(defn match-dbtype ^{ :doc "" }
  [dbtype]
  (*DBTYPES* (keyword (.toLowerCase dbtype))))

(defn match-jdbc-url ^{ :doc "" }
  [url]
  (let [ ss (seq (.split url ":")) ]
    (if (> 1 (.size ss))
      (match-dbtype (nth ss 1))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def BASEMODEL-MONIKER :dbio-basemodel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-vendor [jdbc] nil)

(defn make-model [nm]
  {
    :parent nil
    :id (keyword nm)
    :table nm
    :abstract false
    :system false
    :indexes {}
    :uniques {}
    :fields {}
    :assocs {} })

(defmacro defmodel ^{ :doc "" }
  ([model-name & body]
     `(let [ p#  (-> (make-model ~(name model-name))
                   ~@body) ]
      (def ~model-name  p#))))


(defn with-db-parent-model [pojo par]
  (assoc pojo :parent par))

(defn with-db-table-name [pojo tablename]
  (assoc pojo :table tablename))

(defn with-db-indexes [pojo indices]
  (let [ a (:indexes pojo) ]
    (assoc pojo :indexes (merge a indices))))

(defn with-db-uniques [pojo uniqs]
  (let [ a (:uniques pojo) ]
    (assoc pojo :uniques (merge a uniqs))))

(defn with-db-field [pojo fid fdef]
  (let [ dft { :column (name fid)
               :size 255
               :domain :string
               :assoc-key false
               :pkey false
               :null true
               :auto false
               :dft false
               :dft-value ""
               :updatable true
               :system false
               :index "" }
         fd (assoc (merge dft fdef) :id fid)
         fm (:fields pojo)
         nm (assoc fm fid fd) ]
    (assoc pojo :fields nm)))

(defn with-db-fields [pojo flddefs]
  (let [ rcmap (atom pojo) ]
    (doseq [ en (seq flddefs) ]
      (reset! rcmap (with-db-field @rcmap (first en) (last en))))
    @rcmap))

(defn with-db-assoc [pojo aid adef]
  (let [ dft { :kind nil :rhs nil :fkey "" :singly false }
         ad (merge dft adef)
         am (:assocs pojo)
         nm (assoc am aid ad) ]
    (assoc pojo :assocs nm)))

(defn with-db-assocs [pojo assocs]
  (let [ rcmap (atom pojo) ]
    (doseq [ en (seq assocs) ]
      (reset! rcmap (with-db-assoc @rcmap (first en) (last en))))
    @rcmap))

(defn with-db-abstract [pojo] (assoc pojo :abstract true))

(defn- with-db-system [pojo] (assoc pojo :system true))

(defn- nested-merge [src des]
  (cond
    (and (map? src)(map? des)) (merge src des)
    (and (set? src)(set? des)) (union src des)
    :else des))

(defmodel dbio-basemodel
  (with-db-abstract)
  (with-db-system)
  (with-db-fields {
    :rowid {:column "dbio_rowid" :pkey true :domain :long
            :auto true :system true :updatable false}
    :verid {:column "dbio_version" :domain :long :system true
            :default true :default-value 0}
    :last-modify {:column "dbio_lastchanged" :domain :timestamp
               :system true :default true}
    :created-on {:column "dbio_created_on" :domain :timestamp
                  :system true :default true :updatable false}
    :created-by {:column "dbio_created_by" :system true :domain :string } }))
  
(defprotocol MetaCacheAPI (getMetas [_] ))
(defprotocol SchemaAPI (getModels [_] ))
(deftype Schema [theModels]
  SchemaAPI
  (getModels [_] theModels))

(defn- resolve-local-assoc [ms zm]
  (let [ socs (:assocs zm) zid (:id zm) rc (atom #{}) ]
    (if (or (nil? socs) (empty? socs))
      @rc
      (do
        (doseq [ en (seq socs) ]
          (let [ soc (last en) id (first en)
               kind (:kind soc) rhs (:rhs soc)
               col (case kind
                  :o2m (str (name rhs) "|" "fk_" (name zid) "_" (name id))
                  :o2o (str (name zid) "|" "fk_" (name rhs) "_" (name id))
                  :m2m (if (nil? (get ms (:joined soc)))
                              (throw (DBIOError. (str "Missing joined model for m2m assoc " id)))
                         "")
                  (throw (DBIOError. (str "Invalid assoc type " kind)))) ]
            (when (SU/hgl? col) (reset! rc (conj @rc col)))))
        @rc))))

(defn- resolve-assoc [ms m]
  (let [ par (:parent m) ]
    (if (nil? par)
      (union #{} (resolve-local-assoc ms m))
      (union #{} (resolve-local-assoc ms m) (resolve-assoc ms (get ms par))))))

(defn- resolve-assocs [ms]
  (let [ rc (atom #{} ) ]
    (doseq [ en (seq ms) ]
      (reset! rc (union @rc (resolve-assoc ms (last en)) )))
    @rc))

(defn- inject-fkeys-models [ms fks]
  (let [ rc (atom (merge {} ms)) ]
    (doseq [ k (seq fks) ]
      (let [ ss (.split k "\\|") id (keyword (nth ss 0)) fid (keyword (nth ss 1))
             pojo (get ms id) ]
        (reset! rc (assoc @rc id (with-db-field pojo fid { :domain :long :assoc-key true } )))))
    @rc))

(defn- resolve-parent [ms m]
  (let [ par (:parent m) ]
    (cond
      (keyword? par) (if (nil? (get ms par))
                       (throw (DBIOError. (str "Unknown model " par)))
                       m)
      (nil? par) (assoc m :parent BASEMODEL-MONIKER)
      :else (throw (DBIOError. (str "Invalid parent " par))))))

(defn- resolve-parents [ms]
  (reduce (fn [sum en]
            (let [ rc (resolve-parent ms (last en)) ]
              (assoc sum (:id rc) rc)))
            {} (seq ms)))

(defn- mapize-models [ms]
  (reduce (fn [sum n] (assoc sum (:id n) n)) {} (seq ms)))

(defn- collect-db-xxx-filter [a b]
  (cond
    (keyword? b) :keyword
    (map? b) :map
    :else (throw (DBIOError. (str "Invalid arg " b)))))

(defmulti collect-db-fields collect-db-xxx-filter)
(defmethod collect-db-fields :keyword [cache modelid]
  (collect-db-fields cache (get cache modelid)))
(defmethod collect-db-fields :map [cache zm]
  (let [ par (:parent zm) ]
    (if (nil? par)
      (merge {} (:fields zm))
      (merge {} (:fields zm) (collect-db-fields cache par)))))

(defmulti collect-db-indexes collect-db-xxx-filter)
(defmethod collect-db-indexes :keyword [cache modelid]
  (collect-db-indexes cache (get cache modelid)))
(defmethod collect-db-indexes :map [cache zm]
  (let [ par (:parent zm) ]
    (if (nil? par)
      (merge {} (:indexes zm))
      (merge {} (:indexes zm) (collect-db-indexes cache par)))))

(defmulti collect-db-uniques collect-db-xxx-filter)
(defmethod collect-db-uniques :keyword [cache modelid]
  (collect-db-uniques cache (get cache modelid)))
(defmethod collect-db-uniques :map [cache zm]
  (let [ par (:parent zm) ]
    (if (nil? par)
      (merge {} (:uniques zm))
      (merge {} (:uniques zm) (collect-db-uniques cache par)))))

(defn- colmap-fields [flds]
  (let [ sum (atom {}) ]
    (doseq [ [k v] (seq flds) ]
      (let [ cn (.toUpperCase (:column v)) ]
        (reset! sum (assoc @sum cn v))))
    @sum))

(defn- meta-models [cache]
  (let [ sum (atom {}) ]
    (doseq [ [k m] (seq cache) ]
      (let [ flds (collect-db-fields cache m)
             cols (colmap-fields flds) ]
        (reset! sum (assoc @sum k (with-meta m { :columns cols :fields flds } ) ))))
    @sum))

(defn make-MetaCache ^{ :doc "" }
  [schema]
  (let [ ms (if (nil? schema) {} (mapize-models (.getModels schema)))
         m1 (if (empty? ms) {} (resolve-parents ms))
         m2 (assoc m1 BASEMODEL-MONIKER dbio-basemodel)
         m3 (inject-fkeys-models m2 (resolve-assocs m2))
         m4 (meta-models m3) ]
    (reify MetaCacheAPI
      (getMetas [_] m4))))



(defmodel address
  (with-db-fields {
    :addr1 { :size 200 :null false }
    :addr2 { :size 64}
    :city { :null false}
    :state {:null false}
    :zip {:null false}
    :country {:null false}
                   })
  (with-db-indexes { :i1 #{ :city :state :country }
    :i2 #{ :zip :country }
    :state #{ :state }
    :zip #{ :zip } } ))

(defmodel person
  (with-db-abstract)
  (with-db-fields {
    :fname { :null false }
    :lname { :null false }
    :age { :domain :int }
    :pic { :domain :bytes }
                   })
  (with-db-assocs {
    :addr { :kind :o2m :singly true :rhs :address }
    :spouse { :kind :o2o :rhs :person }
    :accts { :kind :o2m :rhs :bankacct }
                   })
  (with-db-indexes { :i1 #{ :age } })
  (with-db-uniques { :u2 #{ :fname :lname } }))

(defmodel president
  (with-db-parent-model :person))

(defmodel bankacct
  (with-db-fields {
    :acctid { :null false }
    :amount { :null false :domain :double }
                   })
  (with-db-uniques { :u2 #{ :acctid } }))


(def testschema (Schema. [ president address person bankacct ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- safeGetConn [jdbc]
  (let [ d (if (SU/hgl? (:url jdbc))
               (DriverManager/getDriver (:url jdbc))
               nil)
         p (if (SU/hgl? (:user jdbc))
               (doto (Properties.) (.put "password" (SU/nsb (:pwdObj jdbc)))
                                   (.put "user" (:user jdbc)) 
                                   (.put "username" (:user jdbc)))
               (Properties.)) ] 
    (when (nil? d)
      (throw (DBIOError. (str "Can't load Jdbc Url: " (:url jdbc)))))
    (when
      (and (SU/hgl? (:driver jdbc))
           (not= (-> d (.getClass) (.getName)) (:driver jdbc)))
        (warn "Expected " (:driver jdbc) ", loaded with driver: " (.getClass d)))
    (.connect d (:url jdbc) p)))

(defn make-connection ^{ :doc "" }
  [jdbc]
  (let [ conn (if (SU/hgl? (:user jdbc))
                (safeGetConn jdbc)
                (DriverManager/getConnection (:url jdbc))) ]
    (when (nil? conn)
      (throw (DBIOError. (str "Failed to create db connection: " (:url jdbc)))))
    (doto conn
      (.setTransactionIsolation  Connection/TRANSACTION_READ_COMMITTED))))

(defn test-connection ^{ :doc "" }
  [jdbc]
  (CU/TryC (.close (make-connection jdbc))))

(defmulti resolve-vendor class)

(defmethod ^{ :doc "" } resolve-vendor JDBCInfo
  [jdbc]
  (with-open [ conn (make-connection jdbc) ]
    (resolve-vendor conn)))

(defmethod ^{ :doc "" } resolve-vendor Connection
  [conn]
  (let [ md (.getMetaData conn) ]
    (-> { :id (maybeGetVendor (.getDatabaseProductName md)) }
      (assoc :version (.getDatabaseProductVersion md))
      (assoc :name (.getDatabaseProductName md))
      (assoc :quote-string (.getIdentifierQuoteString md))
      (assoc :url (.getURL md))
      (assoc :user (.getUserName md))
      (assoc :lcis (.storesLowerCaseIdentifiers md))
      (assoc :ucis (.storesUpperCaseIdentifiers md))
      (assoc :mcis (.storesMixedCaseIdentifiers md)))))


(defmulti ^{ :doc "" } table-exist? (fn [a b] (class a)))

(defmethod table-exist? JDBCInfo [jdbc table]
  (with-open [ conn (make-connection jdbc) ]
    (table-exist? conn table)))

(defmethod table-exist? Connection [conn table]
  (let [ rc (atom false) ]
    (try
      (let [ mt (.getMetaData conn)
             tbl (cond
                    (.storesUpperCaseIdentifiers mt) (.toUpperCase table)
                    (.storesLowerCaseIdentifiers mt) (.toLowerCase table)
                    :else table) ]
        (with-open [ res (.getColumns mt nil nil tbl nil) ]
          (when (and (not (nil? res)) (.next res))
            (reset! rc true))))
      (catch Throwable e# nil))
    @rc))

(defmulti ^{ :doc "" } row-exist? (fn [a b] (class a)))

(defmethod row-exist? JDBCInfo [jdbc table]
  (with-open [ conn (make-connection jdbc) ]
    (row-exist? conn table)))

(defmethod row-exist? Connection [conn table]
  (let [ rc (atom false) ]
    (try
      (let [ sql (str "SELECT COUNT(*) FROM  " (.toUpperCase table)) ]
        (with-open [ stmt (.createStatement conn) ]
          (with-open [ res (.executeQuery stmt) ]
            (when (and (not (nil? res)) (.next res))
              (reset! rc (> (.getInt res (int 1)) 0))))))
      (catch Throwable e# nil))
    @rc))

(defn- load-columns [mt catalog schema table]
  (let [ pkeys (atom #{}) cms (atom {}) ]
    (with-open [ rs (.getPrimaryKeys mt catalog schema table) ]
      (loop [ sum #{} more (.next rs) ]
        (if (not more)
          (reset! pkeys sum)
          (recur
            (conj sum (.toUpperCase (.getString rs (int 4))) )
            (.next rs)))))
    (with-open [ rs (.getColumns mt catalog schema table "%") ]
      (loop [ sum {} more (.next rs) ]
        (if (not more)
          (reset! cms sum)
          (let [ opt (not= (.getInt rs (int 11)) DatabaseMetaData/columnNoNulls)
                 cn (.toUpperCase (.getString rs (int 4)))
                 ctype (.getInt rs (int 5)) ]
            (recur
              (assoc sum (keyword cn) 
                  { :column cn :sql-type ctype :null opt 
                    :pkey (clojure.core/contains? @pkeys cn) })
              (.next rs))))))

    (with-meta @cms { :supportsGetGeneratedKeys (.supportsGetGeneratedKeys mt)
                      :supportsTransactions (.supportsTransactions mt) } )))

(defn load-table-meta ^{ :doc "" }
  [conn table]
  (let [ mt (.getMetaData conn) dbv (resolve-vendor conn) 
         catalog nil
         schema (if (= (:id dbv) :oracle) "%" nil)
         tbl (cond
                (.storesUpperCaseIdentifiers mt) (.toUpperCase table)
                (.storesLowerCaseIdentifiers mt) (.toLowerCase table)
                :else table) ]
    ;; not good, try mixed case... arrrrrrrrrrhhhhhhhhhhhhhh
    ;;rs = m.getTables( catalog, schema, "%", null)
    (load-columns mt catalog schema tbl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol JDBCPoolAPI
  (shutdown [_] )
  (next-free [_] ))

(deftype JDBCPool [_jdbc _impl] JDBCPoolAPI

  (shutdown [_] (.shutdown _impl))
  (next-free  [_]
    (try
      (.getConnection _impl)
      (catch Throwable e#
        (throw (DBIOError. (str "No free connection."))))))

)

(defn make-db-pool ^{ :doc "" }
  ([jdbc] (make-db-pool jdbc {}))
  ([jdbc options]
    (let [ bcf (BoneCPConfig.) ]
      (debug "Driver : " (:driver jdbc))
      (debug "URL : "  (:url jdbc))
      (when (SU/hgl? (:driver jdbc)) (MU/for-name (:driver jdbc)))
      (doto bcf
        (.setPartitionCount (Math/max 1 (CU/nnz (:partitions options))))
        (.setLogStatementsEnabled (CU/nbf (:debug options)))
        (.setPassword (SU/nsb (:pwdObj jdbc)))
        (.setJdbcUrl (:url jdbc))
        (.setUsername (:user jdbc))
        (.setIdleMaxAgeInSeconds (* 60 60 24)) ;; 1 day
        (.setMaxConnectionsPerPartition (Math/max 2 (CU/nnz (:max-conns options))))
        (.setMinConnectionsPerPartition (Math/max 1 (CU/nnz (:min-conns options))))
        (.setPoolName (CU/uid))
        (.setAcquireRetryDelayInMs 5000)
        (.setConnectionTimeoutInMs  (Math/max 5000 (CU/nnz (:max-conn-wait options))))
        (.setDefaultAutoCommit false)
        (.setAcquireRetryAttempts 1))
      (JDBCPool. jdbc (BoneCP. bcf))))  )



(def ^:private dbutils-eof nil)

