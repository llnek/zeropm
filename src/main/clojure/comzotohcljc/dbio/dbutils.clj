(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.dbutils )

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[clojure.set])
(import '(com.zotoh.frwk.dbio DBIOError))
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])

(def ^:dynamic *DBTYPES* {
    :sqlserver { :test-string "select count(*) from sysusers" }
    :postgresql { :test-string "select 1" }
    :mysql { :test-string "select version()" }
    :h2  { :test-string "select 1" }
    :oracle { :test-string "select 1 from DUAL" }
  })

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
         fd (merge dft fdef)
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

(defn make-MetaCache ^{ :doc "" }
  [schema]
  (let [ ms (if (nil? schema) {} (mapize-models (.getModels schema)))
         m1 (if (empty? ms) {} (resolve-parents ms))
         m2 (assoc m1 BASEMODEL-MONIKER dbio-basemodel)
         m3 (inject-fkeys-models m2 (resolve-assocs m2))
        ]
    (reify MetaCacheAPI
      (getMetas [_] m3))))




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


(def ^:private dbutils-eof nil)

