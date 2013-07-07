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
    :last-mod {:column "dbio_lastchanged" :domain :timestamp
               :system true :default true}
    :created-on {:column "dbio_created_on" :domain :timestamp
                  :system true :default true :updatable false}
    :created-by {:column "dbio_created_by" :system true :domain :string } }))
  
(defprotocol MetaCacheAPI (getMetas [_] ))
(defprotocol SchemaAPI (getModels [_] ))
(deftype Schema [theModels]
  SchemaAPI
  (getModels [_] theModels))

(defn- jiggle-assocs [ms zm]
  (let [ flds (:fields zm) rels (:assocs zm) mid (:id zm) ]
    (reduce (fn [sum en]
              (let [ adef (last en) id (first en)
                     kind (:kind adef)
                     rhs (get ms (:rhs adef))
                     fk (:fkey adef)
                     fk2  (case kind
                          :o2m (str "fk_" (name mid))
                          :o2o (str "fk_" (name id))
                          :m2m ("")
                          (throw (DBIOError. (str "Invalid assoc type " kind))))
                     rf  (if (SU/hgl? fk) fk fk2)
                    ]
                (assoc sum id (assoc adef :fkey rf))
              ))
            {} (seq rels))))

(defn- jiggle-model [ms m]
  (let [ nsocs (jiggle-assocs ms m)
         nm (assoc m :assocs nsocs)
         nms (assoc ms (:id m) nm) ]
    nms))

(defn- jiggle-models [ms]
  (reduce (fn [sum en]
            (let [ rc (jiggle-model sum (get sum (first en))) ]
              rc))
            ms (seq ms)))

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

(defn make-MetaCache ^{ :doc "" }
  [schema]
  (let [ ms (if (nil? schema) {} (mapize-models (.getModels schema)))
         m1 (if (empty? ms) {} (resolve-parents ms))
         m2 (assoc m1 BASEMODEL-MONIKER dbio-basemodel)
        ;; m2 (if (empty? m1) {} (jiggle-models m1))
        ]
    (reify MetaCacheAPI
      (getMetas [_] m2))))




(defmodel address
  (with-db-fields {
    :addr1 { :size 200 :null false }
    :addr2 { :size 64}
    :city { :null false}
    :state {:null false}
    :zip {:null false}
    :country {:null false}
                   }))

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
                   }))

(defmodel president
  (with-db-parent-model :person))

(defmodel bankacct
  (with-db-fields {
    :amount { :null false :domain :double }
                   }))


(defmodel meta-info
  (with-db-field :f0 { :column "F0" :domain :int })
  (with-db-assoc :a0 { :rhs "meta-affa" :kind :o2m :fkey "fk_a0" })
           )
(defmodel base-info
  (with-db-table-name "BASE_INFO")
  (with-db-parent-model :meta-info)
  (with-db-indexes { :i1 #{ :f1 :f2 } })
  (with-db-uniques { :u1 #{ :f0 } })
  (with-db-field :f1 { :column "F1" :domain :int })
  (with-db-field :f2 { :column "F2" :domain :long })
  (with-db-assoc :a1 { :rhs "affa" :kind :o2m :fkey "fk_a1" })
  (with-db-assoc :a2 { :rhs "affa" :kind :o2m :fkey "fk_a2" })
           )
(defmodel tracking-info
  (with-db-table-name "TRACKING_INFO")
  (with-db-parent-model :base-info)
  (with-db-indexes { :i4 #{ :f1 :f2 } })
  (with-db-uniques { :u2 #{ :f0 } })
  (with-db-field :f1 { :column "F1" :domain :double })
  (with-db-field :f2 { :column "F3" :domain :bytes })
  (with-db-assoc :a0 { :rhs "affa" :kind :o2m :fkey "fk_a0" })
  (with-db-assoc :a2 { :rhs "poo" :kind :o2m :fkey "fk_a2" })
           )

;;(def testschema (Schema. [ base-info tracking-info ]))
(def testschema (Schema. [ president address person bankacct ]))


(def ^:private dbutils-eof nil)

