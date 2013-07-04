(ns ^{ :doc "" 
       :author "kenl" }
  comzotohcljc.dbio.dbutils )


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

(defmodel tracking-info []
  (with-db-table-name "TRACKING_INFO")
  (with-db-indexes [ "i1" "i2" ])
  (with-db-uniques [ "u1" "u2" ])
  (with-db-field :f1 { :column "F1" :domain :int })
  (with-db-field :f2 { :column "F2" :domain :long })
  (with-db-assoc :a1 { :rhs "affa" :kind :o2m :fkey "" })
  (with-db-assoc :a2 { :rhs "affa" :kind :o2m :fkey "" })
           )

(defn with-db-table-name [pojo tablename]
  (assoc pojo :table tablename))

(defn with-db-indexes [pojo indices]
  (assoc pojo :indexes indices))

(defn with-db-uniques [pojo uniqs]
  (assoc pojo :uniques uniqs))

(defn with-db-field [pojo fid fdef]
  (let [ dft { :column (name fid)
               :size 255 
               :domain :string 
               :null true 
               :autogen false 
               :dft false 
               :dft-value "" 
               :updatable true 
               :system false 
               :index "" }
         fd (merge dft fdef)
         fm (:fields pojo)
         nm (assoc fm fid fd) ]
    (assoc pojo :fields nm)))

(defn with-db-assoc [pojo aid adef]
  (let [ dft { :kind nil :rhs: nil :fkey "" }
         ad (merge dft adef)
         am (:assocs pojo)
         nm (assoc am aid ad) ]
    (assoc pojo :assocs nm)))







(def ^:private dbutils-eof nil)

