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

{
  :indexes []
  :uniques []
  :table ""
  :fields {
           :field1 { :column "" :domain :int }
           :field2 { :column "" :domain :int }
           }
  :assocs {
          :a1 { :rhs "" :kind :o2o :fkey "" }
          :a2 { :rhs "" :kind :o2m :fkey "" }
           }
}





(def ^:private dbutils-eof nil)

