(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.dbdrivers)

(defprotocol DBDriver
  (getDoubleKeyword [_] )
  (getBlobKeyword [_] )
  (getFloatKeyword [_] )
  (getStringKeyword [_] )
  (getLongKeyword [_] )
  (getIntKeyword [_] )
  (getDateKeyword [_] )
  (getTSKeyword [_] )
  (getTSDefault [_] )
  (genAutoInteger [_ table] )
  (genAutoLong [_ table] )
  (getPad [_] )
  (genBegin [_ table] )
  (genEnd [_ table] )
  (genEndSQL [_] )
  (genDrop [_ table] )
  (genExec [_] )
  )

(deftype H2 [] DBDriver
  (getDateKeyword [_] "TIMESTAMP")
  (getDoubleKeyword [_] "DOUBLE")
  (getBlobKeyword [_] "BLOB")
  (getFloatKeyword [_] "FLOAT")
  (genAutoInteger [_ table fld]
    (str (getPad) (:column fld) " " (getIntKeyword)
              (if (:pkey fld) " IDENTITY(1) " " AUTO_INCREMENT(1) ")))
  (genAutoLong [_ table fld]
    (str (getPad) (:column fld) " " (getLongKeyword)
              (if (:pkey fld) " IDENTITY(1) " " AUTO_INCREMENT(1) ")))
  (genBegin [_ table]
    (str "CREATE CACHED TABLE " table "\n(\n" ))
  (genDrop [_ table]
    (str "DROP TABLE " table " IF EXISTS CASCADE" (genExec) "\n\n")))

(deftype MySQL [] DBDriver
  (getBlobKeyword [_] "LONGBLOB")
  (getTSKeyword [_] "TIMESTAMP")
  (getDoubleKeyword [_] "DOUBLE")
  (getFloatKeyword [_]  "DOUBLE")

  (genEnd [_ table]
    (str "\n) Type=InnoDB" (gendb-exec) "\n\n"))
  (genAutoInteger[ _ table fld]
    (str (getPad) (:column fld) " " (getIntKeyword) " NOT NULL AUTO_INCREMENT"))
  (genAutoLong[ _ table fld]
    (str (getPad) (:column fld) " " (getLongKeyword) " NOT NULL AUTO_INCREMENT"))

  (genDrop [_ table]
    (str "DROP TABLE IF EXISTS " table (genExec) "\n\n")))


(deftype Oracle [] DBDriver
  (getStringKeyword [_] "VARCHAR2")
  (getTSDefault [_] "DEFAULT SYSTIMESTAMP")
  (getLongKeyword [_] "NUMBER(38)")
  (getDoubleKeyword [_] "BINARY_DOUBLE")
  (getFloatKeyword [_] "BINARY_FLOAT")
  (genAutoInteger [_ table fld]
    ;;_ids.put(table, fld)
    (genInteger fld))
  (genAutoLong [_ table fld]
    ;;_ids.put(table, fld)
    (genLong fld))
  (genEndSQL [_]
    val sql = _ids.foldLeft(new StringBuilder ) { (b, t) =>
      b.append( create_sequence(t._1))
      b.append( create_sequence_trigger(t._1, t._2.getId))
      b
    }
    sql.toString


  (genDrop [_ table]
    (str "DROP TABLE " table " CASCADE CONSTRAINTS PURGE" (genExec) "\n\n")))

(deftype PostgeSQL [] DBDriver
  (genDrop [_ table]
    (-> (doto (StringBuilder.)
            (.append "DROP TABLE IF EXISTS ")
            (.append table)
            (.append " CASCADE")
            (.append (gendb-exec))
            (.append "\n\n"))
      (.toString))))

(deftype SQLServer [] DBDriver
  (genDrop [_ table]
    (-> (doto (StringBuilder.)
            (.append "IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id=object_id('")
            (.append table)
            (.append "')) DROP TABLE ")
            (.append table)
            (.append (gendb-exec))
            (.append "\n\n"))
      (.toString))))



(defn get-ddl ^{ :doc "" }
  [cache]
  (let [ drops (StringBuilder.)
         body (StringBuilder.)
         ms (.getMetas cache) ]
    (doseq [ en (seq ms) ]
      (let [ m (last en) id (:id m) tbl (:table m)
             table (if (SU/hgl? tbl) tbl (name id)) ]
        (-> drops (.append (gendb-drop (.toUpperCase table))))
        (-> body (.append (gendb-table m)))))
    (str "" drops body (gendb-endsql))))












(def ^:private dbdrivers-eof nil)

