(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.dbio.sqlops )


(defprotocol SQueryAPI
  )

(deftype SQuery [] SQueryAPI)




(def ^:private sqlops-eof nil)

