(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.wflow.switch )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol SwitchPoint)
(defprotocol Switch)

(defprotocol SwitchChoiceExpr
  (evaluate [_ job] ))

(defn make-switch [choiceExpr]
  (let [ a (make-Activity Switch) ]
    (.setf a :test choiceExpr)
    (.setf a :default nil)
    (.setf a :choices {} )
    a))

(defmethod ac-reify :Switch [ac cur]
  (let [ pipe (get (meta cur) :pipeline)
         f (make-FlowPoint pipe SwitchPoint) ]
    (fw-configure! f ac cur)
    (fw-realize! f)))

(defmethod ac-realize! :Switch [ac cur]
  (let [ cs (.getf ac :choices)
         df (.getf cur :default)
         np (.getf cur :next)
         t  (reduce (fn [sum en]
                      (assoc sum (first en) (ac-reify (last en) np)) )
                    {} (seq cs)) ]
    (.setf cur :choices t)
    (when-not (nil? df)
      (.setf cur :default (ac-reify df np)))
    (.setf cur :test (.getf ac :test))
    cur))


(defmethod fw-evaluate! :SwitchPoint [fw job]
  (let [ cs (.getf fw :choices)
         df (.getf fw :default)
         c (.getf fw :attmt)
         e (.getf fw :test) ]
    (.setf fw :attmt nil)
    (let [ h (get cs (.eval e job))
           x (if (nil? h) df h) ]
      (when-not (nil? x)
        (.setf x :attmt c))
      (fw-realize! fw)
      x)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(def ^:private switch-eof nil)


