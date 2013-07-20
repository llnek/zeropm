(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.wflow.conditionals )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ConditionalPoint)
(defprotocol Conditional)

(defprotocol IfPoint)
(defprotocol If)

(defprotocol WhilePoint)
(defprotocol While)

(defprotocol ForLoopCountExpr
  (eval [_ job] ))
(defprotocol ForPoint)
(defprotocol For)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If

(defn make-if [expr then else]
  (let [ b (make-Activity [ Conditional If ] ) ]
    (.setf b :test expr)
    (.setf b :then then)
    (.setf b :else else)
    b))

(defmethod ac-reify :If [ac cur]
  (let [ pipe (get (meta cur) :pipeline)
         f (make-FlowPoint pipe ConditionalPoint IfPoint) ]
    (fw-configure! f ac cur)
    (fw-realize! f)))

(defmethod ac-realize! :If [ac fw]
  (let [ np (.getf fw :next)
         t (.getf ac :then)
         c (.getf ac :test)
         e (.getf ac :else) ]
    (.setf fw :else (if (nil? e) np (ac-reify e np)))
    (.setf fw :then (ac-reify t np))
    (.setf fw :test c)
    fw))

(defmethod fw-evaluate! :IfPoint [fw job]
  (let [ c (.getf fw :attmt)
         t (.getf fw :then)
         e (.getf fw :else)
         b (.eval (.getf fw :test) job) ]
    (debug "if-(test) = " (if b "OK" "FALSE"))
    (let [ rc (if b t e) ]
      (when-not (nil? rc)
        (.setf rc :attmt c))
      (fw-realize! fw)
      rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; While

(defn make-while [expr body]
  (let [ b (make-Activity [ Conditional While ] ) ]
    (.setf b :test expr)
    (.setf b :then body)
    b))

(defmethod ac-reify :While [ac cur]
  (let [ pipe (get (meta cur) :pipeline)
         f (make-FlowPoint pipe ConditionalPoint WhilePoint) ]
    (fw-configure! f ac cur)
    (fw-realize! f)))

(defmethod ac-realize! :While [ac fw]
  (let [ b (.getf ac :body)
         t (.getf ac :test) ]
    (when-not (nil? b)
      (.setf fw :body (ac-reify b fw)))
    (.setf fw :test t)
    fw))

(defn- evalWhilePoint [fw job]
  (let [ c (.getf fw :attmt)
         np (.getf fw :next)
         body (.getf fw :body)
         tst (.getf fw :test) ]
    (.setf fw :attmt nil)
    (with-local-vars [ rc fw ]
      (if (not (.eval tst job))
        (do
          (debug "test-condition == false")
          (var-set rc np)
          (when-not (nil? @rc)
            (.setf @rc :attmt c))
          (fw-realize! fw))
        (do
          (debug "looping - eval body")
          (.setf body :attmt c)
          (let [ f (fw-evaluate! body job) ]
            (cond
              (or (satisfies? AsyncWaitPoint f)
                  (satisfies? DelayPoint f))
                (do (.setf f :next @rc) (var-set rc f))
              :else
              (when-not (identical? f fw)
                (.setf fw :body f))))) )
      @rc) ))

(defmethod fw-evaluate! :WhilePoint [fw job]
  (evalWhilePoint fw job))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For

(defn make-for [expr body]
  (let [ b (make-Activity [ Conditional While For ] ) ]
    (.setf b :test expr)
    (.setf b :body body)
    b))

(defmethod ac-reify :For [ac cur]
  (let [ pipe (get (meta cur) :pipeline)
         f (make-FlowPoint pipe ConditionalPoint WhilePoint ForPoint) ]
    (fw-configure! f ac cur)
    (fw-realize! f)))

(deftype ForLoopExpr [ ^:unsynchronized-mutable started
                       ^:unsynchronized-mutable loopCnt
                       loopCountExpr ] BoolExpr
  (eval [_ job]
    (try
      (when-not (started)
        (set! loopCnt (.eval loopCountExpr job))
        (set! started true))
      (debug "currnet loop " loopCnt)
      (let [ rc (> loopCnt 0) ]
        rc)
      (finally
        (set! loopCnt (dec loopCnt))))))

(defmethod ac-realize! :For [ac fw]
  (let [ b (.getf ac :body)
         t (.getf ac :test) ]
    (when-not (nil? b)
      (.setf fw :body (ac-reify b fw)))
    (.setf fw :test (ForLoopExpr. false 0 t))
    fw))

(defmethod fw-evaluate! :ForPoint [fw job]
  (evalWhilePoint fw job))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private conditionals-eof nil)

