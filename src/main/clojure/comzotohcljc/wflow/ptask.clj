(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.wflow.ptask )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol PTaskPoint)
(defprotocol PTask)
(defprotocol Work
  (perform [_ fw job] ))

(defn make-ptask-work [cb]
  (let [ impl (make-mmap) ]
    (reify Work
      (perform [_ fw job]
        (let [ c (.getf fw :attmt) ]
          (.setf fw :attmt nil)
          (.mm-s impl :res nil)
          (.mm-s impl :cur fw)
          (apply cb fw job c))))))

(defn make-ptask [work]
  (let [ b (make-Activity PTask) ]
    (.setf b :task work)
    b))

(defmethod ac-reify :PTask [ac cur]
  (let [ pipe (get (meta cur) :pipeline)
         f (make-FlowPoint pipe PTaskPoint) ]
    (fw-configure! f ac cur)
    (fw-realize! f)))

(defmethod ac-realize! :PTask [ac fw]
  (let [ w (.getf ac :task) ]
    (.setf fw :task w)
    fw))

(defmethod fw-evaluate! :PTaskPoint [fw job]
  (do
    (debug "[" (:id (meta fw)) "] about to perform work.")
    (let [ pipe (:pipeline (meta fw))
           w (.getf fw :task)
           np (.getf fw :next)
           na (.perform w fw job) ]
      (with-local-vars [rc np]
        (when-not (nil? na)
          (if (satisfies? Nihil na)
            (var-set rc (ac-reify-zero pipe))
            (var-set rc (ac-reify na rc))))
        @rc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private ptask-eof nil)


