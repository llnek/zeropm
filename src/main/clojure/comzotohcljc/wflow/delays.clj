
(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.wflow.delays)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol AsyncWaitPoint)
(defprotocol AsyncWait)

(defprotocol DelayPoint)
(defprotocol Delay)

(defprotocol FAsyncResumeToken
  (resume [_ resArg] ))

(defn async-resume-token [fw]
  (reify
    FAsyncResumeToken
      (resume [_ resArg]
        (let [ np (.getf fw :next) ]
          (when-not (nil? np)
            (.setf np :attmt resArg)
            (.rerun pipe np))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delay

(defn make-delay [delayMillis]
  (let [ b (make-Activity Delay) ]
    (.setf b :delayMillis delayMillis)
    b))

(defmethod ac-reify :Delay [ac cur]
  (let [ pipe (get (meta cur) :pipeline)
         f (make-FlowPoint pipe DelayPoint) ]
    (fw-configure! f ac cur)
    (fw-realize! f)))

(defmethod ac-realize! :Delay [ac fw]
  (let [ d (.getf ac :delayMillis) ]
    (.setf fw :delayMillis d)
    fw))

(defmethod fw-evaluate! :DelayPoint [fw job] fw)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AsyncWait

(defn make-asyncwait []
  (let [ b (make-Activity AsyncWait) ]
    b))

(defmethod ac-reify :AsyncWait [ac cur]
  (let [ pipe (get (meta cur) :pipeline)
         f (make-FlowPoint pipe AsyncWaitPoint) ]
    (fw-configure! f ac cur)
    (fw-realize! f)))

(defmethod ac-realize! :AsyncWait [ac fw] fw)
(defmethod fw-evaluate! :AsyncWait [fw job] fw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private delays-eof nil)


