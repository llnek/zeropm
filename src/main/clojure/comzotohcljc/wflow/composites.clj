;;
;; COPYRIGHT (C) 2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
;;
;; THIS IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
;; MODIFY IT UNDER THE TERMS OF THE APACHE LICENSE
;; VERSION 2.0 (THE "LICENSE").
;;
;; THIS LIBRARY IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL
;; BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
;;
;; SEE THE LICENSE FOR THE SPECIFIC LANGUAGE GOVERNING PERMISSIONS
;; AND LIMITATIONS UNDER THE LICENSE.
;;
;; You should have received a copy of the Apache License
;; along with this distribution; if not you may obtain a copy of the
;; License at
;; http://www.apache.org/licenses/LICENSE-2.0
;;

(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.wflow.composites )


(require '[comzotohcljc.util.coreutils :as CU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol MutableListAPI
  (is-empty? [_] )
  (size [_])
  (shift [_]))
(deftype MutableList [ ^:unsynchronized-mutable data] MutableListAPI
  (is-empty? [_] (= 0 (count @data) ))
  (size [_] (count @data))
  (shift [this]
    (if (is-empty this)
      nil
      (let [ f (first @data) ]
        (var-set data (pop @data))
        f))))

(defprotocol IterWrapperAPI
  (isEmpty [_])
  (size [_])
  (nextPoint [_]))

(defn make-IterWrapper [outerPoint children]
  (let [ impl (MutableList. (into () (reverse children))) ]
    (reify
      IterWrapperAPI
      (isEmpty [_] (.is-empty? impl))
      (size [_] (.size impl))
      (nextPoint [_]
        (let [ n (.shift impl) ]
          (if (nil? n)
            nil
            (ac-reify n outerPoint)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composite

(defprotocol CompositePoint)
(defprotocol Composite)

(defn composite-add! [b a]
  (when-not (nil? a)
    (let [ c (.getf b :children) ]
      (.setf b :children (conj c a))))
  b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block

(defprotocol BlockPoint)
(defprotocol Block)

(defn make-block [ & args ]
  (let [ b (make-Activity [ Composite Block ] )
         v (if (empty? args)
             []
             (vec (flatten (conj [] args)))) ]
    (.setf b :children v)
    b))

(defmethod ac-reify :Block [ac cur]
  (let [ pipe (get (meta cur) :pipeline)
         f (make-FlowPoint pipe CompositePoint BlockPoint) ]
    ;;(.setf f :inner-points nil)
    (fw-configure! f ac cur)
    (fw-realize! f)))

(defmethod ac-realize! :Block [ac fw]
  (let [ w (make-IterWrapper fw (.getf ac :children)) ]
    (.setf fw :inner-points w)
    fw))

(defmethod fw-evaluate! :BlockPoint [fw job]
  (let [ c (.getf fw :attmt) ;; data pass back from previous async call?
         w (.getf fw :inner-points) ]
    (.setf fw :attmt nil)
    (with-local-vars [ rc nil ]
      (if (or (nil? w) (.isEmpty w))
        (do
          (debug "no more inner elements.")
          (var-set rc (.getf fw :next))
          (when-not (nil? @rc)
            (.setf @rc :attmt c))
          (fw-realize! fw)
          @rc)
        (do
          (debug (.size w) " element(s)")
          (fw-evaluate! (doto (.nextPoint w)(.setf :attmt c)) job))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Join

(defprotocol JoinPoint)
(defprotocol Join)

(defprotocol NullJoinPoint)
(defprotocol NullJoin)

(defprotocol AndJoinPoint)
(defprotocol AndJoin)

(defprotocol OrJoinPoint)
(defprotocol OrJoin)

(defn make-nulljoin []
  (let [ a (make-Activity Join NullJoin) ]
    (.setf a :body nil)
    (.setf a :branches 0)
    a))

(defmethod ac-reify :NullJoin [ac cur]
  (let [ pipe (get (meta cur) :pipeline)
         f (make-FlowPoint pipe JoinPoint NullJoinPoint) ]
    (fw-configure! f ac cur)
    (fw-realize! f)))

(defmethod ac-realize! :NullJoin [ac fw] fw)
(defmethod fw-evaluate! :NullJoinPoint [fw job] nil)


(defn make-andjoin [body]
  (let [ a (make-Activity Join AndJoin) ]
    (.setf a :body body)
    (.setf a :branches 0)
    a))

(defmethod ac-reify :AndJoin [ac cur]
  (let [ pipe (get (meta cur) :pipeline)
         f (make-FlowPoint pipe JoinPoint AndJoinPoint) ]
    (fw-configure! f ac cur)
    (fw-realize! f)))

(defmethod ac-realize! :AndJoin [ac fw]
  (let [ b (.getf ac :body)
         n (.getf ac :branches) ]
    (when-not (nil? b)
      (.setf fw :body (ac-reify b (.getf fw :next))) )
    (.setf fw :branches n)
    (.setf fw :counter (AtomicLong. 0))
    fw))

(defmethod fw-evaluate! :AndJoinPoint [fw job]
  (let [ c (.getf fw :attmt)
         b (.getf fw :branches)
         body (.getf fw :body)
         n (.getf fw :counter) ]
    (.setf fw :attmt nil)
    (.incrementAndGet n)
    (debug "branches " b ", counter "  (.get n) ", join(pid) = " fw)
    (with-local-vars [ rc nil nn (.get n) ]
      ;; all branches have returned, proceed...
      (when (= nn b)
        (var-set rc (if (nil? body) (.getf fw :next) body))
        (when-not (nil? @rc)
          (.setf @rc :attmt c))
        (fw-realize! fw))
      @rc)))



(defn make-orjoin [body]
  (let [ a (make-Activity Join OrJoin) ]
    (.setf a :body body)
    (.setf a :branches 0)
    a))

(defmethod ac-reify :OrJoin [ac cur]
  (let [ pipe (get (meta cur) :pipeline)
         f (make-FlowPoint pipe JoinPoint OrJoinPoint) ]
    (fw-configure! f ac cur)
    (fw-realize! f)))

(defmethod ac-realize! :OrJoin [ac fw]
  (let [ b (.getf ac :body)
         n (.getf ac :branches) ]
    (when-not (nil? b)
      (.setf fw :body (ac-reify b (.getf fw :next))) )
    (.setf fw :branches n)
    (.setf fw :counter (AtomicLong. 0))
    fw))

(defmethod fw-evaluate! :OrJoinPoint [fw job]
  (let [ c (.getf fw :attmt)
         b (.getf fw :branches)
         np (.getf fw :next)
         body (.getf fw :body)
         n (.getf fw :counter) ]
    (.setf fw :attmt nil)
    (.incrementAndGet n)
    (debug "branches " b ", counter "  (.get n) ", join(pid) = " fw)
    (with-local-vars [ rc nil nn (.get n) ]
      (cond
        (= b 0) (do (var-set rc np) (fw-realize! fw))
        (= 1 nn) (var-set rc (if (nil? body) np body))
        (= b nn) (do (var-set rc nil) (fw-realize! fw)))
      (when-not (nil? @rc)
        (.setf @rc :attmt c))
      @rc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Split

(defprotocol SplitPoint)
(defprotocol Split)

(defn make-split [ joiner ]
  (let [ s (make-Activity Composite Split) ]
    (.setf s :children [])
    (.setf s :join joiner)
    s))

(defmethod ac-reify :Split [ac cur]
  (let [ pipe (get (meta cur) :pipeline)
         f (make-FlowPoint pipe CompositePoint SplitPoint) ]
    ;;(.setf f :inner-points nil)
    (fw-configure! f ac cur)
    (fw-realize! f)))

(defmethod ac-realize! :Split [ac fw]
  (let [ cs (.getf ac :children)
         j (.getf ac :join)
         np(.getf fw :next)
         n (count cs)
         s (if (nil? j)
             (ac-reify (make-nulljoin) np)
             (do
               (.setf j :branches n)
               (ac-reify j np))) ]
    (when (nil? j)
      (.setf fw :fall-thru true) )
    (.setf fw :inner-points (make-IterWrapper s cs))
    fw))

(defmethod fw-evaluate! :SplitPoint [fw job]
  (let [ w (:getf fw :inner-points)
         c (:getf fw :attmt) ]
    (.setf fw :attmt nil)
    (while (and (CU/notnil? w) (not (.isEmpty w)))
      (let [ n (.nextPoint w) ]
        (.setf n :attmt c)
        (-> pipe (.core)(.run n))))
    (fw-realize! fw)
    ;; should we also pass the closure to the next step ? not for now
    (if (.getf fw :fall-thru)
      (.getf fw :next)
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private composites-eof nil)


