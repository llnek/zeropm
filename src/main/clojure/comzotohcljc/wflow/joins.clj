(ns { :doc ""
      :author "kenl" }
  comzotohcljc.wflow.joins )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol JoinStep)
(defprotocol Join
  (getBranches [_] )
  (setBranches [_ n] )
  (getBody [_] )
  (withBody [_ b] ))

(defprotocol AndStep)
(defprotocol And)

(defprotocol JoinInnardsAPI
  (get-branches [_] )
  (set-branches [_ n] )
  (get-body [_] )
  (set-body [_ b] ) )
(deftype JoinInnards [ ^:unsynchronized-mutable branches
                       ^:unsynchronized-mutable body]
  JoinInnardsAPI
  (get-branches [_] @branches)
  (set-branches [_ n] (var-set branches n))
  (get-body [_] @body)
  (set-body [_ b] (var-set body b)) )

(defn make-andjoin [body]
  (let [ impl (JoinInnards. 0 body) ]
    (meta-activity (reify
      Activity
      Join
      (getBranches [_] (.get-branches impl))
      (setBranches [_ n] (.set-branches impl n))
      (getBody [_] (.get-body impl))
      (withBody [_ b] (.set-body impl b))
      And) :And )) )




(defn- reifyAndJoin [cur a])















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private joins-eof nil)

