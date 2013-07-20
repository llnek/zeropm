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
  comzotohcljc.wflow.core )




(require '[comzotohcljc.util.coreutils :as CU])





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol MutableObjectAPI
  (setf [_ k v] )
  (getf [_ k] )
  (clrf [_ k] ))

(defprotocol FlowPoint)
(defprotocol Activity)

(defprotocol NihilPoint)
(defprotocol Nihil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-Activity [ & args ]
  `(let [ impl# (CU/make-mmap) ]
    (reify
      MutableObjectAPI
      (setf [_ k v] (.mm-s impl# k v))
      (getf [_ k] (.mm-g impl# k))
      (clrf [_ k] (.mm-r impl# k))
      Activity
      ~@(filterv CU/notnil? args))))






(defmacro make-FlowPoint [ pipe & args ]
  `(let [ impl# (CU/make-mmap) ]
    (with-meta (reify
      MutableObjectAPI
      (setf [_ k v] (.mm-s impl# k v))
      (getf [_ k] (.mm-g impl# k))
      (clrf [_ k] (.mm-r impl# k))
      Runnable
      (run [me#] )
      FlowPoint
      ~@(filterv CU/notnil? args)) { :pipeline ~pipe } )))


(defn fw-configure! [fp ac cur]
  (do
    (.setf fp :template ac)
    (.setf fp :next cur)))


(defmethod fw-realize! [fw]
  (let [ a (.getf fw :template) ]
    (ac-realize! a fw)
    fw))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nihil

(defn make-nihil []
  (let [ b (make-Activity Nihil) ]
    b))

(defmethod ac-reify :Nihil [ac cur]
  (let [ pipe (get (meta cur) :pipeline)
         f (make-FlowPoint pipe NihilPoint) ]
    (fw-configure! f ac cur)
    (fw-realize! f)))

(defmethod ac-realize! :Nihil [ac fw]
  (do
    (.setf fw :next fw)
    fw))

(defmethod fw-evaluate! :NihilPoint [fw job] fw)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(def ^:private core-eof nil)

