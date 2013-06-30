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

(ns ^{ :doc "Generate some sequence numbers." :author "kenl" }
  comzotohcljc.util.seqnumgen)

(import '(java.util.concurrent.atomic AtomicLong AtomicInteger) )


(def ^:private _numInt (AtomicInteger. 1))
(def ^:private  _numLong (AtomicLong. 1))

(defn next-int ^{ :doc "Return a sequence number (integer)." }
  []
  (.getAndIncrement _numInt))

(defn next-long ^{ :doc "Return a sequence number (long)." }
  []
  (.getAndIncrement _numLong))





(def ^:private  seqnumgen-eof nil)

