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

(ns ^{ :doc "OS Process related utilities." :author "kenl" }
  com.zotoh.frwk.procutils
  (:import (java.lang.management ManagementFactory))
  (:import (com.zotoh.frwk.util CoreUtils))
  (:require [ com.zotoh.frwk.coreutils :as CU])
  (:require [ com.zotoh.frwk.metautils :as MU])
  (:require [ com.zotoh.frwk.strutils :as SU])
  )

(defn asyncExec
  "Run the code (runnable) in a separate daemon thread."
  [runable]
  (if (nil? runable)
    nil
    (doto (Thread. runable)
      (.setContextClassLoader (MU/getCZldr))
      (.setDaemon true)
      (.start))) )

(defn coroutine
  "Run this function asynchronously."
  [func]
  (let [ r (reify Runnable
             (run [_] (do (func)))) ]
    (asyncExec r)))

(defn safeWait
  "Block current thread for some millisecs."
  [millisecs]
  (try
    (if (> millisecs 0) (Thread/sleep millisecs))
    (catch Throwable t nil)))

(defn pid
  "Get the current process pid."
  []
  (let [ ss (.split (SU/nsb (.getName (ManagementFactory/getRuntimeMXBean))) "@") ]
    (if (or (nil? ss) (empty ss)) "" (first ss))) )



(def ^:private procutils-eof nil)

