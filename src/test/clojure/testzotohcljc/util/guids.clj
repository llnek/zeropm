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

(ns testzotohcljc.util.guids
  (:use [clojure.test])
  (:require [comzotohcljc.util.guids :as GU])
  )

;;(def ^:private UID_2 (GU/new-uuid))
;;(def ^:private UID_1 (GU/new-uuid))
;;(def ^:private WID_2 (GU/new-wwid))
;;(def ^:private WID_1 (GU/new-wwid))

(deftest test-guids-module

(is (not (= (GU/new-wwid) (GU/new-wwid))))
(is (not (= (GU/new-uuid) (GU/new-uuid))))

(is (= (.length (GU/new-wwid)) 48))
(is (= (.length (GU/new-uuid)) 36))

)

(def ^:private test-guids-eof nil)

(clojure.test/run-tests 'testzotohcljc.util.guids)

