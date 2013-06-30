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

(ns testzotohcljc.util.codes)

(use '[clojure.test])
(require '[comzotohcljc.util.countrycode :as CC])
(require '[comzotohcljc.util.usastate :as SC])

(deftest test-codes-module

(is (= (CC/find-country "AU") (CC/find-country "au")))
(is (= "Australia" (CC/find-country "AU")))
(is (= "AU" (CC/find-code "Australia")))
(is (false? (CC/usa? "aa")))
(is (and (CC/usa? "US") (= (CC/usa? "US") (CC/usa? "us"))))
(is (> (.size (CC/list-codes)) 0))

(is (= (SC/find-state "CA") (SC/find-state "ca")))
(is (= "California" (SC/find-state "ca")))
(is (= "CA" (SC/find-code "California")))
(is (> (.size (SC/list-codes)) 0))

)

(def ^:private codes-eof nil)

;;(clojure.test/run-tests 'testzotohcljc.util.codes)

