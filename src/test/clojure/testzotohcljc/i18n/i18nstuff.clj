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

(ns testzotohcljc.i18n.i18nstuff)

(use '[clojure.test])
(require '[comzotohcljc.i18n.i18nutils :as NU])
(require '[comzotohcljc.util.coreutils :as CU])


(deftest test-i18nstuff-module

(is (= "hello joe, how is your dawg" (let [ rs (NU/load-resource (CU/rc-url "com/zotoh/frwk/i18n/Resources_en.properties")) ]
           (NU/get-string rs "test" [ "joe", "dawg" ]))))










)

(def ^:private i18nstuff-eof nil)

(clojure.test/run-tests 'testzotohcljc.i18n.i18nstuff)

