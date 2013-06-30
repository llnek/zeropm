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

(ns testzotohcljc.util.strutils)

(use '[clojure.test])
(require '[comzotohcljc.util.strutils :as SU])


(deftest test-strutils-module

(is (false? (SU/has? "hallowed are the ori" \z)))
(is (true? (SU/has? "hallowed are the ori" \w)))

(is (= "heeloo" (SU/nsb "heeloo")))
(is (= "" (SU/nsb nil)))

(is (= "heeloo" (SU/nsn "heeloo")))
(is (= "(null)" (SU/nsn nil)))

(is (false? (SU/same? "aaa" "axa")))
(is (true? (SU/same? "aaa" "aaa")))

(is (true? (SU/hgl? "haha")))
(is (false? (SU/hgl? "")))

(is (= "haha" (SU/strim "            haha                          ")))
(is (= "" (SU/strim nil)))

(is (= "joe;blogg" (let [ x (StringBuilder.) ]
                (SU/add-delim! x ";" "joe")
                (SU/add-delim! x ";" "blogg")
                (.toString x))))

(is (= 4 (.size (SU/splunk "hello, how are you" 5))))

(is (true? (SU/hasic-any? "hallowed are the ori" [ "sdfsdg" "jffflf" "Are" ])))
(is (false? (SU/has-any? "hallowed are the ori" [ "sdfsdg" "jffflf" "Are" ])))

(is (true? (SU/swic-any? "hallowed are the ori" [ "sdfsdg" "jffflf" "Hall" ])))
(is (true? (SU/sw-any? "hallowed are the ori" [ "sdfsdg" "jffflf" "ha" ])))
(is (false? (SU/sw-any? "hallowed are the ori" [ "sdfsdg" "jffflf" ])))

(is (true? (SU/eqic-any? "heeloo" [ "sdfsdg" "jffflf" "HeeLoo" ])))
(is (true? (SU/eq-any? "heeloo" [ "sdfsdg" "jffflf" "heeloo" ])))
(is (false? (SU/eq-any? "heeloo" [ "sdfsdg" "jffflf" ])))

(is (= 10 (.length (SU/make-string \x 10))))
(is (= "ori" (SU/right "Hallowed are the ori" 3)))
(is (= "Hal" (SU/left "Hallowed are the ori" 3)))










)

(def ^:private strutils-eof nil)

;;(clojure.test/run-tests 'testzotohcljc.util.strutils)

