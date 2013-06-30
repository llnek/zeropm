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

(ns testzotohcljc.util.metautils)

(use '[clojure.test])
(require '[comzotohcljc.util.metautils :as MU])


(deftest test-metautils-module

(is (true? (MU/is-child (Class/forName "java.lang.Number") (Class/forName "java.lang.Integer"))))
(is (true? (MU/is-child (Class/forName "java.lang.Number") (Integer. 3))))
(is (identical? (MU/bytes-class) (class (byte-array 0))))
(is (identical? (MU/chars-class) (class (char-array 0))))

(is (true? (MU/is-boolean? (class (boolean true)))))
(is (true? (MU/is-char? (class (char 3)))))
(is (true? (MU/is-int? (class (int 3)))))
(is (true? (MU/is-long? (class (long 3)))))
(is (true? (MU/is-float? (class (float 3.2)))))
(is (true? (MU/is-double? (class (double 3.2)))))
(is (true? (MU/is-byte? (class (aget (byte-array 1) 0)))))
(is (true? (MU/is-short? (class (short 3)))))
(is (true? (MU/is-string? (class ""))))
(is (true? (MU/is-bytes? (class (byte-array 0)))))

(is (not (nil? (MU/for-name "java.lang.String"))))
(is (not (nil? (MU/get-cldr))))

(is (true? (do (MU/set-cldr (MU/get-cldr)) true)))

(is (not (nil? (MU/load-class "java.lang.String"))))

(is (= "" (MU/make-obj "java.lang.String")))

(is (= 1 (.size (MU/list-parents (Class/forName "java.lang.String")))))

(is (> (.size (MU/list-methods (Class/forName "java.lang.String"))) 40))
(is (> (.size (MU/list-fields (Class/forName "java.lang.String"))) 5))


)

(def ^:private metautils-eof nil)

;;(clojure.test/run-tests 'testzotohcljc.util.metautils)

