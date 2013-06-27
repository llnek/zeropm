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

(ns testzotohcljc.util.procutils
  (:use [clojure.test])
  (:import (org.apache.commons.io FileUtils))
  (:import (java.io File))
  (:require [comzotohcljc.util.coreutils :as CU])
  (:require [comzotohcljc.util.procutils :as PU])
  )

(def ^:private CUR_MS (System/currentTimeMillis))
(def ^:private CUR_FP (File. (str (System/getProperty "java.io.tmpdir") "/" CUR_MS)))

(deftest test-procutils-module

(is (true? (do
              (PU/coroutine (fn [] (FileUtils/writeStringToFile CUR_FP "heeloo" "utf-8")))
              (PU/safe-wait 3000)
              (and (.exists CUR_FP) (>= (.length CUR_FP) 6)))))

(is (> (.length (PU/pid)) 0))


)

(def ^:private test-procutils-eof nil)

(clojure.test/run-tests 'testzotohcljc.util.procutils)

