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

(ns testzotohcljc.util.fileutils
  (:use [clojure.test])
  (:import (org.apache.commons.io FileUtils))
  (:import (com.zotoh.frwk.io XData))
  (:import (java.io File))
  (:require [comzotohcljc.util.fileutils :as FU])
  (:require [comzotohcljc.util.coreutils :as CU])
  )

(def ^:private TMP_DIR (File. (System/getProperty "java.io.tmpdir")))
(def ^:private TMP_FP (File. TMP_DIR (str (CU/uid) ".txt")))
(eval '(do (FileUtils/writeStringToFile TMP_FP "heeloo")))

(deftest test-fileutils-module

(is (true? (FU/file-readwrite? TMP_FP)))
(is (true? (FU/file-read? TMP_FP)))

(is (true? (FU/dir-readwrite? TMP_DIR)))
(is (true? (FU/dir-read? TMP_DIR)))

(is (false? (FU/can-exec? TMP_FP)))
(is (true? (FU/can-exec? TMP_DIR)))

(is (= "/tmp/a/b" (FU/parent-path "/tmp/a/b/c")))
(is (nil?  (FU/parent-path nil)))

(is (= "heeloo" (let [ fp (str (CU/uid) ".txt") ]
                    (FU/save-file TMP_DIR fp (FU/get-file TMP_DIR (.getName TMP_FP)))
                    (FileUtils/readFileToString (File. TMP_DIR fp) "utf-8")) ))


)

(def ^:private test-fileutils-eof nil)

(clojure.test/run-tests 'testzotohcljc.util.fileutils)

