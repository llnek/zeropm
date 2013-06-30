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

(ns testzotohcljc.util.ioutils)

(use '[clojure.test])
(import '(org.apache.commons.io FileUtils))
(import '(java.io FileReader File InputStream OutputStream FileOutputStream))
(import '(com.zotoh.frwk.io XData XStream))
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.ioutils :as IO])


(def ^:private TMP_DIR (File. (System/getProperty "java.io.tmpdir")))
(def ^:private TMP_FP (File. TMP_DIR (str (CU/uid) ".txt")))
(eval '(do (FileUtils/writeStringToFile TMP_FP "heeloo" "utf-8")))


(deftest test-ioutils-module

(is (true? (.exists (IO/make-tmpfile))))

(is (true? (let [ v (IO/newly-tmpfile) ]
              (and (.exists (first v)) (nil? (nth v 1))))))

(is (true? (let [ v (IO/newly-tmpfile true)
                  rc (and (.exists (first v)) (instance? OutputStream (nth v 1))) ]
             (when rc (.close (nth v 1)))
             rc)))

(is (instance? InputStream (IO/streamify (byte-array 10))))

(is (instance? OutputStream (IO/make-baos)))

(is (= "616263" (IO/hexify-string (CU/bytesify "abc"))))

(is (= "heeloo world!" (CU/stringify (IO/gunzip (IO/gzip (CU/bytesify "heeloo world!"))))))

(is (true? (do (IO/reset-stream! (IO/streamify (CU/bytesify "hello"))) true)))

(is (true? (let [ xs (IO/open-file (.getCanonicalPath TMP_FP))
                    rc (instance? XStream xs) ] (.close xs) rc)))

(is (true? (let [ xs (IO/open-file TMP_FP) rc (instance? XStream xs) ] (.close xs) rc)))

(is (= "heeloo world" (CU/stringify (IO/from-gzb64 (IO/to-gzb64 (CU/bytesify "heeloo world"))))))

(is (>= (with-open [ inp (IO/open-file TMP_FP) ] (IO/available inp)) 6))

(is (true? (.exists (with-open [ inp (IO/open-file TMP_FP) ] (IO/copy-stream inp)))))

(is (true? (let [ v (IO/newly-tmpfile false) ]
                (with-open [inp (IO/open-file TMP_FP) ]
                  (with-open [ os (FileOutputStream. (first v)) ]
                    (IO/copy-bytes inp os 4)))
                (>= (.length (first v)) 4))))

(is (true? (.isDiskFile (IO/make-xdata true))))
(is (false? (.isDiskFile (IO/make-xdata))))

(is (true? (let [ x (with-open [ inp (IO/open-file TMP_FP) ] (IO/read-bytes inp true)) ]
                (and (instance? XData x) (.isDiskFile x) (> (.size x) 0))) ))

(is (true? (let [ x (with-open [ inp (IO/open-file TMP_FP) ] (IO/read-bytes inp)) ]
                (and (instance? XData x) (not (.isDiskFile x)) (> (.size x) 0))) ))

(is (true? (let [ x (with-open [ rdr (FileReader. TMP_FP) ] (IO/read-chars rdr true)) ]
                (and (instance? XData x) (.isDiskFile x) (> (.size x) 0))) ))

(is (true? (let [ x (with-open [ rdr (FileReader. TMP_FP) ] (IO/read-chars rdr)) ]
                (and (instance? XData x) (not (.isDiskFile x)) (> (.size x) 0))) ))

(is (= "heeloo" (String. (IO/morph-chars (CU/bytesify "heeloo")))))

(is (false? (with-open [ p1 (IO/open-file TMP_FP)]
                (with-open [ p2 (IO/open-file TMP_FP)] (IO/diff? p1 p2)))))

)

(def ^:private ioutils-eof nil)

;;(clojure.test/run-tests 'testzotohcljc.util.ioutils)

