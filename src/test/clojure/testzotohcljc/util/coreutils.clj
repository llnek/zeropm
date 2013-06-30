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

(ns testzotohcljc.util.coreutils)

(use '[clojure.test])
(import '(java.util Properties Date Calendar))
(import '(java.sql Timestamp))
(import '(java.net URL))
(import '(java.io FileOutputStream File))
(import '(java.nio.charset Charset))
(require '[comzotohcljc.util.coreutils :as CU])


(def ^:private VAR_USER (System/getProperty "user.name"))
(def ^:private VAR_PATH (System/getenv "PATH"))

(def ^:private dummyResourcePath "com/zotoh/frwk/i18n/Resources_en.properties")
(def ^:private dummyProperties (Properties.))
(eval '(do
  (.put dummyProperties "1" "hello${user.name}")
  (.put dummyProperties "2" "hello${PATH}")
  (.put dummyProperties "3" "${user.name}${PATH}")
  (def ^:private dummyPropertiesResult (CU/subs-props dummyProperties))))


(deftest test-coreutils-module

(is (CU/is-nichts? CU/*NICHTS*))
(is (not (CU/is-nichts? "")))
(is (= (CU/nil-nichts nil) CU/*NICHTS*))
(is (= (CU/nil-nichts "") ""))

(is (not (CU/match-char? \space #{ \a \b \x })))
(is (CU/match-char? \x #{ \a \b \x }))

(is (not (nil? (CU/sysvar "java.io.tmpdir"))))
(is (not (nil? (CU/envvar "PATH"))))

(is (not (nil? (CU/uid))))
(is (< (.indexOf (CU/uid) ":\\-") 0))

(is (not (nil? (CU/new-random))))

(is (instance? Timestamp (CU/now-jtstamp)))
(is (instance? Date (CU/now-date)))
(is (instance? Calendar (CU/now-cal)))

(is (instance? Charset (CU/to-charset "utf-16")))
(is (instance? Charset (CU/to-charset)))

(is (= "/c:/temp/abc.txt" (CU/nice-fpath (File. "/c:\\temp\\abc.txt"))))
(is (= "/c:/temp/abc.txt" (CU/nice-fpath "/c:\\temp\\abc.txt")))

(is (= (str "hello" VAR_PATH "world" VAR_USER) (CU/subs-var "hello${PATH}world${user.name}")))
(is (= (str "hello" VAR_PATH) (CU/subs-evar "hello${PATH}")))
(is (= (str "hello" VAR_USER) (CU/subs-svar "hello${user.name}")))


(is (= (str VAR_USER VAR_PATH) (.getProperty dummyPropertiesResult "3")))
(is (= (str "hello" VAR_USER) (.getProperty dummyPropertiesResult "1")))
(is (= (str "hello" VAR_PATH) (.getProperty dummyPropertiesResult "2")))

(is (= "Java Virtual Machine Specification" (CU/sysprop "java.vm.specification.name")))

(is (= "/tmp/a/b/c" (CU/trim-lastPathSep  "/tmp/a/b/c////")))
(is (= "c:\\temp" (CU/trim-lastPathSep  "c:\\temp\\\\\\\\")))

(is (= "heeloo" (CU/deserialize (CU/serialize "heeloo"))))

(is (= "java.lang.String" (CU/get-classname "")))

;;(is (= "/tmp/a/b/c" (CU/file-path (File. "/tmp/a/b/c"))))

(is (true? (CU/is-unix?)))

(is (= (double 100) (CU/conv-double  "xxxx" 100)))
(is (= 23.23 (CU/conv-double  "23.23" 100)))
(is (= 100 (CU/conv-long "xxxx" 100)))
(is (= 23 (CU/conv-long "23" 100)))

(is (true? (CU/conv-bool "true")))
(is (true? (CU/conv-bool "yes")))
(is (true? (CU/conv-bool "1")))
(is (false? (CU/conv-bool "false")))
(is (false? (CU/conv-bool "no")))
(is (false? (CU/conv-bool "0")))

(is (= 3 (.size
  (let [ fp (File. (str (System/getProperty "java.io.tmpdir") "/" (CU/uid))) ]
    (with-open [ os (FileOutputStream. fp) ] (.store dummyProperties os ""))
    (CU/load-javaprops fp)) )))

(is (= "heeloo" (CU/stringify (CU/bytesify "heeloo"))))

(is (instance? (class (byte-array 0)) (CU/rc-bytes dummyResourcePath)))
(is (> (.length (CU/rc-str dummyResourcePath)) 0))
(is (instance? java.net.URL (CU/rc-url dummyResourcePath)))

(is (= "heeloo" (CU/stringify (CU/inflate (CU/deflate (CU/bytesify "heeloo"))))))

(is (= "0x24A0x3cb0x3eZ0x21" (CU/normalize "$A<b>Z!")))

(is (> (CU/now-millis) 0))

(is (= "/tmp/abc.txt" (CU/get-fpath "file:/tmp/abc.txt")))

(is (instance? URL (CU/fmt-fileurl "/tmp/abc.txt")))

(is (and (instance? File (CU/get-tmpdir)) (not (= (CU/make-tmpdir) (CU/get-tmpdir)))))

(is (true? (do (CU/test-isa "" (Class/forName "java.lang.Long") (Class/forName "java.lang.Number")) true)))
(is (true? (do (CU/test-isa "" "" (Class/forName "java.lang.Object")) true)))
(is (true? (do (CU/test-nonil "" (Object.)) true)))
(is (true? (do (CU/test-cond "" true) true)))
(is (true? (do (CU/test-nestr "" "heeloo") true)))

(is (true? (do (CU/test-nonegnum "" 23.0) true)))
(is (true? (do (CU/test-nonegnum "" 23) true)))
(is (true? (do (CU/test-nonegnum "" 0.0) true)))
(is (true? (do (CU/test-nonegnum "" 0) true)))

(is (true? (do (CU/test-posnum "" 23.0) true)))
(is (true? (do (CU/test-posnum "" 23) true)))

(is (true? (do (CU/test-neseq "" [ 1 2 ]) true)))

(is (thrown? IllegalArgumentException (CU/throw-badarg "a")))

(is (true? (let [ x (IllegalArgumentException. "") ] (identical? x (CU/root-cause x)))))

(is (= "java.lang.IllegalArgumentException: heeloo" (CU/root-causemsg (IllegalArgumentException. "heeloo"))))

(is (= 3 (.size (CU/gen-numbers 1 10 3))))

(is (= "ACZ" (CU/sort-join [ "Z" "C" "A"])))


















)

(def ^:private coreutils-eof nil)

;;(clojure.test/run-tests 'testzotohcljc.util.coreutils)

