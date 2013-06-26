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

(ns testzotohcljc.util.coreutils
  (:use [clojure.test])
  (:import (java.util Date Calendar))
  (:import (java.sql Timestamp))
  (:import (java.io File))
  (:import (java.nio.charset Charset))
  (:require [comzotohcljc.util.coreutils :as CU])
  )

(def ^:private VAR_USER (System/getProperty "user.name"))
(def ^:private VAR_PATH (System/getenv "PATH"))

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













(def ^:private test-coreutils-eof nil)


