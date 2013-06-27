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

(ns testzotohcljc.util.byteutils
  (:use [clojure.test])
  (:import (java.nio.charset Charset))
  (:require [comzotohcljc.util.byteutils :as BU])
  )

(def ^:private CS_UTF8 (Charset/forName "utf-8"))

(deftest test-byteutils-module

(is (= "heeloo" (String. (BU/to-chars (BU/to-bytes (.toCharArray "heeloo") CS_UTF8) CS_UTF8))))

(is (= 4 (alength (BU/write-bytes (Integer/MAX_VALUE)))))
(is (= 8 (alength (BU/write-bytes (Long/MAX_VALUE)))))

(is (= (Integer/MAX_VALUE) (BU/read-int (BU/write-bytes (Integer/MAX_VALUE)))))
(is (= (Long/MAX_VALUE) (BU/read-long (BU/write-bytes (Long/MAX_VALUE)))))

)

(def ^:private test-byteutils-eof nil)

(clojure.test/run-tests 'testzotohcljc.util.byteutils)

