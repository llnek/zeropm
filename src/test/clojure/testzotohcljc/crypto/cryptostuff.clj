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

(ns testzotohcljc.crypto.cryptostuff
  (:use [clojure.test])
  (:require [comzotohcljc.crypto.cryptors :as RT])
  (:require [comzotohcljc.crypto.stores :as ST])
  (:require [comzotohcljc.crypto.cryptutils :as RU])
  )

(deftest test-cryptostuff-module


(is (not (= "heeloo, how are you?" (RT/caesar-decrypt (RT/caesar-encrypt "heeloo, how are you?" 709394) 666))))
(is (= "heeloo, how are you?" (RT/caesar-decrypt (RT/caesar-encrypt "heeloo, how are you?" 709394) 709394)))

(is (= "heeloo" (let [ c (comzotohcljc.crypto.cryptors.JasyptCryptor.) ]
                      (.decrypt c (.encrypt c "heeloo")))))

(is (= "heeloo" (let [ c (comzotohcljc.crypto.cryptors.JasyptCryptor.) ]
                      (.decrypt c "secret" (.encrypt c "secret" "heeloo")))))

(is (= "heeloo" (let [ c (comzotohcljc.crypto.cryptors.JavaCryptor.) ]
                      (.decrypt c (.encrypt c "heeloo")))))

(is (= "heeloo" (let [ c (comzotohcljc.crypto.cryptors.JavaCryptor.) ]
                      (.decrypt c "secret" (.encrypt c "secret" "heeloo")))))

(is (= "heeloo" (let [ c (comzotohcljc.crypto.cryptors.BouncyCryptor.) ]
                      (.decrypt c (.encrypt c "heeloo")))))

(is (= "heeloo" (let [ c (comzotohcljc.crypto.cryptors.BouncyCryptor.) ]
                      (.decrypt c "secret" (.encrypt c "secret" "heeloo")))))

(is (= .(length (.text (create-strong-pwd 16))) 16))
(is (= .(length (create-random-string 64)) 64))

(is (instance? comzotohcljc.crypto.cryptors.Password (create-password "secret-text")))
(is (.startsWith (.encoded (create-password "secret-text")) "CRYPT:"))


)

(def ^:private test-cryptostuff-eof nil)

(clojure.test/run-tests 'testzotohcljc.crypto.cryptostuff)

