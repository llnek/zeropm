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

(ns ^{ :doc "General utilties." :author "ken" }
  com.zotoh.cljc.util.coreutils
  (:use [clojure.tools.logging :only (info warn error debug)])
  ;;(:use [clojure.string])
  (:import (java.security SecureRandom))
  (:import (java.nio.charset Charset))
  (:import (java.io
    InputStream File FileInputStream
    ByteArrayInputStream ByteArrayOutputStream))
  (:import (java.util Properties Date GregorianCalendar TimeZone))
  (:import (java.util.zip DataFormatException Deflater Inflater))
  (:import (java.sql Timestamp))
  (:import (java.rmi.server UID))
  (:import (org.apache.commons.lang3.text StrSubstitutor))
  (:import (org.apache.commons.lang3 StringUtils))
  (:import (org.apache.commons.io IOUtils FilenameUtils))
  (:import (org.apache.commons.lang3 SerializationUtils))
)

(def ^:private _BOOLS #{ "true" "yes"  "on"  "ok"  "active"  "1"} )
(def ^:private _PUNCS #{ \_ \- \. \( \) \space } )

(defmacro TryC
  [ & exprs ]
  `(try (do ~@exprs) (catch Throwable e# nil)) )

(defn- nsb [s] (if (nil? s) (str "") s))

(defn- get-czldr
  ([] (get-czldr nil) )
  ([cl]
    (if (nil? cl) (.getContextClassLoader (Thread/currentThread)) cl )))

(deftype NICHTS [])
(def ^:dynamic *NICHTS* (->NICHTS ))

(defn nil-nichts [obj] (if (nil? obj) *NICHTS* obj))

(defn is-nichts? [obj] (identical? obj *NICHTS*))

(defn match-char? ^{ :doc "Returns true if this char exists inside this set of chars." }
  [ch setOfChars]
  (if (nil? setOfChars) false (contains? setOfChars ch)))

(defn sysvar ^{ :doc "Get value for this system property." }
  [v]
  (if (StringUtils/isEmpty v) nil (System/getProperty v)))

(defn envvar ^{ :doc "Get value for this env var." }
  [v]
  (if (StringUtils/isEmpty v) nil (System/getenv v)))

(defn uid ^{ :doc "Generate a unique id using std java." }
  []
  (.replaceAll (.toString (UID.)) "[:\\-]+" ""))

(defn new-random ^{ :doc "Return a new random object." }
  []
  (SecureRandom. (SecureRandom/getSeed 20)) )

(defn now-jts ^{ :doc "Return a java sql Timestamp." } [] (Timestamp. (.getTime (Date.))))

(defn now-date ^{ :doc "Return a java Date." } [] (Date.) )

(defn now-cal ^{ :doc "Return a Gregorian Calendar." } [] (GregorianCalendar. ))

(defn to-charset ^{ :doc "Return a java Charset of the encoding." }
  ([enc] (Charset/forName enc))
  ([] (to-charset "utf-8")) )

(defmulti ^{ :doc "Convert the file path into nice format without backslashes."  } nice-fpath class)

(defmethod nice-fpath String
  [fpath]
  (FilenameUtils/normalizeNoEndSeparator (nsb fpath) true) )

(defmethod nice-fpath String
  [aFile]
  (if (nil? aFile) "" (nice-fpath (.getCanonicalPath aFile)) ))

(defn subs-var ^{ :doc "Replaces all system & env variables in the value." }
  [value]
  (if (nil? value)
    ""
    (.replace (StrSubstitutor. (System/getenv)) (StrSubstitutor/replaceSystemProperties value))))

(defn subs-svar ^{ :doc "Expand any sys-var found inside the string value." }
  [value]
  (if (nil? value) "" (StrSubstitutor/replaceSystemProperties value)) )

(defn subs-evar ^{ :doc "Expand any env-var found inside the string value." }
  [value]
  (if (nil? value) "" (.replace (StrSubstitutor. (System/getenv)) value)) )

(defn subs-props ^{ :doc "Expand any env & sys vars found inside the property values." }
  [^Properties props]
  (reduce
    (fn [bc k]
      (.put bc k (subs-var (.get props k))) bc )
    (Properties.) (.keySet props) ))

(defn sysprop ^{ :doc "Get the value of a system property." }
  [prop]
  (System/getProperty (nsb prop) ""))

(defn homedir ^{ :doc "Get the user's home directory." }
  []
  (File. (sysprop "user.home")) )

(defn getuser ^{ :doc "Get the current user login name." }
  []
  (sysprop "user.name"))

(defn getcwd  ^{ :doc "Get the current dir." }
  []
  (sysprop "user.dir"))

(defn trim-lastPathSep ^{ :doc "Get rid of trailing dir paths." }
  [path]
  (.replaceFirst (nsb path) "[/\\\\]+$"  ""))

(defn serialize ^{ :doc "Object serialization." }
  [obj]
  (if (nil? obj) nil (SerializationUtils/serialize obj)) )

(defn deserialize ^{ :doc "Object deserialization." }
  [bits]
  (if (nil? bits) nil (SerializationUtils/deserialize bits)))

(defn get-classname ^{ :doc "Get the object's class name." }
  [obj]
  (if (nil? obj) "null" (.getName (.getClass obj))))

(defn file-path ^{ :doc "Get the file path." }
  [aFile]
  (if (nil? aFile) "" (nice-fpath aFile)))

(defn is-windows? ^{ :doc "Returns true if platform is windows." }
  []
  (>= (.indexOf (.toLowerCase (sysprop "os.name")) "windows") 0 ))

(defn is-unix? ^{ :doc "Returns true if platform is *nix." }
  []
  (not is-windows?))

(defn conv-long ^{ :doc "Parse string as a long value." }
  [s dftLongVal]
  (try (Long/parseLong s) (catch Throwable e dftLongVal)))

(defn conv-int ^{ :doc "Parse string as an int value." }
  [s dftIntVal]
  (try (Integer/parseInt s) (catch Throwable e dftIntVal)))

(defn conv-double ^{ :doc "Parse string as a double value." }
  [s dftDblVal]
  (try (Double/parseDouble s) (catch Throwable e dftDblVal)))

(defn conv-float ^{ :doc "Parse string as a double value."}
  [s dftFltVal]
  (try (Float/parseFloat s) (catch Throwable e dftFltVal)))

(defn conv-bool ^{ :doc "Parse string as a boolean value." }
  [s]
  (contains? _BOOLS (.toLowerCase (nsb s))))

(defmulti ^{ :doc "Load java properties from input-stream." } load-javaprops class)

(defmethod load-javaprops InputStream
  [inp]
  (let [ ps (Properties.) ]
    (.load ps inp)
    ps))

(defmethod load-javaprops File
  [aFile]
  (with-open [ inp (FileInputStream. aFile) ]
    (load-javaprops inp)))

(defn stringify ^{ :doc "Make a string from bytes." }
  ([bits] (stringify bits "utf-8"))
  ([bits encoding] (if (nil? bits) (String. bits encoding))))

(defn bytesify ^{ :doc "Get bytes with the right encoding." }
  ([s] (bytesify s "utf-8"))
  ([s encoding] (if (nil? s) nil (.getBytes s encoding))))

(defn rc-stream ^{ :doc "Load the resource as stream." }
  ([rcPath] (rc-stream rcPath nil))
  ([rcPath czLoader]
    (if (nil? rcPath) nil (.getResourceAsStream (get-czldr czLoader) rcPath))) )

(defn rc-url ^{ :doc "Load the resource as URL." }
  ([rcPath] (rc-url rcPath nil))
  ([rcPath czLoader]
    (if (nil? rcPath) nil (.getResource (get-czldr czLoader) rcPath))) )

(defn rc-str ^{ :doc "Load the resource as string." }
  ([rcPath encoding] (rc-str encoding nil))
  ([rcPath] (rc-str rcPath "utf-8" nil))
  ([rcPath encoding czLoader]
    (with-open [ inp (rc-stream rcPath czLoader) ]
      (stringify (IOUtils/toByteArray inp) encoding ))) )

(defn rc-bytes ^{ :doc "Load the resource as byte[]." }
  ([rcPath] (rc-bytes rcPath nil))
  ([rcPath czLoader]
    (with-open [ inp (rc-stream rcPath czLoader) ]
      (IOUtils/toByteArray inp))) )

(defn deflate ^{ :doc "Compress the given byte[]." }
  [bits]
  (if (nil? bits)
    nil
    (let [ buf (byte-array 1024) cpz (Deflater.) ]
      (doto cpz
        (.setLevel (Deflater/BEST_COMPRESSION))
        (.setInput bits)
        (.finish))
      (with-open [ bos (ByteArrayOutputStream. (alength bits)) ]
        (loop []
          (if (.finished cpz)
            (.toByteArray bos)
            (do (.write bos buf 0 (.deflate cpz buf)) (recur))
          ))))) )

(defn inflate ^{ :doc "Decompress the given byte[]." }
  [bits]
  (if (nil? bits)
    nil
    (let [ buf (byte-array 1024) decr (Inflater.) baos (ByteArrayOutputStream. (alength bits)) ]
      (.setInput decr bits)
      (loop []
        (if (.finished decr)
            (.toByteArray baos)
            (do (.write baos buf 0 (.inflate decr buf)) (recur))
          )))) )

(defn normalize ^{ :doc "Normalize a filepath, hex-code all non-alpha characters." }
  [fname]
  (reduce
    (fn [buf ch]
      (when (or (java.lang.Character/isLetterOrDigit ch) (contains? _PUNCS ch))
        (.append buf ch)
        (.append buf (str "_0x" (Integer/toString (int ch) 16)) ))
      buf)
    (StringBuilder.)
    (seq fname)))

(defn now-millis ^{ :doc "Return the current time in milliseconds." }
  []
  (java.lang.System/currentTimeMillis))

(defn get-fpath ^{ :doc "Return the file path only." }
  [fileUrlPath]
  (if (nil? fileUrlPath)
    ""
    (.getPath (java.net.URL. fileUrlPath))) )

(defn fmt-fileurl ^{ :doc "Return the file path in file:/ format." }
  [path]
  (if (nil? path)
    ""
    (.toURL (.toURI (File. path)))))

(defn- fetch-tmpdir [extra]
  (let [ fp (File. (str (sysprop "java.io.tmpdir") "/" extra) ) ]
    (.mkdirs fp)
    fp))

(defn make-tmpdir ^{ :doc "Generate and return a new temp File dir." } [] (fetch-tmpdir (uid)))

(defn get-tmpdir ^{ :doc "Return the current temp File dir." } [] (fetch-tmpdir ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test and assert funcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^{ :doc "Tests if object is subclass of parent." } test-isa
  (fn [a b c] (cond (instance? Class b) :class :else :object)))

(defmethod test-isa :class
  [param childz parz]
  (assert (and (not (nil? childz)) (.isAssignableFrom parz childz))
        (str "" param " not-isa " (.getName parz)) ))

(defmethod test-isa :object
  [param obj parz]
  (assert (and (not (nil? obj)) (.isAssignableFrom parz (.getClass obj)))
        (str "" param " not-isa " (.getName parz)) ))

(defn tst-nonil ^{ :doc "Assert object is not null." }
  [param obj]
  (assert (not (nil? obj)) (str "" param " is null.")))

(defn tst-cond ^{ :doc "Assert a condition." }
  [c msg]
  (assert (= c true) (str msg)))

(defn tst-nestr ^{ :doc "Assert string is not empty." }
  [param v]
  (assert (not (StringUtils/isEmpty v)) (str "" param " is empty.")))

(defmulti ^{ :doc "Assert number is not negative." } tst-nonegnum
  (fn [a b]
    (cond
      (instance? Double b) :double
      (instance? Long b) :long
      (instance? Float b) :double
      (instance? Integer b) :long
      :else (throw (IllegalArgumentException. "allow numbers only")))))

(defmulti ^{ :doc "Assert number is positive." } tst-posnum
  (fn [a b]
    (cond
      (instance? Double b) :double
      (instance? Long b) :long
      (instance? Float b) :double
      (instance? Integer b) :long
      :else (throw (IllegalArgumentException. "allow numbers only")))))

(defmethod tst-nonegnum :double
  [param v]
  (assert (>= v 0.0) (str "" param " must be >= 0.")))

(defmethod tst-nonegnum :long
  [param v]
  (assert (>= v 0) (str "" param " must be >= 0.")))

(defmethod tst-posnum :double
  [param v]
  (assert (> v 0.0) (str "" param " must be > 0.")))

(defmethod tst-posnum :long
  [param v]
  (assert (> v 0) (str "" param " must be > 0.")))

(defn tst-neseq ^{ :doc "Assert sequence is not empty." }
  [param v]
  (assert (not (nil? (not-empty v))) (str  param  " must be non empty.") ))

(defn throw-badarg ^{ :doc "Force throw a bad parameter exception." }
  [msg]
  (throw (IllegalArgumentException. msg)))

(defn root-cause ^{ :doc "Dig into error and find the root exception." }
  [root]
  (loop [r root t (if (nil? root) nil (.getCause root)) ]
    (if (nil? t)
      r
      (recur t (.getCause t)) )))

(defn root-causemsg ^{ :doc "Dig into error and find the root exception message." }
  [root]
  (let [ e (root-cause root) ]
    (if (nil? e) "" (str (.getName (.getClass e)) ": " (.getMessage e)))))

(defn gen-numbers ^{ :doc "Return a list of random int numbers between a range." }
  [start end howMany]
  (if (or (>= start end) (< (- end start) howMany) )
    []
    (let [ _end (if (< end Integer/MAX_VALUE) (+ end 1) end )
           r (new-random) ]
      (loop [ rc [] cnt howMany ]
        (if (<= cnt 0)
          rc
          (let [ n (.nextInt r _end) ]
            (if (and (>= n start) (not (contains? rc n)))
              (recur (conj rc n) (dec cnt))
              (recur rc cnt) )))))) )

(defn sort-join ^{ :doc "Sort a list of strings and then concatenate them." }
  ([ss] (sort-join "" ss))
  ([sep ss] (if (nil? ss) "" (clojure.string/join sep (sort ss)))))

















(def ^:private coreutils-eof nil)

