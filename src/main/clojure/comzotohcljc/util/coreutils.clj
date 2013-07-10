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

(ns ^{ :doc "General utilties." :author "kenl" }
  comzotohcljc.util.coreutils)

(use '[clojure.tools.logging :only (info warn error debug)])
;;(:use [clojure.string])
(import '(java.security SecureRandom))
(import '(java.net URL))
(import '(java.nio.charset Charset))
(import '(java.io
  InputStream File FileInputStream
  ByteArrayInputStream ByteArrayOutputStream))
(import '(java.util Properties Date GregorianCalendar TimeZone))
(import '(java.util.zip DataFormatException Deflater Inflater))
(import '(java.sql Timestamp))
(import '(java.rmi.server UID))
(import '(org.apache.commons.lang3.text StrSubstitutor))
(import '(org.apache.commons.lang3 StringUtils))
(import '(org.apache.commons.io IOUtils FilenameUtils))
(import '(org.apache.commons.lang3 SerializationUtils))



(def ^:private _BOOLS #{ "true" "yes"  "on"  "ok"  "active"  "1"} )
(def ^:private _PUNCS #{ \_ \- \. \( \) \space } )

(defmacro TryC
  [ & exprs ]
  `(try (do ~@exprs) (catch Throwable e# nil)) )

(defn- nsb [s] (if (nil? s) (str "") s))

(defn- get-czldr
  ([] (get-czldr nil) )
  ([^ClassLoader cl]
    (if (nil? cl) (.getContextClassLoader (Thread/currentThread)) cl)))

(deftype NICHTS [])
(def ^:dynamic *NICHTS* (->NICHTS))

(defn nil-nichts [obj] (if (nil? obj) *NICHTS* obj))

(defn is-nichts? [obj] (identical? obj *NICHTS*))

(defn flatten-nil ^{ :doc "" }
  [vs]
  (cond
    (nil? vs) nil
    (empty? vs) []
    :else (into [] (remove nil? vs))))

(defn ndz [^double d] (if (nil? d) 0.0 d))
(defn nnz [^long n] (if (nil? n) 0 n))

(defn match-char? ^{ :doc "Returns true if this char exists inside this set of chars." }
  [ch setOfChars]
  (if (nil? setOfChars) false (contains? setOfChars ch)))

(defn sysvar ^{ :doc "Get value for this system property." }
  [^String v]
  (if (StringUtils/isEmpty v) nil (System/getProperty v)))

(defn envvar ^{ :doc "Get value for this env var." }
  [^String v]
  (if (StringUtils/isEmpty v) nil (System/getenv v)))

(defn uid ^{ :doc "Generate a unique id using std java." }
  []
  (.replaceAll (.toString (UID.)) "[:\\-]+" ""))

(defn new-random ^{ :doc "Return a new random object." }
  ([] (new-random 4))
  ([numBytes] (SecureRandom. (SecureRandom/getSeed numBytes)) ))

(defn now-jtstamp ^{ :doc "Return a java sql Timestamp." } [] (Timestamp. (.getTime (Date.))))

(defn now-date ^{ :doc "Return a java Date." } [] (Date.) )

(defn now-cal ^{ :doc "Return a Gregorian Calendar." } [] (GregorianCalendar. ))

(defn to-charset ^{ :doc "Return a java Charset of the encoding." }
  ([^String enc] (Charset/forName enc))
  ([] (to-charset "utf-8")) )

(defmulti ^{ :doc "Convert the file path into nice format without backslashes."  } nice-fpath class)

(defmethod nice-fpath String
  [^String fpath]
  (FilenameUtils/normalizeNoEndSeparator (nsb fpath) true))

(defmethod nice-fpath File
  [^File aFile]
  (if (nil? aFile) "" (nice-fpath (.getCanonicalPath aFile)) ))

(defn subs-var ^{ :doc "Replaces all system & env variables in the value." }
  [^String value]
  (if (nil? value)
    ""
    (.replace (StrSubstitutor. (System/getenv)) (StrSubstitutor/replaceSystemProperties value))))

(defn subs-svar ^{ :doc "Expand any sys-var found inside the string value." }
  [^String value]
  (if (nil? value) "" (StrSubstitutor/replaceSystemProperties value)) )

(defn subs-evar ^{ :doc "Expand any env-var found inside the string value." }
  [^String value]
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
  [^bytes bits]
  (if (nil? bits) nil (SerializationUtils/deserialize bits)))

(defn get-classname ^{ :doc "Get the object's class name." }
  [obj]
  (if (nil? obj) "null" (.getName (.getClass obj))))

(defn file-path ^{ :doc "Get the file path." }
  [^File aFile]
  (if (nil? aFile) "" (nice-fpath aFile)))

(defn is-windows? ^{ :doc "Returns true if platform is windows." }
  []
  (>= (.indexOf (.toLowerCase (sysprop "os.name")) "windows") 0 ))

(defn is-unix? ^{ :doc "Returns true if platform is *nix." }
  []
  (not (is-windows?)))

(defn conv-long ^{ :doc "Parse string as a long value." }
  [^String s ^long dftLongVal]
  (try (Long/parseLong s) (catch Throwable e dftLongVal)))

(defn conv-double ^{ :doc "Parse string as a double value." }
  [^String s ^double dftDblVal]
  (try (Double/parseDouble s) (catch Throwable e dftDblVal)))

(defn conv-bool ^{ :doc "Parse string as a boolean value." }
  [^String s]
  (contains? _BOOLS (.toLowerCase (nsb s))))

(defmulti ^{ :doc "Load java properties from input-stream." } load-javaprops class)

(defmethod load-javaprops InputStream
  [^InputStream inp]
  (doto (Properties.) (.load inp)))

(defmethod load-javaprops File
  [^File aFile]
  (load-javaprops (-> aFile (.toURI) (.toURL) )))

(defmethod load-javaprops URL
  [^URL aFile]
  (with-open [ inp (.openStream aFile) ]
    (load-javaprops inp)))

(defn stringify ^{ :doc "Make a string from bytes." }
  ([^bytes bits] (stringify bits "utf-8"))
  ([^bytes bits ^String encoding] (if (nil? bits) nil (String. bits encoding))))

(defn bytesify ^{ :doc "Get bytes with the right encoding." }
  ([^String s] (bytesify s "utf-8"))
  ([^String s ^String encoding] (if (nil? s) nil (.getBytes s encoding))))

(defn rc-stream ^{ :doc "Load the resource as stream." }
  ([^String rcPath] (rc-stream rcPath nil))
  ([^String rcPath ^ClassLoader czLoader]
    (if (nil? rcPath) nil (.getResourceAsStream (get-czldr czLoader) rcPath))) )

(defn rc-url ^{ :doc "Load the resource as URL." }
  ([^String rcPath] (rc-url rcPath nil))
  ([^String rcPath ^ClassLoader czLoader]
    (if (nil? rcPath) nil (.getResource (get-czldr czLoader) rcPath))) )

(defn rc-str ^{ :doc "Load the resource as string." }
  ([^String rcPath ^String encoding] (rc-str encoding nil))
  ([^String rcPath] (rc-str rcPath "utf-8" nil))
  ([^String rcPath ^String encoding ^ClassLoader czLoader]
    (with-open [ inp (rc-stream rcPath czLoader) ]
      (stringify (IOUtils/toByteArray inp) encoding ))) )

(defn rc-bytes ^{ :doc "Load the resource as byte[]." }
  ([^String rcPath] (rc-bytes rcPath nil))
  ([^String rcPath ^ClassLoader czLoader]
    (with-open [ inp (rc-stream rcPath czLoader) ]
      (IOUtils/toByteArray inp))) )

(defn deflate ^{ :doc "Compress the given byte[]." }
  [^bytes bits]
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
  [^bytes bits]
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
  [^String fname]
  (.toString (reduce
    (fn [buf ch]
      (if (or (java.lang.Character/isLetterOrDigit ch) (contains? _PUNCS ch))
        (.append buf ch)
        (.append buf (str "0x" (Integer/toString (int ch) 16)) ))
      buf)
    (StringBuilder.)
    (seq fname))))

(defn now-millis ^{ :doc "Return the current time in milliseconds." }
  []
  (java.lang.System/currentTimeMillis))

(defn get-fpath ^{ :doc "Return the file path only." }
  [^String fileUrlPath]
  (if (nil? fileUrlPath)
    ""
    (.getPath (java.net.URL. fileUrlPath))) )

(defn fmt-fileurl ^{ :doc "Return the file path as URL." }
  [^String path]
  (if (nil? path)
    nil
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
  [^String param ^Class childz ^Class parz]
  (assert (and (not (nil? childz)) (.isAssignableFrom parz childz))
        (str "" param " not-isa " (.getName parz)) ))

(defmethod test-isa :object
  [^String param obj ^Class parz]
  (assert (and (not (nil? obj)) (.isAssignableFrom parz (.getClass obj)))
        (str "" param " not-isa " (.getName parz)) ))

(defn test-nonil ^{ :doc "Assert object is not null." }
  [^String param obj]
  (assert (not (nil? obj)) (str "" param " is null.")))

(defn test-cond ^{ :doc "Assert a condition." }
  [^String msg cnd ]
  (assert (= cnd true) (str msg)))

(defn test-nestr ^{ :doc "Assert string is not empty." }
  [^String param v]
  (assert (not (StringUtils/isEmpty v)) (str "" param " is empty.")))

(defmulti ^{ :doc "Assert number is not negative." } test-nonegnum
  (fn [a b]
    (cond
      (instance? Double b) :double
      (instance? Long b) :long
      (instance? Float b) :double
      (instance? Integer b) :long
      :else (throw (IllegalArgumentException. "allow numbers only")))))

(defmulti ^{ :doc "Assert number is positive." } test-posnum
  (fn [a b]
    (cond
      (instance? Double b) :double
      (instance? Long b) :long
      (instance? Float b) :double
      (instance? Integer b) :long
      :else (throw (IllegalArgumentException. "allow numbers only")))))

(defmethod test-nonegnum :double
  [^String param v]
  (assert (>= v 0.0) (str "" param " must be >= 0.")))

(defmethod test-nonegnum :long
  [^String param v]
  (assert (>= v 0) (str "" param " must be >= 0.")))

(defmethod test-posnum :double
  [^String param v]
  (assert (> v 0.0) (str "" param " must be > 0.")))

(defmethod test-posnum :long
  [^String param v]
  (assert (> v 0) (str "" param " must be > 0.")))

(defn test-neseq ^{ :doc "Assert sequence is not empty." }
  [^String param v]
  (assert (not (nil? (not-empty v))) (str  param  " must be non empty.") ))

(defn throw-badarg ^{ :doc "Force throw a bad parameter exception." }
  [^String msg]
  (throw (IllegalArgumentException. msg)))

(defn root-cause ^{ :doc "Dig into error and find the root exception." }
  [^Throwable root]
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


(defn into-map ^{ :doc "" }
     [^Properties props]
       (reduce (fn [sum k]
                   (assoc sum k (.get props k))) {} (seq (.keySet props))))














(def ^:private coreutils-eof nil)

