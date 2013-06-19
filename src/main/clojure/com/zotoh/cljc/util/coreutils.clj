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
    InputStream File FileInputStream ByteArrayInputStream ByteArrayOutputStream))
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

(defn- nsb [s]  (if (nil? s) (str "") s))

(deftype NICHTS [])
(def ^:dynamic *NICHTS* (->NICHTS ))

(defn nilToNichts
  "Returns null object to the internal NICHTS singleton."
  [obj]
  (if (nil? obj) *NICHTS* obj))

(defn isNichts?
  "Returns true if this object is the internal NICHTS singleton."
  [obj]
  (identical? obj *NICHTS*))

(defn matchChar?
  "Returns true if this char exists inside this set of chars."
  [ch setOfChars]
  (if (nil? setOfChars) false (contains? setOfChars ch)))

(defn sysVar
  "Get value for this system property."
  [v]
  (if (StringUtils/isEmpty v) nil (System/getProperty v)))

(defn envVar
  "Get value for this env var."
  [v]
  (if (StringUtils/isEmpty v) nil (System/getenv v)))

(defn uid
  "Generate a unique id using std java."
  []
  (.replaceAll (.toString (UID.)) "[:\\-]+" ""))

(defn- getCZldr
  "Get the current java class loader."
  ([] (getCZldr nil) )
  ([cl]
    (if (nil? cl) (.getContextClassLoader (Thread/currentThread)) cl )))

(defn newRandom
  "Return a new random object."
  []
  (SecureRandom. (SecureRandom/getSeed 20)) )

(defn nowJTS
  "Return a sql Timestamp."
  []
  (Timestamp. (.getTime (Date.))))

(defn nowJDate
  "Return a java Date."
  []
  (Date. ) )

(defn nowCal
  "Return a Gregorian Calendar."
  []
  (GregorianCalendar. ))

(defn charSet
  "Return a java Charset of the encoding."
  ([] (charSet "utf-8"))
  ([enc] (Charset/forName enc)) )

(defmulti niceFPath class)

(defmethod ^{ :doc "Convert the file path into nice format without backslashes."  } niceFPath String
  [fpath]
  (FilenameUtils/normalizeNoEndSeparator (nsb fpath) true) )

(defmethod ^{ :doc "Convert the file path into nice format without backslashes." } niceFPath File
  [aFile]
  (if (nil? aFile) "" (niceFPath (.getCanonicalPath aFile)) ))

(defn filterVar
  "Replaces all the variables in the given value with their matching values from the system and env vars."
  [value]
  (if (nil? value) ""
    (.replace (StrSubstitutor. (System/getenv)) (StrSubstitutor/replaceSystemProperties value))))

(defn filterSysVar
  "Expand any sys-var found inside the string value."
  [value]
  (if (nil? value) "" (StrSubstitutor/replaceSystemProperties value)) )

(defn filterEnvVar
  "Expand any env-var found inside the string value."
  [value]
  (if (nil? value) "" (.replace (StrSubstitutor. (System/getenv)) value)) )

(defn filterExpandVars
  "Expand any env & sys vars found inside the property values."
  [ ^Properties props ]
  (reduce
    (fn [bc k]
      (.put bc k (filterVar (.get props k))) bc )
    (Properties.) (.keySet props) ))

(defn sysProp
  "Get the value of a system property."
  [prop]
  (System/getProperty (nsb prop) ""))

(defn userHomeDir
  "Get the user's home directory."
  []
  (File. (sysProp "user.home")) )

(defn userName
  "Get the current user login name."
  []
  (sysProp "user.name"))

(defn getCwd
  "Get the current dir."
  []
  (sysProp "user.dir"))

(defn trimLastPathSep
  "Get rid of trailing dir paths."
  [path]
  (.replaceFirst (nsb path) "[/\\\\]+$"  ""))

(defn serialize
  "Object serialization."
  [obj]
  (if (nil? obj) nil (SerializationUtils/serialize obj)) )

(defn deserialize
  "Object deserialization."
  [bits]
  (if (nil? bits) nil (SerializationUtils/deserialize bits)))

(defn safeGetClzname
  "Get the object's class name."
  [obj]
  (if (nil? obj) "null" (.getName (.getClass obj))))

(defn filePath
  "Get the file path."
  [aFile]
  (if (nil? aFile) "" (niceFPath aFile)))

(defn isWindows?
  "Returns true if platform is windows."
  []
  (>= (.indexOf (.toLowerCase (sysProp "os.name")) "windows") 0 ))

(defn isUnix?
  "Returns true if platform is *nix."
  []
  (not isWindows?))

(defn convLong
  "Parse string as a long value."
  [ s dftLongVal ]
  (try
    (Long/parseLong s)
    (catch Throwable e dftLongVal)))

(defn convInt
  "Parse string as an int value."
  [ s dftIntVal ]
  (try
    (Integer/parseInt s)
    (catch Throwable e dftIntVal)))

(defn convDouble
  "Parse string as a double value."
  [ s dftDblVal ]
  (try
    (Double/parseDouble s)
    (catch Throwable e dftDblVal)))

(defn convFloat
  "Parse string as a double value."
  [ s dftFltVal ]
  (try
    (Float/parseFloat s)
    (catch Throwable e dftFltVal)))

(defn convBool
  "Parse string as a boolean value."
  [s]
  (contains? _BOOLS (.toLowerCase (nsb s))))

(defmulti loadJavaProps class )

(defmethod ^{ :doc "Load java properties from input-stream." } loadJavaProps InputStream
  [inp]
  (let [ ps (Properties.) ]
    (.load ps inp)
    ps))

(defmethod ^{ :doc "Load java properties from file." } loadJavaProps File
  [aFile]
  (with-open [ inp (FileInputStream. aFile) ]
    (loadJavaProps inp)))

(defn mkString
  "Make a string from bytes."
  ([bits] (mkString bits "utf-8"))
  ([bits encoding]
    (if (nil? bits) (String. bits encoding))))

(defn getBytes
  "Get bytes with the right encoding."
  ([s] (getBytes s "utf-8"))
  ([s encoding]
    (if (nil? s) nil (.getBytes s encoding)
    )))

(defn rc2Stream
  "Load the resource as stream."
  ([rcPath] (rc2Stream rcPath nil))
  ([rcPath czLoader]
    (if (nil? rcPath) nil (.getResourceAsStream (getCZldr czLoader) rcPath))) )

(defn rc2Url
  "Load the resource as URL."
  ([rcPath] (rc2Url rcPath nil))
  ([rcPath czLoader]
    (if (nil? rcPath) nil (.getResource (getCZldr czLoader) rcPath))) )

(defn rc2Str
  "Load the resource as string."
  ([rcPath] (rc2Str rcPath "utf-8" nil))
  ([rcPath encoding] (rc2Str encoding nil))
  ([rcPath encoding czLoader]
    (with-open [ inp (rc2Stream rcPath czLoader) ]
      (mkString (IOUtils/toByteArray inp) encoding ))) )

(defn rc2Bytes
  "Load the resource as byte[]."
  ([rcPath] (rc2Bytes rcPath nil))
  ([rcPath czLoader]
    (with-open [ inp (rc2Stream rcPath czLoader) ]
      (IOUtils/toByteArray inp))) )

(defn deflate
  "Compress the given byte[]."
  [bits]
  (if (nil? bits) nil
    (let [ buf (byte-array 1024) cpz (Deflater.) ]
      (.setLevel cpz (Deflater/BEST_COMPRESSION))
      (.setInput cpz bits)
      (.finish cpz)
      (with-open [ bos (ByteArrayOutputStream. (alength bits)) ]
        (loop []
          (if (.finished cpz)
            (.toByteArray bos)
            (do (.write bos buf 0 (.deflate cpz buf)) (recur))
          ))))) )

(defn inflate
  "Decompress the given byte[]."
  [bits]
  (if (nil? bits) nil
    (let [ buf (byte-array 1024) dec (Inflater.) ]
      (.setInput dec bits)
      (with-open [ bos (ByteArrayOutputStream. (alength bits)) ]
        (loop []
          (if (.finished dec)
            (.toByteArray bos)
            (do (.write bos buf 0 (.inflate dec buf)) (recur))
          ))))) )

(defn normalize
  "Normalize a filepath, hex-code all non-alpha characters."
  [fname]
  (reduce
    (fn [buf ch]
      (when (or (java.lang.Character/isLetterOrDigit ch) (contains? _PUNCS ch))
        (.append buf ch)
        (.append buf (str "_0x" (Integer/toString (int ch) 16)) ))
      buf)
    (StringBuilder.)
    (seq fname)))

(defn nowMillis
  "Return the current time in milliseconds."
  []
  (java.lang.System/currentTimeMillis))

(defn asFilePathOnly
  "Return the file path in file:/ format."
  [fileUrlPath]
  (if (nil? fileUrlPath)
    ""
    (.getPath (java.net.URL. fileUrlPath))) )

(defn asFileUrl
  "Return the file path in file:/ format."
  [path]
  (if (nil? path)
    ""
    (.toURL (.toURI (File. path)))))

(defn- fetchTmpDir
  "Create a temp dir."
  [extra]
  (let [ fp (File. (str (sysProp "java.io.tmpdir") "/" extra) ) ]
    (.mkdirs fp)
    fp))

(defn genTmpDir
  "Generate and return a new temp File dir."
  []
  (fetchTmpDir (uid)))

(defn tmpDir
  "Return the current temp File dir."
  []
  (fetchTmpDir ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test and assert funcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tstClassISA
  "Tests if class is subclass of parent."
  [param childz parz]
  (assert (and (not (nil? childz)) (.isAssignableFrom parz childz))
        (str "" param " not-isa " (.getName parz)) ))

(defn tstObjISA
  "Tests if object's class is subclass of parent."
  [param obj parz]
  (assert (and (not (nil? obj)) (.isAssignableFrom parz (.getClass obj)))
        (str "" param " not-isa " (.getName parz)) ))

(defn tstCond
  "Assert a condition."
  [c msg]
  (assert (= c true) (str msg)))

(defn tstObjArg
  "Assert object is not null."
  [param obj]
  (assert (not (nil? obj)) (str "" param " is null.")))

(defn tstEStrArg
  "Assert string is not empty."
  [param v]
  (assert (not (StringUtils/isEmpty v)) (str "" param " is empty.")))

(defn tstNonNegNum
  "Assert number is not negative."
  [param v]
  (assert (>= v 0) (str "" param " must be >= 0.")))

(defn tstPosNum
  "Assert number is > 0"
  [param v]
  (assert (> v 0) (str "" param " must be > 0.")))

(defn tstNESeq
  "Assert sequence is not empty."
  [param v]
  (assert (not (nil? (not-empty v))) (str  param  " must be non empty.") ))

(defn errBadArg
  "Force throw an exception."
  [msg]
  (throw (IllegalArgumentException. msg)))

(defn findRootCause
  "Dig into error and find the root exception."
  [root]
  (loop [r root t (if (nil? root) nil (.getCause root)) ]
    (if (nil? t)
      r
      (recur t (.getCause t)) )))

(defn findRootCauseMsgWithClassInfo
  "Dig into error and find the root exception message."
  [root]
  (let [ e (findRootCause root) ]
    (if (nil? e) "" (str (.getName (.getClass e)) ": " (.getMessage e)))))

(defn findRootCauseMsg
  "Dig into error and find the root exception message."
  [root]
  (let [ e (findRootCause root) ]
    (if (nil? e) "" (.getMessage e))))

(defn genNumsBetween
  "Return a list of random int numbers."
  [start end howMany]
  (if (or (>= start end) (< (- end start) howMany) )
    []
    (let [ _end (if (< end Integer/MAX_VALUE) (+ end 1) end )
           r (newRandom) ]
      (loop [ rc [] cnt howMany ]
        (if (<= cnt 0)
          rc
          (let [ n (.nextInt r _end) ]
            (if (and (>= n start) (not (contains? rc n)))
              (recur (conj rc n) (dec cnt))
              (recur rc cnt) )))))) )

(defn sortAndJoin
  "Sort a list of strings and then concatenate them."
  ([ss] (sortAndJoin "" ss))
  ([sep ss]
    (if (nil? ss)
      ""
      (clojure.string/join sep (sort ss)))))

















(def ^:private coreutils-eof nil)

