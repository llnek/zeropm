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

(ns ^{  :doc "Utility functions for class related or reflection related operations."
        :author "kenl" }
  comzotohcljc.util.metautils)


(import '(java.lang.reflect Field Method Modifier))
(require '[ comzotohcljc.util.coreutils :as CU ] )
(require '[ comzotohcljc.util.strutils :as SU ] )

(defmulti ^{ :doc "Returns true if clazz is subclass of this base class." } is-child
  (fn [a b]
    (cond
      (instance? Class b) :class
      :else :object)))

(defmethod is-child :class
  [^Class basz ^Class cz]
  (if (or (nil? basz) (nil? cz)) false (.isAssignableFrom basz cz)))

(defmethod is-child :object
  [^Class basz obj]
  (if (or (nil? basz) (nil? obj)) false (is-child basz (.getClass obj))))

(defn bytes-class ^{ :doc "Return the java class for byte[]." }
  []
  (Class/forName "[B"))

(defn chars-class ^{ :doc "Return the java class for char[]." }
  []
  (Class/forName "[C"))

(defn is-boolean? ^{ :doc "True if class is Boolean." }
  [^Class classObj]
  (if (SU/eq-any? (.getName classObj) ["boolean" "Boolean" "java.lang.Boolean"] ) true false))

(defn is-char? ^{ :doc "True if class is Char." }
  [^Class classObj]
  (if (SU/eq-any? (.getName classObj) [ "char" "Char" "java.lang.Character" ] ) true false))

(defn is-int? ^{ :doc "True if class is Int." }
  [^Class classObj]
  (if (SU/eq-any? (.getName classObj) [ "int" "Int" "java.lang.Integer" ] ) true false))

(defn is-long? ^{ :doc "True if class is Long." }
  [^Class classObj]
  (if (SU/eq-any? (.getName classObj) [ "long" "Long" "java.lang.Long" ] ) true false))

(defn is-float? ^{ :doc "True if class is Float." }
  [^Class classObj]
  (if (SU/eq-any? (.getName classObj) [ "float" "Float" "java.lang.Float" ]) true false ))

(defn is-double? ^{ :doc "True if class is Double." }
  [^Class classObj]
  (if (SU/eq-any? (.getName classObj) [ "double" "Double" "java.lang.Double" ]) true false))

(defn is-byte? ^{ :doc "True if class is Byte." }
  [^Class classObj]
  (if (SU/eq-any? (.getName classObj) [ "byte" "Byte" "java.lang.Byte" ]) true false ))

(defn is-short? ^{ :doc "True if class is Short." }
  [^Class classObj]
  (if (SU/eq-any? (.getName classObj) [ "short" "Short" "java.lang.Short" ]) true false))

(defn is-string? ^{ :doc "True if class is String." }
  [^Class classObj]
  (if (SU/eq-any? (.getName classObj) [ "String" "java.lang.String" ]) true false) )

(defn is-bytes? ^{ :doc "True if class is byte[]." }
  [^Class classObj]
  (= classObj (bytes-class)) )

(defn for-name ^{ :doc "Load a java class by name." }
  ( [^String z] (for-name z nil))
  ( [^String z ^ClassLoader cl]
    (if (nil? cl) (java.lang.Class/forName z) (java.lang.Class/forName z true cl))) )

(defn get-cldr ^{ :doc "Get the current classloader." }
  ([] (get-cldr nil))
  ([^ClassLoader cl] (if (nil? cl) (.getContextClassLoader (Thread/currentThread)) cl)))

(defn set-cldr ^{ :doc "Set current classloader." }
  [^ClassLoader cl]
  (let []
    (CU/test-nonil "class-loader" cl)
    (.setContextClassLoader (Thread/currentThread) cl)))

(defn load-class ^{ :doc "Load this class by name." }
  ( [^String clazzName] (load-class clazzName nil))
  ( [^String clazzName ^ClassLoader cl]
    (if (not (SU/hgl? clazzName)) nil (.loadClass (get-cldr cl) clazzName))))

(defn- ctorObj [cz]
  (do
    (CU/test-nonil "java-class" cz)
    (.newInstance (.getDeclaredConstructor cz (make-array Class 0))  (make-array Object 0)  )))

(defn make-obj ^{ :doc "Make an object of this class by calling the default constructor." }
  ([^String clazzName] (make-obj clazzName nil))
  ([^String clazzName ^ClassLoader cl]
   (if (not (SU/hgl? clazzName)) nil (ctorObj (load-class clazzName cl)))) )

(defn list-parents ^{ :doc "List all parent classes." }
  [^Class javaClass]
  (let [ rc (loop [ sum [] par javaClass ]
              (if (nil? par)
                sum
                (recur (conj sum par) (.getSuperclass par))))  ]
    ;; since we always add the original class, we need to ignore it on return
    (if (> (.size rc) 1) (rest rc) [] )))


(defn- iterXXX [ ^Class cz level getDeclXXX bin ]
  (let [ props (getDeclXXX cz) ]
    (reduce (fn [sum m]
              (let [ x (.getModifiers m) ]
                (if (and (> level 0)
                         (or (Modifier/isStatic x) (Modifier/isPrivate x)) )
                  sum
                  (assoc sum (.getName m) m)))) bin props) ))

(defn- listMtds [ ^Class cz level ]
  (let [ par (.getSuperclass cz) ]
    (iterXXX cz level #(.getDeclaredMethods %) (if (nil? par) {} (listMtds par (inc level))))))

(defn- listFlds [ ^Class cz level ]
  (let [ par (.getSuperclass cz) ]
    (iterXXX cz level #(.getDeclaredFields %) (if (nil? par) {} (listFlds par (inc level))))))

(defn list-methods ^{ :doc "List all methods belonging to this class, including inherited ones." }
  [^Class javaClass]
  (vals (if (nil? javaClass) {} (listMtds javaClass 0 ))) )

(defn list-fields ^{ :doc "List all fields belonging to this class, including inherited ones." }
  [^Class javaClass]
  (vals (if (nil? javaClass) {} (listFlds javaClass 0 ))) )










(def ^:private metautils-eof nil)

