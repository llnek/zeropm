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

(ns ^{ :doc "Date related utilities." :author "kenl" }
  com.zotoh.cljc.util.dateutils
  (:import (java.text ParsePosition SimpleDateFormat))
  (:import (java.util Locale TimeZone SimpleTimeZone
    Date Calendar GregorianCalendar))
  (:import (java.sql Timestamp))
  (:import (org.apache.commons.lang3 StringUtils))
  (:require [ com.zotoh.cljc.util.constants :as CS ])
  (:require [ com.zotoh.cljc.util.coreutils :as CU ])
  (:require [ com.zotoh.cljc.util.strutils :as SU ])
  )

(defn leap-year? ^{ :doc "Return true if this is a leap year." }
  [year]
  (cond (zero? (mod year 400)) true
    (zero? (mod year 100)) false
    :else (zero? (mod year 4))) )

(defn has-tz? ^{ :doc "Returns true if this datetime string contains some timezone info." }
  [dateStr]
  (let [ tkns (.split (SU/nsb dateStr) (if (SU/has? dateStr \:) CS/TS_REGEX CS/DT_REGEX )) ]
    (some (fn [s]
            (or (SU/has-any? (SU/nsb s) ["+" "-"])
            (.matches (SU/nsb s) "\\s*[A-Z]+\\s*"))) tkns)) )

(defn parse-timestamp ^{ :doc "Convert string into a valid Timestamp object.
  *tstr* conforming to the format \"yyyy-mm-dd hh:mm:ss.[fff...]\"" }
  [tstr]
  (try
    (Timestamp/valueOf tstr)
    (catch Throwable t nil)) )

(defn parse-date ^{ :doc "Convert string into a Date object." }
  [tstr fmt]
  (if (or (StringUtils/isEmpty tstr) (StringUtils/isEmpty fmt))
    nil
    (.parse (SimpleDateFormat. fmt) tstr)))

(defn parse-iso8601 ^{ :doc "Parses datetime in ISO8601 format." }
  [tstr]
  (if (StringUtils/isEmpty tstr)
    nil
    (let [ fmt (if (SU/has? tstr \:)
                  (let [ s (if (SU/has? tstr \.) CS/DT_FMT_MICRO CS/DT_FMT ) ]
                      (if (has-tz? tstr) (str s "Z") s))
                          CS/DATE_FMT ) ]
      (parse-date tstr fmt))))

(defn fmt-timestamp ^{ :doc "Convert Timestamp into a string value." }
  [ts]
  (if (nil? ts) "" (.toString ts)))

(defn fmt-date ^{ :doc "Convert Date into string value." }
  ( [dt] (fmt-date dt CS/DT_FMT_MICRO nil))
  ( [dt fmt] (fmt-date dt fmt nil))
  ( [dt fmt tz]
    (if (or (nil? dt) (StringUtils/isEmpty fmt))
      ""
      (let [ df (SimpleDateFormat. fmt) ]
        (if-not (nil? tz) (.setTimeZone df tz))
        (.format df dt)))) )

(defn fmt-gmt ^{ :doc "Convert Date object into a string - GMT timezone." }
  [dt]
  (do
    (fmt-date dt CS/DT_FMT_MICRO (SimpleTimeZone. 0 "GMT")) ))


(defn- add [cal calendarField amount]
  (if (nil? cal)
    nil
    (doto (GregorianCalendar. (.getTimeZone cal))
      (.setTime (.getTime cal))
      (.add calendarField amount))))

(defn add-years ^{ :doc "Add n more years to the calendar." }
  [cal yrs]
  (add cal Calendar/YEAR yrs))

(defn add-months ^{ :doc "Add n more months to the calendar." }
  [cal mts]
  (add cal Calendar/MONTH mts))

(defn add-days ^{ :doc "Add n more days to the calendar." }
  [cal days]
  (add cal Calendar/DAY_OF_YEAR days))

(defn fmt-cal ^{ :doc "Formats time to yyyyMMdd-hhmmss." }
  [cal]
  (do
    (java.lang.String/format (Locale/getDefault) "%1$04d%2$02d%3$02d-%4$02d%5$02d%6$02d"
       (into-array Object [
            (.get cal Calendar/YEAR)
            (+ 1 (.get cal Calendar/MONTH))
            (.get cal Calendar/DAY_OF_MONTH)
            (.get cal Calendar/HOUR_OF_DAY)
            (.get cal Calendar/MINUTE)
            (.get cal Calendar/SECOND) ] ))))

(defn debug-cal ^{ :doc "Debug show a calendar's internal data." }
  [cal]
  (do
    (clojure.string/join ""
        [ "{" (.. cal (getTimeZone) (getDisplayName) )  "} "
          "{" (.. cal (getTimeZone) (getID)) "} "
          "[" (.getTimeInMillis cal) "] "
          (fmt-cal cal) ])))








(def ^:private dateutils-eof nil)


