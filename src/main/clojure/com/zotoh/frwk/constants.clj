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

(ns com.zotoh.frwk.constants)

(def TS_REGEX "^\\d\\d\\d\\d-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])\\s\\d\\d:\\d\\d:\\d\\d")
(def DT_REGEX "^\\d\\d\\d\\d-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$")
(def TS_FMT_NANO "yyyy-MM-dd HH:mm:ss.fffffffff" )
(def TS_FMT "yyyy-MM-dd HH:mm:ss")

(def DT_FMT_MICRO "yyyy-MM-dd' 'HH:mm:ss.SSS" )
(def DT_FMT "yyyy-MM-dd' 'HH:mm:ss" )
(def DATE_FMT "yyyy-MM-dd" )

(def ISO8601_FMT "yyyy-MM-dd' 'HH:mm:ss.SSSZ" )

(def USASCII "ISO-8859-1" )
(def UTF16 "UTF-16" )
(def UTF8 "UTF-8" )
(def SLASH   "/" )
(def PATHSEP   SLASH )

(def BOOLS #{ "true", "yes", "on", "ok", "active", "1"} )

(def MONTHS [ "JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" "AUG" "SEP" "OCT" "NOV" "DEC" ] )


(def COPYRIGHT "COPYRIGHT (C) 2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.")
(def ^:private constants-eof nil)


