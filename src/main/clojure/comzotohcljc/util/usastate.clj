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

(ns ^{ :doc "A class that maps the state-code to the state-name." :author "kenl" }
  comzotohcljc.util.usastate )

(def ^:private _CCODES {
    "AL"  "Alabama"
    "AK"  "Alaska"
    "AZ"  "Arizona"
    "AR"  "Arkansas"
    "CA"  "California"
    "CO"  "Colorado"
    "CT"  "Connecticut"
    "DE"  "Delaware"
    "FL"  "Florida"
    "GA"  "Georgia"
    "HI"  "Hawaii"
    "ID"  "Idaho"
    "IL"  "Illinois"
    "IN"  "Indiana"
    "IA"  "Iowa"
    "KS"  "Kansas"
    "KY"  "Kentucky"
    "LA"  "Louisiana"
    "ME"  "Maine"
    "MD"  "Maryland"
    "MA"  "Massachusetts"
    "MI"  "Michigan"
    "MN"  "Minnesota"
    "MS"  "Mississippi"
    "MO"  "Missouri"
    "MT"  "Montana"
    "NE"  "Nebraska"
    "NV"  "Nevada"
    "NH"  "New Hampshire"
    "NJ"  "New Jersey"
    "NM"  "New Mexico"
    "NY"  "New York"
    "NC"  "North Carolina"
    "ND"  "North Dakota"
    "OH"  "Ohio"
    "OK"  "Oklahoma"
    "OR"  "Oregon"
    "PA"  "Pennsylvania"
    "RI"  "Rhode Island"
    "SC"  "South Carolina"
    "SD"  "South Dakota"
    "TN"  "Tennessee"
    "TX"  "Texas"
    "UT"  "Utah"
    "VT"  "Vermont"
    "VA"  "Virginia"
    "WA"  "Washington"
    "WV"  "West Virginia"
    "WI"  "Wisconsin"
    "WY"  "Wyoming"
})

(def ^:private _CCODESEQ (seq _CCODES))

(defn list-codes ^{ :doc "List all the abbreviated states." }
  []
  (keys _CCODES))

(defn find-state ^{ :doc "Return the full state name." }
  [^String code]
  (_CCODES (.toUpperCase code)))

(defn find-code ^{ :doc "Return the abbreviated state code." }
  [^String state]
  (let [ rs (filter #(= (nth % 1) state) _CCODESEQ) ]
      (if (nil? rs) nil (nth (first rs) 0))))


(def ^:private usastate-eof nil)

