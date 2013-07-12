;; COPYRIGHT (C) 2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
;;
;; THIS IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
;; MODIFY IT UNDER THE TERMS OF THE APACHE LICENSE,
;; VERSION 2.0 (THE "LICENSE").
;;
;; THIS LIBRARY IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
;; BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
;;
;; SEE THE LICENSE FOR THE SPECIFIC LANGUAGE GOVERNING PERMISSIONS
;; AND LIMITATIONS UNDER THE LICENSE.
;;
;; You should have received a copy of the Apache License
;; along with this distribution; if not, you may obtain a copy of the
;; License at
;; http://www.apache.org/licenses/LICENSE-2.0
;;

(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.blason.etc.core
  (:gen-class))

(use '[clojure.tools.logging :only (info debug)])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(require '[comzotohcljc.util.constants :as CS])
(require '[comzotohcljc.i18n.i18nutils :as LU])

(def ^:private CMDLINE-INFO [
  ["create web[/jetty]"  "e.g. create app as a webapp."]
  ["create"  "e.g. create an app."]
  ["podify <app-name>"  "e.g. package app as a pod file"]

  ["ide eclipse <app-name>" "Generate eclipse project files."]
  ["build <app-name> [target]" "Build app."]
  ["test <app-name>" "Run test cases."]

  ["debug" "Start & debug the application."]
  ["start [bg]" "Start the application."]

  ["generate serverkey" "Create self-signed server key (pkcs12)."]
  ["generate password" "Generate a random password."]
  ["generate csr" "Create a Certificate Signing Request."]
  ["encrypt <password> <clear-text>" "e.g. encrypt SomeSecretData"]
  ["decrypt <password> <cipher-text>" "e.g. decrypt Data"]
  ["testjce" "Check JCE  Policy Files."]

  ["demo samples" "Generate a set of samples."]
  ["version" "Show version info."] ])

(defprotocol AppRunnerAPI
  ())
(deftype AppRunner [] AppRunnerAPI
  (start [this args]
    (if (not (parse-args this args))
      (usage this)
      nil))
  (usage [_]
    (println (SU/make-string \= 78))
    (println "> blason <commands & options>")
    (println "> -----------------")
    drawHelpLines("> %-35s\' %s\n", a)
    println(">")
    println("> help - show standard commands")
    println(mkString('=',78))
  }



      //println("#### apprunner loader = " + getClass().getClassLoader().getClass().getName())
      //println("#### sys loader = " + ClassLoader.getSystemClassLoader().getClass().getName())
;; arg(0) is blason-home
(defn- parseArgs [rcb args]
  (let [ home (CU/trim-lastPathSep (nice-fpath (nth args 0)))
         h (File. home)
         c (CmdArgs. h (CU/getcwd) rcb) ]
    (if (not (FU/dir-read? h))
      false
      (if (not (-> (.getCmds c)(.contains (nth args 1))))
        false
        (try (.eval c ( Arrays.copyOfRange(args, 1, args.size)); true } catch { case _:Throwable => false


(defn -main ^{ :doc "" }
  [& args]
  (alter-var-root #'*read-eval* (constantly false))
  (let [ rcb (LU/get-resource "comzotohcljc/blason/etc/Resources" (Locale/getDefault)) ]
    (if (or (< (.size args) 2) (not (parseArgs rcb args)))
      (usage)
      nil)))





  ;;(info "hello" 23 "is me!")
  ;;(println (CS/COPYRIGHT)) )






