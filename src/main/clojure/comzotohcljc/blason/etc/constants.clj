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

(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.blason.etc.constants)


(def SYS_DEVID_PFX "system.####")
(def SYS_DEVID_SFX "####")

(def SYS_DEVID_REGEX (str SYS_DEVID_PFX "[0-9A-Za-z_\\-\\.]+" SYS_DEVID_SFX))
(def SHUTDOWN_DEVID (str SYS_DEVID_PFX "kill_9" SYS_DEVID_SFX))
(def SHUTDOWN_URI "/kill9")

(def POD_PROTOCOL "pod:")
(def META_INF "META-INF")
(def POD_INF "POD-INF")
(def WEB_INF "WEB-INF")
(def DN_BLOCKS   "blocks")
(def DN_CORE "exec")
(def DN_CONF "conf")
(def DN_CLASSES "classes")
(def DN_LIB "lib")
(def DN_CFG "etc")
(def DN_BOXX "apps"    )
(def DN_PODS   "pods")
(def DN_LOGS "logs")
(def DN_TMP "tmp")
(def DN_DBS "dbs")
(def DN_DIST "dist")
(def DN_TEMPLATES   "templates")
(def DN_VIEWS   "views")
(def DN_PAGES   "pages")
(def DN_PATCH "patch")
(def DN_IMAGES "images")
(def DN_SCRIPTS "scripts")
(def DN_STYLES "styles")
(def DN_PUBLIC "public")

(def MN_FILE  (str META_INF "/" "MANIFEST.MF"))
(def POD_CLASSES  (str POD_INF "/" DN_CLASSES))
(def POD_PATCH (str POD_INF "/" DN_PATCH))
(def POD_LIB (str POD_INF "/" DN_LIB))

(def WEB_CLASSES (str WEB_INF "/" DN_CLASSES))
(def WEB_LIB  (str WEB_INF "/" DN_LIB))
(def WEB_LOG (str WEB_INF "/logs"))
(def WEB_XML (str WEB_INF "/web.xml"))

(def MN_RNOTES  (str META_INF "/" "RELEASE-NOTES.txt"))
(def MN_README  (str META_INF "/" "README.md"))
(def MN_NOTES  (str META_INF "/" "NOTES.txt"))
(def MN_LIC  (str META_INF "/" "LICENSE.txt"))

(def CFG_ENV_CF (str DN_CONF "/" "env.conf"))
(def CFG_APP_CF (str DN_CONF "/" "app.conf"))

(def PF_BLASON_APPDOMAIN "blason.app.domain")
(def PF_BLASON_APPID "blason.appid")
(def PF_BLASON_APPTASK "blason.app.task")

(def PF_JMXMGM "jmx-management")
(def PF_HOMEDIR "blason.home")
(def PF_PROPS "blason.conf")
(def PF_ROUTE_INFO "route.info")
(def PF_CLISH "cli-shell")
(def PF_COMPS "components")
(def PF_REGS "registries")
(def PF_KERNEL "kernel")
(def PF_EXECV "execvisor")
(def PF_DEPLOYER "deployer")
(def PF_BLOCKS "blocks")
(def PF_APPS "apps")
(def PF_SVCS "services")

(def K_ROOT_CZLR "root.loader")
(def K_EXEC_CZLR "exec.loader")

(def K_BASEDIR "base.dir")
(def K_PODSDIR "pods.dir")
(def K_CFGDIR "cfg.dir")
(def K_APPDIR "app.dir")
(def K_PLAYDIR "play.dir")
(def K_LOGDIR "log.dir")
(def K_TMPDIR "tmp.dir")
(def K_DBSDIR "dbs.dir")
(def K_BKSDIR "blocks.dir")

(def K_COUNTRY "country")
(def K_LOCALE "locale")
(def K_LANG "lang")

(def K_META "meta")

























(def ^:private constants-eof nil)

