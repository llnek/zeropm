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

(ns ^{ :doc "This is a utility class that provides various MIME related functionality." :author "kenl" }
  com.zotoh.cljc.crypto.cryptutils
  (:use [clojure.tools.logging :only (info warn error debug)])
  (:import (org.apache.commons.lang3 StringUtils))
  (:import (java.net URLDecoder URLEncoder))
  (:import (java.io InputStream File))
  (:import (java.net URL))
  (:import (java.util.regex Pattern))
  (:import (java.util Properties))
  (:import (javax.mail Message))
  )

(defn getCharset "" [ct]
    var pos = nsb(ct).indexOf("charset=")
//    var rc= "ISO-8859-1"
    var rc="utf-8"
    if (pos > 0) {
      val s = ct.substring(pos+8)
      pos = STU.indexOfAny(s, "; \t\r\n")
      if (pos > 0) {
        rc=s.substring(0, pos)
      }
    }
    rc
  }
  
  /**
   * @param cType
   * @return
   */
  def isSigned(cType:String) = {
    val ct= nsb(cType).lc
    //tlog().debug("MimeUte:isSigned: ctype={}", cType)
    ( ct.indexOf("multipart/signed") >=0 ) ||
          (isPKCS7mime(ct) && (ct.indexOf("signed-data") >=0) )
  }

  /**
   * @param cType
   * @return
   */
  def isEncrypted(cType:String) = {
    val ct= nsb(cType).lc
    //tlog().debug("MimeUte:isEncrypted: ctype={}", cType);
    (isPKCS7mime(ct)  &&  (ct.indexOf("enveloped-data") >= 0) )
  }

  /**
   * @param cType
   * @return
   */
  def isCompressed(cType:String) = {
    val ct= nsb(cType).lc
    //tlog().debug("MimeUte:isCompressed: ctype={}", cType);
    (ct.indexOf("application/pkcs7-mime") >= 0 ) &&
        (ct.indexOf("compressed-data") >= 0 )
  }

  /**
   * @param cType
   * @return
   */
  def isMDN(cType:String) = {
    val ct= nsb(cType).lc
    //tlog().debug("MimeUte:isMDN: ctype={}", cType);
    (ct.indexOf("multipart/report") >=0) &&
        (ct.indexOf("disposition-notification") >= 0)
  }

  /**
   * @param obj
   * @return
   * @throws Exception
   */
  def maybeAsStream(obj:Any) = {
    obj match {
      case b:Array[Byte] =>  asStream(b)
      case i:InputStream =>  i
      case s:String =>  asStream(asBytes(s))
      case _ => null
    }
  }

  /**
   * @param u
   * @return
   */
  def urlDecode(u:String) = {
    if (u==null) null else try {
      URLDecoder.decode(u, "UTF-8")
    } catch {
      case e:Throwable => null
    }
  }

  /**
   * @param u
   * @return
   */
  def urlEncode(u:String) = {
    if (u==null) null else try {
      URLEncoder.encode(u, "UTF-8")
    } catch {
      case e:Throwable => null
    }
  }

  private def isPKCS7mime(s:String) = {
    (s.indexOf("application/pkcs7-mime") >=0) ||
      (s.indexOf("application/x-pkcs7-mime") >=0)
  }

  def guessMimeType(file:File, dft:String = "" ) = {
    val matcher = _extRegex.matcher( file.getName().lc)
    val ext = if (matcher.matches()) {
      matcher.group(1)
    } else { 
      "" 
    }

    mimeCache().getProperty(ext) match {
      case s:String if !STU.isEmpty(s) => s
      case _ => dft
    }

  }

  def guessContentType(file:File, enc:String="utf-8", dft:String = "application/octet-stream" ) = {
    val ct = guessMimeType(file, "") match {
      case s:String if !STU.isEmpty(s) => s
      case _ => dft
    }
    if (! ct.startsWith("text/")) ct else {
      ct + "; charset=" + enc
    }
  }

  def test(mimeType:String ) = {
    mimeCache().contains( nsb(mimeType).split( ";") )
  }

  def mimeCache() = {
   _mimeCache 
  }

  def setupCache(file:URL) {
    _mimeCache = using(file.openStream ) { (inp) =>
      asQuirks(inp)      
    }
  }

  private val _extRegex = Pattern.compile("^.*\\.([^.]+)$")
  private var _mimeCache:JPS = null
  
}

sealed class MimeUtils {}

trait Email {

  def addRecipient(fld:Message.RecipientType, addrs:String ): Unit
  def attach(fp:File ): Unit
  def send(body:String , contentType:String, subType:String) : Unit
  def send(body:String ) : Unit

}


















(def ^:private mimeutils-eof nil)

