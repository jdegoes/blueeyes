package blueeyes.core.http 

import blueeyes.util.ProductPrefixUnmangler

sealed abstract class HttpVersion extends ProductPrefixUnmangler {
  
  def versionNum = unmangledName.split("/")(1)
  def value = unmangledName 

  override def toString = value
}

object HttpVersions {

  def parseByVersionNum(inString: String): Option[HttpVersion] = { 
    def outHttpVers = inString.trim match {
      case "1.1" => Some(`HTTP/1.1`)
      case "1.0" => Some(`HTTP/1.1`)
      case _ => None
    }
    return outHttpVers
  }

  case object `HTTP/1.0` extends HttpVersion
  case object `HTTP/1.1` extends HttpVersion

}
