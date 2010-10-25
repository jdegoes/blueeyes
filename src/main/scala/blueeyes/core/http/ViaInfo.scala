package blueeyes.core.http

/* For use in the ViaHTTP Header */

sealed trait ViaInfo {
   
  def httpVersion: HttpVersion
  def receivedBy: String

  def value: String = (httpVersion :: receivedBy :: Nil).mkString(" ")
  override def toString = value
}

object ViaInfos {

  def parseViaInfos(inString: String): Array[ViaInfo] = { 
    def parsedInfos: Array[ViaInfo] = inString.split(",").map(_.trim.split(" ") match {
      case Array(x, y, z) => CustomViaInfo(HttpVersions.parseByVersionNum(x), y)
      case Array(x, y) => CustomViaInfo(HttpVersions.parseByVersionNum(x), y)
      case _ => NullViaInfo(HttpVersions.`HTTP/1.1`, "name")
    })
    return parsedInfos
  }

  case class CustomViaInfo(httpVersion: HttpVersion, receivedBy: String) extends ViaInfo
  object CustomViaInfo {
    def apply(ver: Option[HttpVersion], name: String): ViaInfo = {
      if (ver == None) 
        NullViaInfo(HttpVersions.`HTTP/1.1`, "name")
      else 
        CustomViaInfo(ver.get, name)
    }
  }

  case class NullViaInfo(httpVersion: HttpVersion, receivedBy: String) extends ViaInfo {
    override def value = ""
  }
}



