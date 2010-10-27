package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler
import scala.util.matching.Regex

/* For use in the X-Content-Type-Options Header */

sealed trait HttpIp {

  def ip: String  

  def value: String = ip

  override def toString = value

}


object HttpIps {

  def parseHttpIps(inString: String): Option[Array[HttpIp]] = { 

    def ipRegex = """(\d{1,3}\.){4}""".r
    def ips: Array[HttpIp] = ipRegex.findAllIn(inString).toArray.map(CustomIP(_))
    return Some(ips)

  }

  case class CustomIP(ip: String) extends HttpIp

}

