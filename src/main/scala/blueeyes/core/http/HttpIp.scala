package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler
import scala.util.matching.Regex
import java.net.InetAddress

/* For use in the X-Forwarded-For Header */

sealed trait HttpIp {

  def ip: InetAddress

  def value: String = ip.getHostAddress

  override def toString = value

}


object HttpIps {

  def ipRegex = """((\d){1,3}\.){3}(\d){1,3}""".r

  def parseHttpIps(inString: String): Array[HttpIp] = { 

    def inetAddresses: Array[InetAddress] = ipRegex.findAllIn(inString).toArray.map(x => InetAddress.getByName(x)).filterNot(_ == null)
    def customIps: Array[HttpIp] = inetAddresses.map(x => CustomIP(x)) 
    return customIps

  }

  def parseSingleIp(inString: String): Option[HttpIp] = {
    
    def httpIp: Option[HttpIp] = ipRegex.findFirstIn(inString).map(x => InetAddress.getByName(x)) match {
      case None       => None
      case Some(null) => None
      case Some(d)    => Some(CustomIP(d))
    }

    return httpIp
  }

  case class CustomIP(ip: InetAddress) extends HttpIp

}

