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


  def parseHttpIps(inString: String): Array[HttpIp] = { 

    def ipRegex = """((\d){1,3}\.){3}(\d){1,3}""".r

    def inetAddresses: Array[InetAddress] = ipRegex.findAllIn(inString).toArray.map(x => InetAddress.getByName(x)).filterNot(_ == null)
    def customIps: Array[HttpIp] = inetAddresses.map(x => CustomIP(x)) 
    return customIps

  }

  case class CustomIP(ip: InetAddress) extends HttpIp

}

