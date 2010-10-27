package blueeyes.core.http

import java.net.URI
import scala.util.matching.Regex


sealed trait HttpDomain {

  def uri: URI
  def port: Option[Int] 
  def value = (uri.getHost.toList ++ port.toList.map(_.toString) :: Nil).mkString(":")
  override def toString = value
  def absoluteUri = uri.getPath

}

object HttpDomains {

  def parseHttpDomains(inString: String): Option[HttpDomain] = {

    def domain: URI = new URI(inString.split(":")(0))
    if (domain == null)
      return None
    def port: Option[Int] = """:\d+""".r.findFirstIn(inString).map(_.toInt)
    return Some(CustomDomain(domain, port))
  }

  case class CustomDomain(uri: URI, port: Option[Int]) extends HttpDomain

}
