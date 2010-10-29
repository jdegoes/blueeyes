package blueeyes.core.http

import java.net.URI
import scala.util.matching.Regex


sealed trait HttpUri {

  def uri: URI
  def port: Option[Int] 
  def host: String = ((uri.getHost :: Nil) ::: port.toList.map(_.toString)).mkString(":")
  def path: String = uri.getPath
  def absoluteUri = uri.toString 
  override def toString = absoluteUri

}

object HttpUris {

  def parseHttpUris(inString: String): Option[HttpUri] = {
    var uri: URI = null

    try {
      uri = new URI(inString.trim) 
    } catch {
      case _ => return None
    }

    if (uri == null)
      return None

    def port: Option[Int] = """(?<=[a-z]):\d+""".r.findFirstIn(inString).map(_.toInt)
    return Some(CustomUri(uri, port))
  }

  def parseEmails(inString: String): Option[HttpUri] = {
    def EmailRegex = """([a-zA-Z\d_-]|\.)+@([a-zA-Z\d_-]|\.)+""".r
    def email: Option[String] = EmailRegex.findFirstIn(inString.trim)
    def emailUri: Option[URI] = email.getOrElse("").split("@").toList.length match {
        case 2 => try { Some(new URI(email.get)) } catch { case _ => return None }
        case _ => return None
      }
    def port: Option[Int] = """:\d+""".r.findFirstIn(inString).map(_.toInt)
    if (emailUri == None) {
      return None
    }
    return Some(CustomUri(emailUri.get, port))
  }

  case class CustomUri(uri: URI, port: Option[Int]) extends HttpUri

}
