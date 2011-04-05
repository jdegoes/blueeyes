package blueeyes.core.http

import java.net.URI
import scala.util.parsing.combinator._
import scala.util.parsing.input._


sealed trait HttpUri {

  def uri: URI
  def port: Option[Int] 
  def host: String = ((uri.getHost :: Nil) ::: port.toList.map(_.toString)).mkString(":")
  def path: String = uri.getPath
  def absoluteUri = uri.toString 
  override def toString = absoluteUri

}

object HttpUris extends RegexParsers{

  def parseHttpUris(inString: String): Option[HttpUri] = {
    var uri: URI = null

    try {
      uri = new URI(inString.trim) 
    } catch {
      case _ => return None
    }
    if (uri == null)
      return None

    return Some(CustomUri(uri, if (uri.getPort != -1) Some(uri.getPort) else None))
  }

  def parseEmails(inString: String): Option[HttpUri] = {
    def parser = (regex("""([a-zA-Z\d_-]|\.)+@([a-zA-Z\d_-]|\.)+""".r)?) ^^ {case email => email.map(v => CustomUri(new URI(v), None))}

    parser(new CharSequenceReader(inString)) match {
      case Success(result, _) => result

      case Failure(msg, _) => error("The charSets " + inString + " has a syntax error: " + msg)

      case Error(msg, _) => error("There was an error parsing \"" + inString + "\": " + msg)
    }
  }

  case class CustomUri(uri: URI, port: Option[Int]) extends HttpUri

}
