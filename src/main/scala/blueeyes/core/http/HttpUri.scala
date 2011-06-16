package blueeyes.core.http

import scala.util.parsing.combinator._
import scala.util.parsing.input._

//
//sealed trait HttpUri {
//
//  def uri: URI
//  override def toString = uri.toString
//}
//
//object HttpUris extends RegexParsers{
//
//  def parseHttpUris(inString: String): Option[HttpUri] = {
//    return Some(CustomUri(URI(inString.trim)))
//  }
//
//  def parseEmails(inString: String): Option[HttpUri] = {
//    def parser = (regex("""([a-zA-Z\d_-]|\.)+@([a-zA-Z\d_-]|\.)+""".r)?) ^^ {case email => email.map(v => CustomUri(URI(v)))}
//
//    parser(new CharSequenceReader(inString)) match {
//      case Success(result, _) => result
//
//      case Failure(msg, _) => sys.error("The charSets " + inString + " has a syntax error: " + msg)
//
//      case Error(msg, _) => sys.error("There was an error parsing \"" + inString + "\": " + msg)
//    }
//  }
//
//  case class CustomUri(uri: URI) extends HttpUri
//
//}
