package blueeyes.core.http

import org.joda.time.DateTime
import scala.util.matching.Regex

/* For use in the Cookie HTTP Header */

/* Example of a cookie (src Wikipedia):

RMID=732423sdfs73242; expires=Fri, 31-Dec-2010 23:59:59 GMT; path=/;
domain=.example.net

Note that a cookie can have multiple name-value pairs

*/


sealed trait HttpCookie {
  def name: String
  def cookieValue: String
  def expires: Option[HttpDateTime] 
  def stringExpires = expires.toString
  def domain: Option[String]
  def path: Option[String]
  def value: String = name + "=" + cookieValue +
                      expires.map(x => "; expires="+x).getOrElse("") +
                      path.map(x => "; path="+x).getOrElse("") +
                      domain.map(x => "; domain="+x).getOrElse("")
    
  override def toString = value

}

import scala.util.parsing.combinator._
import scala.util.parsing.input._
object CookiesPattern extends PartialFunction[String, List[HttpCookie]]{
  import CookiesPatternParsers._

  private def applyParser = cookies

  def isDefinedAt(s: String): Boolean = applyParser.apply(new CharSequenceReader(s)) match {
    case Success(result, _) => true

    case _ => false
  }

  def apply(s: String): List[HttpCookie] = applyParser.apply(new CharSequenceReader(s)) match {
    case Success(result, _) => result.filter(_ != None).map(_.get)

    case Failure(msg, _)    => parseFailure(msg, s)

    case Error(msg, _)      => parseError(msg, s)
  }

  private def parseFailure(msg: String, s: String) = {
    error("The pattern " + this.toString + " does not match " + s + ": " + msg)
  }
  private def parseError(msg: String, s: String) = {
    error("There was an error parsing \"" + s + "\" with pattern \"" + this.toString + "\": " + msg)
  }  
}

object CookiesPatternParsers extends RegexParsers {
  import HttpCookies._

  def keysParser :       Parser[String]                 = ("expires" | "path" | "domain")
  def secureParser :     Parser[Any]                    = ";" ~ "secure"
  def nameParser :       Parser[String]                 = """[a-zA-Z0-9]+""".r
  def valueParser:       Parser[String]                 = """[^;]+""".r
  def cookieValueParser: Parser[Option[HttpCookie]]     =  nameParser ~ "=" ~ valueParser ^^ {case n~equal~v => Some(CookieData(n, v))} | (valueParser ^^ (v => None))
  def parameterParser:   Parser[Tuple2[String, String]] = ";" ~ keysParser  ~ "=" ~ valueParser   ^^ {case sep~k~equal~v => Tuple2[String, String](k, v)}
  def cookieParser:      Parser[Option[HttpCookie]]     = cookieValueParser ~ ((parameterParser | secureParser)*) ^^ {case cookie~optional => {
    cookie match{
      case Some(e : CookieData) => {
        var result = e
        optional.foreach(v => result = v match {
          case ("expires", x: String) => result.copy(expires = HttpDateTimes.parseHttpDateTimes(x))
          case ("path",    x: String) => result.copy(path = Some(x))
          case ("domain",  x: String) => result.copy(domain = Some(x))
          case _ => result
        })
        Some(result)
      }
      case _ => None
    }
  }}
  def cookies:     Parser[List[Option[HttpCookie]]]  = repsep(cookieParser, ";")
}

object HttpCookies {
  case class CookieData (name: String, cookieValue: String, expires: Option[HttpDateTime] = None, path: Option[String] = None, domain: Option[String] = None) extends HttpCookie
}
