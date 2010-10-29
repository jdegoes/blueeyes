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

object HttpCookies {

  def parseHttpCookies(inString: String): Option[HttpCookie] = {
    def NameRegex = """([a-zA-Z\d])+=[^;]*""".r

    /* Ex of date: expires=Mon, 01-Jan-2001 00:00:00 GMT */
    def DateRegex = """expires=[^;]+""".r

    /* Example of Path: path=/ */
    def PathRegex = """path=[^;]+""".r

    /* Example of domain: domain=.example.nets/kittens */
    def DomainRegex = """domain=[^;]+""".r

    def nameValuePair: List[String] = NameRegex.findFirstIn(inString.trim).getOrElse("").split("=").toList
    if (nameValuePair.length != 2)
      return None
    
    def date: Option[HttpDateTime] = HttpDateTimes.parseHttpDateTimes(DateRegex.findFirstIn(inString).getOrElse("").split("=")(1))

    def path: Option[String] = PathRegex.findFirstIn(inString.trim).map(_.replaceFirst("path=",""))

    def domain: Option[String] = DomainRegex.findFirstIn(inString.trim).map(_.replaceFirst("domain=",""))

    return Some(CookieData(nameValuePair(0), nameValuePair(1), date, path, domain))
  }


  case class CookieData (name: String, cookieValue: String, expires: Option[HttpDateTime], path: Option[String], domain: Option[String]) extends HttpCookie

  object CookieData {
    def apply(name: String, cookieValue: String): HttpCookie = 
      new CookieData (name, cookieValue, None, None, None)

    def apply(name: String, cookievalue: String, path: String): HttpCookie = 
      new CookieData (name, cookievalue, None, Some(path), None)

    def apply(name: String, cookieValue: String, expires: HttpDateTime): HttpCookie = 
      new CookieData (name, cookieValue, Some(expires), None, None)

    def apply(name: String, cookieValue: String, expires: HttpDateTime, path: String): HttpCookie = 
      new CookieData (name, cookieValue, Some(expires), Some(path), None)

    def apply(name: String, cookieValue: String, expires: HttpDateTime, path: String, domain: String): HttpCookie = 
      new CookieData (name, cookieValue, Some(expires), Some(path), Some(domain))
  }

}
