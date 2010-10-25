package blueeyes.core.http

import org.joda.time.DateTime

/* For use in the Cookie HTTP Header */

sealed trait HttpCookie {
  def name: String
  def cookieValue: String
  def expires: Option[HttpDateTime] //... Probably should be an Option[Date] object 
  def stringExpires = expires.toString
  def domain: Option[String]
  def path: Option[String]
  def value: String = List(List(name, cookieValue).mkString("=") ::
    stringExpires.map(List("expires", _)).mkString("=") ::
    path.map(List("path", _)).mkString("=") ::
    domain.map(List("domain",_)).mkString("=") :: Nil).mkString("; ")
  override def toString = value

}

object HttpCookies {

  def parseHttpCookies(inString: String): HttpCookie = {
    def cb = new CookieBuilder() 

    def outCookie = inString.trim.split(";").map(_.trim).map(_.split("=")).map(x => x match {
      case Array("path", any) => cb.path = any
      case Array("expires", any) => cb.expires = HttpDateTimes.parseHttpDateTimes(any)
      case Array("domain", any) => cb.domain = any
      case Array(name, value) => cb.name = name; cb.cookieValue = value
    })
    return cb.buildCookie;
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

  private class CookieBuilder() {
 
    private[this] var n: String = "" 
    private[this] var cv: String = ""
    private[this] var exp: Option[HttpDateTime] = None
    private[this] var dom: Option[String] = None
    private[this] var pat: Option[String] = None

    def name: String = n
    def name_=(name: String) { n = name }

    def cookieValue: String = cv
    def cookieValue_=(inCookieValue: String) { cv = inCookieValue }

    def expires: Option[HttpDateTime] = exp
    def expires_=(date: HttpDateTime) { exp = Some(date) }

    def domain: Option[String] = dom
    def domain_=(inDomain: String) { dom = Some(inDomain) }

    def path: Option[String] = pat 
    def path_=(inPath: String) { pat = Some(inPath) }

    def buildCookie: HttpCookie = new CookieData(name, cookieValue, expires, path, domain)
  }

}
