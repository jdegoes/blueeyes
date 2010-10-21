package blueeyes.core.service

import org.joda.time.DateTime

/* For use in the Cookie HTTP Header */

sealed trait HttpCookie {
  def name: String
  def cookieValue: String
  def expire: Option[String] //... Probably should be an Option[Date] object 
  def domain: Option[String]
  def path: Option[String]
  def value: String = List(List(name, cookieValue).mkString("=") ::
                           expire.map(List("expire", _)).mkString("=") ::
                           path.map(List("path", _)).mkString("=") ::
                           domain.map(List("domain",_)).mkString("=") :: Nil).mkString("; ")
  override def toString = value

}

object HttpCookies {

  case class CookieData (name: String, cookieValue: String, expire: Option[String], path: Option[String], domain: Option[String]) extends HttpCookie

  object CookieData {
    def apply(name: String, cookieValue: String): CookieData = new CookieData (name, cookieValue, None, None, None)

    def apply(name: String, cookieValue: String, expire: HttpDateTime): CookieData = 
      new CookieData (name, cookieValue, Some(expire.toString), None, None)

    def apply(name: String, cookieValue: String, expire: HttpDateTime, path: String): CookieData = 
      new CookieData (name, cookieValue, Some(expire.toString), Some(path), None)

    def apply(name: String, cookieValue: String, expire: HttpDateTime, path: String, domain: String): CookieData = 
      new CookieData (name, cookieValue, Some(expire.toString), Some(path), Some(domain))
  }

}
