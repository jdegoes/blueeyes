package blueeyes.core.http

import org.specs.Specification
import HttpCookies._

class HttpCookieSpec extends Specification{

  "HttpCookies: Should parse simple cookies" in{
    check("Cat=Mittens", CookieData("Cat", "Mittens"))
  }
  "HttpCookies: Should parse cookies with expires date" in{
    check("Cat=Mittens; expires=Mon, 01-Jan-2001 00:00:00 UTC", CookieData("Cat", "Mittens", HttpDateTimes.parseHttpDateTimes("Mon, 01-Jan-2001 00:00:00 UTC")))
  }
  "HttpCookies: Should parse cookies with path" in{
    check("Cat=Mittens; path=/foo", CookieData("Cat", "Mittens", None, Some("/foo"), None))
  }
  "HttpCookies: Should parse cookies with domain" in{
    check("Cat=Mittens; domain=google.com", CookieData("Cat", "Mittens", None, None, Some("google.com")))
  }
  "HttpCookies: Should parse cookies with  all information" in{
    check("Cat=Mittens; expires=Mon, 01-Jan-2001 00:00:00 UTC; path=/foo; domain=google.com", CookieData("Cat", "Mittens", HttpDateTimes.parseHttpDateTimes("Mon, 01-Jan-2001 00:00:00 UTC"), Some("/foo"), Some("google.com")))
  }
  "HttpCookies: Should parse cookies with unnamed value" in{
    check("value;Cat=Mittens", CookieData("Cat", "Mittens"))
  }
  "HttpCookies: Should parse simple cookies with semicolon" in{
    check("Cat=Mittens;", CookieData("Cat", "Mittens"))
  }
  "HttpCookies: Should parse several cookies" in{
    check("version;Cat=Mittens; Dog=Noel", CookieData("Cat", "Mittens"), CookieData("Dog", "Noel"))
  }

  private def check(cookieStr: String, cookie: HttpCookie*){
    CookiesPattern.isDefinedAt(cookieStr)  must be (true)
    CookiesPattern.apply(cookieStr)        mustEqual(List(cookie: _*))

  }
}

