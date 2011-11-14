package blueeyes.core.http

import org.specs2.mutable.Specification
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
  "HttpCookies: Should parse cookies with '_' character" in{
    check("_gads=39d84b1ece008558", CookieData("_gads", "39d84b1ece008558"))
    check("_livingsocial_sessionid=a8f08366-42d7-4e19-bc65-d9f08f536ff5", CookieData("_livingsocial_sessionid", "a8f08366-42d7-4e19-bc65-d9f08f536ff5"))
  }

  private def check(cookieStr: String, cookie: HttpCookie*) = {
    CookiesPattern.isDefinedAt(cookieStr)  must be_==(true)
    CookiesPattern.apply(cookieStr)        mustEqual(List(cookie: _*))
  }
}

