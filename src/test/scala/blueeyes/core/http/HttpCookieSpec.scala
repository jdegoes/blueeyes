package blueeyes.core.http

import org.specs.Specification
import blueeyes.core.http.MimeTypes._

class HttpCookieSpec extends Specification {

  "HttpCookie/Cookie:  Should create a new cookie valid data" in {
    HttpHeaders.Cookie(HttpCookies.parseHttpCookies("Cat=Mittens; expires=Mon, 01-Jan-2001 00:00:00 UTC; path=/; domain=.kittens.com").get).value mustEqual "Cat=Mittens; expires=Mon, 01 Jan 2001 00:00:00 GMT; path=/; domain=.kittens.com"
  }

  "HttpCookie/Set-Cookie:  Should create with as much valid data as given" in {
    HttpHeaders.`Set-Cookie`(HttpCookies.parseHttpCookies("Cat=Mittens; expires=feeefoo; path=/; doomblloo=kittens.com").get).value mustEqual "Cat=Mittens; path=/"
  }

}

