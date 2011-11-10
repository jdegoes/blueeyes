package blueeyes.core.http

import org.specs2.mutable.Specification
import org.specs2.matcher.MustThrownMatchers

class HttpUriSpec extends Specification with MustThrownMatchers {
  "Host:  Should parse correct host uri" in {
    HttpHeaders.Host(URI.opt("http://www.socialmedia.com/coolServer/index.html").get).value mustEqual "www.socialmedia.com"
  }

  "Host:  Should parse correct host uri" in {
    HttpHeaders.Host(URI.opt("http://maps.google.com/coolmap.html").get).value mustEqual "maps.google.com"
  }

  "Host:  Should parse correct host uri with port" in {
    URI.opt("http://maps.google.com:8080/coolmap.html").get mustEqual URI("http://maps.google.com:8080/coolmap.html")
  }

  "Location: Should return correct url on parsed input" in {
    HttpHeaders.`Location`(URI.opt("  http://www.socialmedia.com/index.html  ").get).value mustEqual "http://www.socialmedia.com/index.html"
  }
}
