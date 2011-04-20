package blueeyes.core.http

import org.specs.Specification
import blueeyes.core.http.MimeTypes._
import URI._

class HttpUriSpec extends Specification {

  "From:  Should return the correct email name with a well-formed email and parse to None otherwise" in {
    HttpHeaders.From(URI.parseEmails("johnsmith@socialmedia.com ").get).value mustEqual "johnsmith@socialmedia.com"
  }

  "From: Should return the correct (although weird) email" in {
    HttpHeaders.From(URI.parseEmails(" j.o.n.Sm.ith@so.cia.lmedia.com ").get).value mustEqual "j.o.n.Sm.ith@so.cia.lmedia.com"
  }

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
