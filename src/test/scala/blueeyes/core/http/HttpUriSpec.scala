package blueeyes.core.http

import org.specs.Specification
import blueeyes.core.http.MimeTypes._

class HttpUriSpec extends Specification {

  "From:  Should return the correct email name with a well-formed email and parse to None otherwise" in {
    HttpHeaders.From(HttpUris.parseEmails("johnsmith@socialmedia.com").get).value mustEqual "johnsmith@socialmedia.com"
  }

  "From: Should return the correct (although weird) email" in {
    HttpHeaders.From(HttpUris.parseEmails(" j.o.n.Sm.ith@so.cia.lmedia.com ").get).value mustEqual "j.o.n.Sm.ith@so.cia.lmedia.com"
  }

  "HttpUris: Should parse non-email to None" in {
    HttpUris.parseEmails("209h3094)(it092jom") mustEqual None
  }

  "Host:  Should parse correct host uri" in {
    HttpHeaders.Host(HttpUris.parseHttpUris("http://www.socialmedia.com/coolServer/index.html").get).value mustEqual "www.socialmedia.com"
  }

  "Host:  Should parse correct host uri" in {
    HttpHeaders.Host(HttpUris.parseHttpUris("http://maps.google.com/coolmap.html").get).value mustEqual "maps.google.com"
  }

  "HttpUris: Should parse nonstandard characters to None" in {
    HttpUris.parseHttpUris("@^#&(!_") mustEqual None
  }

  "Location: Should return correct url on parsed input" in {
    HttpHeaders.`Location`(HttpUris.parseHttpUris("  http://www.socialmedia.com/index.html  ").get).value mustEqual "http://www.socialmedia.com/index.html"
  }

  "HttpUris: Parsing should return none on bad input" in {
    HttpUris.parseHttpUris("&%*#(!)Thttp://.socialmedia.com") mustEqual None
  }

}
