package blueeyes.core.http

import org.specs.Specification

class HttpNumberSpec extends Specification {

  "HttpNumbers/Content-Length:  Should return ContentLength or parse to None on bad input" in {
    HttpHeaders.`Content-Length`(HttpNumbers.parseHttpNumbers("5").get).value mustEqual "5"
  }

  "HttpNumbers:  Should return ContentLength or parse to None on bad input" in {
    HttpNumbers.parseHttpNumbers("bees") mustEqual None
  }
}
