package blueeyes.core.http

import org.specs.Specification
import blueeyes.core.http.MimeTypes._

class IfRangeSpec extends Specification {

  "If-Range:  should return an HttpDateTime from an HttpDateTime input" in {
    HttpHeaders.`If-Range`(IfRanges.parseIfRanges("Tue, 29 Dec 2009 12:12:12 GMT").get).value mustEqual "Tue, 29 Dec 2009 12:12:12 GMT"
  }

  "If-Range:  should return an HttpDateTime from an HttpDateTime input" in {
    HttpHeaders.`If-Range`(IfRanges.parseIfRanges("\"e-tag content\"").get).value mustEqual "\"e-tag content\""
  }

}

