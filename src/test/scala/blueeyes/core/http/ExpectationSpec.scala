package blueeyes.core.http

import org.specs.Specification
import blueeyes.core.http.MimeTypes._

class ExpectationSpec extends Specification {

  "Expectation:  Should return continue or failure" in {
    HttpHeaders.Expect(Expectations.parseExpectations("100").get).value mustEqual "100-continue"
  }

  "Expectation: Should return failure" in {
    HttpHeaders.Expect(Expectations.parseExpectations("417").get).value mustEqual "417-expectationfailed"
  }

  "Expectation: Should parse to none on bad input" in { 
    Expectations.parseExpectations("asdf4s17") mustEqual None
  }
}

