package blueeyes.core.http

import org.specs2.mutable.Specification
import org.specs2.matcher.MustThrownMatchers

class HttpNumberSpec extends Specification with MustThrownMatchers {

  "HttpNumbers:  Should return ContentLength or parse to None on bad input" in {
    HttpNumbers.parseHttpNumbers("bees") mustEqual None
  }
}
