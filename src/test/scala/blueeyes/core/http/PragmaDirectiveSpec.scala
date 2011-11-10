package blueeyes.core.http

import org.specs2.mutable.Specification
import blueeyes.core.http.MimeTypes._
import org.specs2.matcher.MustThrownMatchers

class PragmaDirectiveSpec extends Specification with MustThrownMatchers {

  "Pragma: Parsing should return 'no-cache'"  in {
    HttpHeaders.Pragma(PragmaDirectives.parsePragmaDirectives(" No-Cache ").get).value mustEqual ("no-cache")
  }

  "Pragma: Parsing should return None on bad input" in {
    PragmaDirectives.parsePragmaDirectives(" zom ") mustEqual None
  }
}
