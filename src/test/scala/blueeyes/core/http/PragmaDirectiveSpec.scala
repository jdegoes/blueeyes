package blueeyes.core.http

import org.specs.Specification
import blueeyes.core.http.MimeTypes._

class PragmaDirectiveSpec extends Specification {

  "Pragma: Parsing should return 'no-cache'"  in {
    HttpHeaders.Pragma(PragmaDirectives.parsePragmaDirectives(" No-Cache ").get).value mustEqual ("no-cache")
  }

  "Pragma: Parsing should return None on bad input" in {
    PragmaDirectives.parsePragmaDirectives(" zom ") mustEqual None
  }
}
