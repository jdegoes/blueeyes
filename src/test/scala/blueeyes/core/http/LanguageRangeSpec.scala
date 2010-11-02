package blueeyes.core.http

import org.specs.Specification
import blueeyes.core.http.MimeTypes._

class LanguageRangeSpec extends Specification {

  "Language-Range:  Should produce en-uk from \"en-uk\"" in {
    LanguageRanges.parseLanguageRanges("en-uk")(0).value mustEqual "en-uk"
  }

  "Accept-Language:  Should create languages (en-us-calif, is, cn) from \"en-us-calif, is=now!, cn; q=1\""  in {
    HttpHeaders.`Accept-Language`(LanguageRanges.parseLanguageRanges("en-us-calif, is=now!, cn; q=1"): _*)
      .value mustEqual "en-us-calif, is, cn"
  }

}
