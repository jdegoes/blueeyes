package blueeyes.core.http

import org.specs.Specification
import blueeyes.core.http.MimeTypes._

class RangeUnitSpec extends Specification {

  "Range-Units:  Should parse \"bytes\" as Some(bytes) produce None for \"cats\"" in {
    RangeUnits.parseRangeUnits("bytes").map(_.toString).getOrElse("") mustEqual "bytes"
    RangeUnits.parseRangeUnits("cats").map(_.toString).getOrElse("") mustEqual ""
  }

  "Accept-Ranges:  Should create none from \"none\"" in {
    HttpHeaders.`Accept-Ranges`(RangeUnits.parseRangeUnits("none").get).value mustEqual "none"
  }
}
