package blueeyes.core.http

import org.specs2.mutable.Specification
import blueeyes.core.http.MimeTypes._
import org.specs2.matcher.MustThrownMatchers

class RangeUnitSpec extends Specification with MustThrownMatchers {

  "Range-Units:  Should parse \"bytes\" as Some(bytes) produce None for \"cats\"" in {
    RangeUnits.parseRangeUnits("bytes").map(_.toString).getOrElse("") mustEqual "bytes"
    RangeUnits.parseRangeUnits("cats").map(_.toString).getOrElse("") mustEqual ""
  }

  "Accept-Ranges:  Should create none from \"none\"" in {
    HttpHeaders.`Accept-Ranges`(RangeUnits.parseRangeUnits("none").get).value mustEqual "none"
  }
}
