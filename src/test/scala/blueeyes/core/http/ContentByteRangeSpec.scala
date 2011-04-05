package blueeyes.core.http

import org.specs.Specification
import ContentByteRanges._

class ContentByteRangeSpec extends Specification with HttpNumberImplicits{

  "ContentByteRange: Should parse correctly on good input" in {
    ContentByteRanges.parseContentByteRanges("bytes 0-499/1234") mustEqual Some(ByteInstance("bytes", ByteRanges.BytePair(0, 499), "1234"))
    ContentByteRanges.parseContentByteRanges("bytes 0-499/*") mustEqual Some(ByteInstance("bytes", ByteRanges.BytePair(0, 499), "*"))
  }

  "ContentByteRange: Should produce none on bad input" in {
    ByteRanges.parseByteRanges("bytes cats/foo") mustEqual None
  }
}
