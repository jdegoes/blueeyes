package blueeyes.core.http

import org.specs.Specification

class ByteRangeSpec extends Specification {

  "Range: Should parse correctly on good input" in {
    HttpHeaders.Range(ByteRanges.parseByteRanges("bytes=0-500, 699-2000, -4").get).value mustEqual "bytes=0-500, 699-2000, -4"
  }

  "Range: Should produce none on bad input" in {
    ByteRanges.parseByteRanges("bytes=cats") mustEqual None
    ByteRanges.parseByteRanges("bytes=1-29, cats").get.toString mustEqual "bytes=1-29"
  }
}
