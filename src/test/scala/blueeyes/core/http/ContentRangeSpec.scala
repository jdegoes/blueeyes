package blueeyes.core.http

import org.specs2.mutable.Specification
import org.specs2.matcher.MustThrownMatchers

class ContentRangeSpec extends Specification with MustThrownMatchers {

  "Content-Range: Should parse bytes=1234-5678/1212 correctly" in {
    HttpHeaders.`Content-Range`(ContentByteRanges.parseContentByteRanges("bytes=1234-5678/1212").get).value mustEqual "bytes=1234-5678/1212"
  }

  "Content-Range: Should parse bytes=1234-5678 to None"  in {
    ContentByteRanges.parseContentByteRanges("bleh=1234-5678") mustEqual None
  }

}

