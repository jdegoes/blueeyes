package blueeyes.core.http

import org.specs.Specification

class ContentRangeSpec extends Specification {

  "Content-Range: Should parse bytes=1234-5678/1212 correctly" in {
    HttpHeaders.`Content-Range`(ContentByteRanges.parseContentByteRanges("bytes=1234-5678/1212").get).value mustEqual "bytes=1234-5678/1212"
  }

  "Content-Range: Should parse bytes=1234-5678 to None"  in {
    ContentByteRanges.parseContentByteRanges("bleh=1234-5678") mustEqual None
  }

}

