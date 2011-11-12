package blueeyes.core.http

import org.specs2.mutable.Specification
import Encodings._

class EncodingSpec extends Specification{

  "Encodings:  Should produce a encoding" in {
    Encodings.parseEncodings("compress") mustEqual List(compress)
  }

  "Encodings:  Should produce list of encodings" in {
    Encodings.parseEncodings("x-compress, *") mustEqual List(`x-compress`, `*`)
  }

  "Encodings:  Should produce custom encodings" in {
    Encodings.parseEncodings("customa, customb") mustEqual List(CustomEncoding("customa"), CustomEncoding("customb"))
  }
}

