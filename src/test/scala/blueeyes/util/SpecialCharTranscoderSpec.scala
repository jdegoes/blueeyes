package blueeyes.util

import org.specs2.mutable.Specification

class SpecialCharTranscoderSpec extends Specification{
  val transcoder = SpecialCharTranscoder.fromMap('_', Map('.' -> 'd', '@' -> 'a'))
  
  "SpecialCharTranscoder.encode" should {
    "encode specified chars" in { 
      transcoder.encode("@foo.baz") mustEqual ("_afoo_dbaz")
    }
    
    "encode escape char" in {
      transcoder.encode("_@_") mustEqual ("___a__")
    }
  }
  
  "SpecialCharTranscoder.decode" should {
    "decode specified chars" in { 
      transcoder.decode("_afoo_dbaz") mustEqual ("@foo.baz")
    }
    
    "decode escape char" in {
      transcoder.decode("___a__") mustEqual ("_@_")
    }
  }
}