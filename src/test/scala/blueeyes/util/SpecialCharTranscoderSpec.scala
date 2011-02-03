package blueeyes.util

import org.specs.Specification
import org.specs.util._

class SpecialCharTranscoderSpec extends Specification {
  val t = SpecialCharTranscoder.fromMap('_', Map('.' -> 'd', '@' -> 'a'))
  
  "SpecialCharTranscoder.encode" should {
    "encode specified chars" in { 
      t.encode("@foo.baz") mustEqual ("_afoo_dbaz")
    }
    
    "encode escape char" in {
      t.encode("_@_") mustEqual ("___a__")
    }
  }
  
  "SpecialCharTranscoder.decode" should {
    "decode specified chars" in { 
      t.decode("_afoo_dbaz") mustEqual ("@foo.baz")
    }
    
    "decode escape char" in {
      t.decode("___a__") mustEqual ("_@_")
    }
  }
}