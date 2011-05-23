package blueeyes.util

import org.specs.Specification
class SpecialCharToStringTranscoderSpec extends Specification {
  val t = SpecialCharToStringTranscoder({case c: Char if (c == '.' | c == '@') => new String(Array('%', c, c))},
    {case c :: Nil if (c == '%') => None
     case '%' :: List(c) => None
     case '%' :: y :: List(c) if (y == c) => Some(c)
    }
  )
  
  "SpecialCharToStringTranscoder.encode" should {
    "encode specified chars" in { 
      t.encode("@foo.baz") mustEqual ("%@@foo%..baz")
    }
  }
  
  "SpecialCharToStringTranscoder.decode" should {
    "decode specified chars" in { 
      t.decode("%@@foo%..baz") mustEqual ("@foo.baz")
    }
    "decode incomple chars" in {
      t.decode("%@foo%..baz%.") mustEqual ("%@foo.baz%.")
    }
  }
}