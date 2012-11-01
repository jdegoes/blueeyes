package blueeyes.util

import org.specs2.mutable.Specification

class SpecialCharToStringTranscoderSpec extends Specification{
  val transcoder = SpecialCharToStringTranscoder({case c: Char if (c == '.' | c == '@') => new String(Array('%', c, c))},
    {case c :: Nil if (c == '%') => None
     case '%' :: List(c) => None
     case '%' :: y :: List(c) if (y == c) => Some(c)
    }
  )
  
  "SpecialCharToStringTranscoder.encode" should {
    "encode specified chars" in { 
      transcoder.encode("@foo.baz") mustEqual ("%@@foo%..baz")
    }
  }
  
  "SpecialCharToStringTranscoder.decode" should {
    "decode specified chars" in { 
      transcoder.decode("%@@foo%..baz") mustEqual ("@foo.baz")
    }
    "decode incomple chars" in {
      transcoder.decode("%@foo%..baz%.") mustEqual ("%@foo.baz%.")
    }
  }
}