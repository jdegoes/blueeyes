package blueeyes.core.data

import org.specs2.mutable.Specification
import org.specs2.matcher.MustThrownMatchers

class BijectionSpec extends Specification with MustThrownMatchers{
  "Bijection.identity: creates Bijection which does not change data" in{
    Bijection.identity[String]("foo")         mustEqual ("foo")
    Bijection.identity[String].unapply("foo") mustEqual ("foo")
  }
  "Bijection.inverse: creates inverse Bijection" in{
    val inversed = BijectionsByteArray.ByteArrayToString.inverse
    inversed.unapply(Array[Byte]('f', 'o', 'o'))  mustEqual("foo")
    inversed("foo").toList                        mustEqual(List[Byte]('f', 'o', 'o'))
  }
  "Bijection.compose: creates composed Bijection" in{
    val composed = BijectionsChunkString.StringToChunk.andThen(BijectionsChunkByteArray.ChunkToArrayByte)
    composed("foo").toList mustEqual(List[Byte]('f', 'o', 'o'))
  }
}