package blueeyes.core.data

import org.specs.Specification

class BijectionSpec extends Specification{
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
    val composed = BijectionsChunkReaderString.StringToChunkReader.andThen(BijectionsChunkReaderByteArray.ChunkReaderToArrayByte)
    composed("foo").toList mustEqual(List[Byte]('f', 'o', 'o'))
  }
}