package blueeyes.core.data

import org.specs.Specification

class BijectionSpec extends Specification{
  "Bijection.identity: creates Bijection which does not change data" in{
    Bijection.identity[String]("foo")         mustEqual ("foo")
    Bijection.identity[String].unapply("foo") mustEqual ("foo")
  }
  "Bijection.inverse: creates inverse Bijection" in{
    val inversed = BijectionsString.ByteArrayToString.inverse
    inversed.unapply(Array[Byte]('f', 'o', 'o'))  mustEqual("foo")
    inversed("foo").toList                        mustEqual(List[Byte]('f', 'o', 'o'))
  }
  "Bijection.compose: creates composed Bijection" in{
    val composed = BijectionsString.XMLToString.andThen(BijectionsString.StringToByteArray)
    composed(<f></f>).toList mustEqual(List[Byte]('<', 'f', '>', '<', '/', 'f', '>'))
  }
}