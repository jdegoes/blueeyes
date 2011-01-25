package blueeyes.core.data

import org.specs.Specification
import blueeyes.json.JsonAST._

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
  "Bijection.BijectionsByteArray: convert JValue to Bytes Array" in{
    val value     = JObject(JField("Foo", JString("bar")) :: Nil)
    val bijection = BijectionsByteArray.JValueToByteArray

    bijection.unapply(bijection(value)) mustEqual(value)
  }
  "Bijection.XMLToByteArray: convert XML to Bytes Array" in{
    val value     = <f></f>
    val bijection = BijectionsByteArray.XMLToByteArray

    bijection.unapply(bijection(value)) mustEqual(value)
  }
}