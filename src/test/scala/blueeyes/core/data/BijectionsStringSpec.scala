package blueeyes.core.data

import org.specs.Specification

class BijectionsStringSpec extends Specification{

  "BijectionsString.ByteArrayToString: Should convert Bytes Array to String" in{
    BijectionsString.ByteArrayToString(Array[Byte]('f', 'o', 'o')) mustEqual("foo")
  }
  "BijectionsString.ByteArrayToString: Should convert String to Bytes Array " in{
    BijectionsString.ByteArrayToString.unapply("foo").toList mustEqual(List[Byte]('f', 'o', 'o'))
  }
  "BijectionsString.XMLToString: Should convert XML to String" in{
    BijectionsString.XMLToString(<foo></foo>) mustEqual("<foo></foo>")
  }
  "BijectionsString.XMLToString: Should convert String to Bytes Array " in{
    BijectionsString.XMLToString.unapply("<foo></foo>") mustEqual(<foo></foo>)
  }
}