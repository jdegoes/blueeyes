package blueeyes.core.data

import org.specs2.mutable.Specification
import Bijection._

class BijectionsChunkXMLSpec extends Specification with BijectionsChunkXML with BijectionsByteArray{
  "BijectionsChunkXML" should{
    "parser valid XML" in{
      XMLToChunk.unapply(new MemoryChunk(XMLToByteArray(<f></f>))) mustEqual(<f></f>)
    }
    "throw error when XML is incomplete" in{
      XMLToChunk.unapply(new MemoryChunk((XMLToByteArray(<f></f>).tail))) must throwA[RuntimeException]
    }
  }
}
