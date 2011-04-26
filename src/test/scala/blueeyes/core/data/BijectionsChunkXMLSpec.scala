package blueeyes.core.data

import org.specs.Specification

class BijectionsChunkXMLSpec extends Specification with BijectionsChunkXML with BijectionsByteArray{
  "BijectionsChunkXML" should{
    "parser valid XML" in{
      XMLToChunk.unapply(new MemoryChunk(XMLToByteArray(<f></f>))) mustEqual(<f></f>)
    }
    "throw error when XML is incomplete" in{
      XMLToChunk.unapply(new MemoryChunk(XMLToByteArray(<f></f>).toList.tail.toArray)) must throwA[RuntimeException]
    }
  }
}