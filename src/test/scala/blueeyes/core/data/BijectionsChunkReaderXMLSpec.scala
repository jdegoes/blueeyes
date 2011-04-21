package blueeyes.core.data

import org.specs.Specification

class BijectionsChunkReaderXMLSpec extends Specification with BijectionsChunkReaderXML with BijectionsByteArray{
  "BijectionsChunkReaderXML" should{
    "parser valid XML" in{
      XMLToChunkReader.unapply(new MemoryChunk(XMLToByteArray(<f></f>))) mustEqual(<f></f>)
    }
    "throw error when XML is incomplete" in{
      XMLToChunkReader.unapply(new MemoryChunk(XMLToByteArray(<f></f>).toList.tail.toArray)) must throwA[RuntimeException]
    }
  }
}