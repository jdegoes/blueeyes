package blueeyes.core.data

import org.specs.Specification
import blueeyes.json.JsonAST._

class BijectionsChunkReaderJsonSpec extends Specification with BijectionsChunkReaderJson{
  "BijectionsChunkReaderJson" should{
    "parser valid JSON" in{
      JValueToChunkReader.unapply(new MemoryChunk("""{"foo": "bar"}""".getBytes())) mustEqual(JObject(List(JField("foo", JString("bar")))))
    }
    "throw error when JSON is incomplete" in{
      JValueToChunkReader.unapply(new MemoryChunk("""{"foo": "bar""".getBytes())) must throwA[RuntimeException]
    }
  }
}