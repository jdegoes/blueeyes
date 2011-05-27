package blueeyes.core.data

import org.specs.Specification
import blueeyes.json.JsonAST._
import blueeyes.json.JsonParser.ParseException

class BijectionsChunkJsonSpec extends Specification with BijectionsChunkJson{
  "BijectionsChunkJson" should{
    "parser valid JSON" in{
      JValueToChunk.unapply(new MemoryChunk("""{"foo": "bar"}""".getBytes())) mustEqual(JObject(List(JField("foo", JString("bar")))))
    }
    "throw error when JSON is incomplete" in{
      JValueToChunk.unapply(new MemoryChunk("""{"foo": "bar""".getBytes())) must throwA[ParseException]
    }
  }
}