package blueeyes.core.data

import org.specs2.mutable.Specification
import blueeyes.json._

class BijectionsChunkJsonSpec extends Specification with BijectionsChunkJson{
  "BijectionsChunkJson" should{
    "parser valid JSON" in{
      JValueToChunk.unapply(Chunk("""{"foo": "bar"}""".getBytes())) mustEqual(JObject(List(JField("foo", JString("bar")))))
    }
    "throw error when JSON is incomplete" in{
      JValueToChunk.unapply(Chunk("""{"foo": "bar""".getBytes())) must throwA[Exception]
    }
  }
}
