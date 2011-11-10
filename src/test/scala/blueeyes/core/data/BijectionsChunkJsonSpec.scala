package blueeyes.core.data

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import blueeyes.json.JsonParser.ParseException
import org.specs2.matcher.MustThrownMatchers

class BijectionsChunkJsonSpec extends Specification with BijectionsChunkJson with MustThrownMatchers{
  "BijectionsChunkJson" should{
    "parser valid JSON" in{
      JValueToChunk.unapply(new MemoryChunk("""{"foo": "bar"}""".getBytes())) mustEqual(JObject(List(JField("foo", JString("bar")))))
    }
    "throw error when JSON is incomplete" in{
      JValueToChunk.unapply(new MemoryChunk("""{"foo": "bar""".getBytes())) must throwA[ParseException]
    }
  }
}