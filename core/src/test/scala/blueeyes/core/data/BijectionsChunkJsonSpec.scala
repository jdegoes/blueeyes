package blueeyes.core.data

import akka.dispatch.Future
import org.specs2.mutable.Specification
import blueeyes.bkka.AkkaDefaults
import blueeyes.json.JsonAST._
import blueeyes.json.JsonParser.ParseException

class BijectionsChunkJsonSpec extends Specification with BijectionsChunkJson with AkkaDefaults {
  "BijectionsChunkJson" should {

    "parser valid JSON" in {
      JValueToChunk.unapply(Chunk("""{"foo": "bar"}""".getBytes())) mustEqual(JObject(List(JField("foo", JString("bar")))))
    }

    "throw error when JSON is incomplete" in {
      JValueToChunk.unapply(Chunk("{\"foo\": \"bar\"".getBytes())) must throwA[ParseException]
    }

    "parse multi-chunk chunk" in {
      val str1 = "{\"foo\": "
      val str2 = "\"bar\"}"
      val chunky = Chunk(str1.getBytes(), Some(Future(Chunk(str2.getBytes(), None))))

      JValueToChunk.unapply(chunky) mustEqual JObject(List(JField("foo", JString("bar"))))
    }

    "encode non-ascii data correctly" in {
      val nonAscii = "♠éüλ"
      val json = JObject(List(JField("foo", JString(nonAscii))))

      JValueToChunk.unapply(JValueToChunk.apply(json)) mustEqual json
    }
  }
}
