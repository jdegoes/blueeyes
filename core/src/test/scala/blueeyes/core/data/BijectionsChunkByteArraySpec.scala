package blueeyes.core.data

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import blueeyes.concurrent.test.FutureMatchers
import akka.dispatch.Future

class BijectionsChunkByteArraySpec extends Specification with BijectionsByteArray with BijectionsChunkByteArray with blueeyes.bkka.AkkaDefaults with FutureMatchers {
  private val jObject1 = JObject(List(JField("foo", JString("bar"))))
  private val jObject2 = JObject(List(JField("bar", JString("foo"))))
  private val bijection = chunksToChunksArrayByte[JValue]

  "BijectionsChunkByteArray" should{
    "convert chunk to bytes chunks" in{
      val chunks     = Chunk(jObject1, Some(Future[Chunk[JValue]](Chunk(jObject2))))
      val bytesChunk = bijection(chunks)

      ByteArrayToJValue(bytesChunk.data) mustEqual(jObject1)

      bytesChunk.next.get must whenDelivered {
        (chunk: ByteChunk) => ByteArrayToJValue(chunk.data) mustEqual(jObject2)
      }
    }

    "convert bytes chunk to chunk" in{
      val chunks     = Chunk(JValueToByteArray(jObject1), Some(Future[ByteChunk](Chunk(JValueToByteArray(jObject2)))))
      val bytesChunk = bijection.unapply(chunks)

      bytesChunk.data mustEqual(jObject1)
      bytesChunk.next.get.map(_.data) must whenDelivered (be_==(jObject2))
    }
  }
}
