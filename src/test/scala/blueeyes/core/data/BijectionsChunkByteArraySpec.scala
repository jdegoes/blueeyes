package blueeyes.core.data

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import blueeyes.concurrent.Future

class BijectionsChunkByteArraySpec extends Specification with BijectionsByteArray with BijectionsChunkByteArray{
  private val jObject1 = JObject(List(JField("foo", JString("bar"))))
  private val jObject2 = JObject(List(JField("bar", JString("foo"))))
  private val bijection = chunksToChunksArrayByte[JValue]

  "BijectionsChunkByteArray" should{
    "convert chunk to bytes chunks" in{
      val chunks     = new MemoryChunk[JValue](jObject1, () => Some(Future.sync[Chunk[JValue]](new MemoryChunk[JValue](jObject2))))
      val bytesChunk = bijection(chunks)

      ByteArrayToJValue(bytesChunk.data) mustEqual(jObject1)
      ByteArrayToJValue(bytesChunk.next.get.value.get.data) mustEqual(jObject2)
    }
    "convert bytes chunk to chunk" in{
      val chunks     = new ByteMemoryChunk(JValueToByteArray(jObject1), () => Some(Future.sync[ByteChunk](new ByteMemoryChunk(JValueToByteArray(jObject2)))))
      val bytesChunk = bijection.unapply(chunks)

      bytesChunk.data mustEqual(jObject1)
      bytesChunk.next.get.value.get.data mustEqual(jObject2)
    }
  }
}
