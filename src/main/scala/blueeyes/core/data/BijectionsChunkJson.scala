package blueeyes.core.data

import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.json.JsonParser
import blueeyes.concurrent.Future

trait BijectionsChunkJson{
  import java.io.{InputStreamReader, ByteArrayInputStream, OutputStreamWriter, ByteArrayOutputStream}

  implicit val JValueToChunk      = new Bijection[JValue, ByteChunk]{
    def apply(t: JValue)         = {
      val stream = new ByteArrayOutputStream()

      compact(render(t), new OutputStreamWriter(stream))

      new MemoryChunk(stream.toByteArray)
    }

    def unapply(s: ByteChunk) = JsonParser.parse(new InputStreamReader(new ByteArrayInputStream(s.data), "UTF-8"))
  }

  implicit val ChunkToJValue    = JValueToChunk.inverse

}
object BijectionsChunkJson extends BijectionsChunkJson


trait BijectionsChunkFutureJson{
  import BijectionsChunkJson._
  implicit val FutureJValueToChunk = new Bijection[Future[JValue], ByteChunk]{
    def apply(t: Future[JValue]) = t.map(JValueToChunk(_)).value.getOrElse(new MemoryChunk(Array[Byte]()))

    def unapply(b: ByteChunk) = AggregatedByteChunk(b, None).map(ChunkToJValue(_))
  }

  implicit val ChunkToFutureJValue  = FutureJValueToChunk.inverse
}

object BijectionsChunkFutureJson extends BijectionsChunkFutureJson
