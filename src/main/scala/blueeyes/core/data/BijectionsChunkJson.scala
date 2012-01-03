package blueeyes.core.data

import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.json.JsonParser
import akka.dispatch.Future
import akka.dispatch.Await
import akka.util.Timeout

trait BijectionsChunkJson{
  import java.io.{InputStreamReader, ByteArrayInputStream, OutputStreamWriter, ByteArrayOutputStream}

  implicit val JValueToChunk = new Bijection[JValue, ByteChunk] {
    def apply(t: JValue) = {
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
  implicit def futureJValueToChunk(implicit timeout: Timeout = Timeout.zero) = new Bijection[Future[JValue], ByteChunk] {
    def apply(t: Future[JValue]) = Await.result(t.map(JValueToChunk(_)), timeout.duration)

    def unapply(b: ByteChunk) = AggregatedByteChunk(b, None).map(ChunkToJValue(_))
  }

  implicit def chunkToFutureJValue(implicit timeout: Timeout = Timeout.zero) = futureJValueToChunk(timeout).inverse
}

object BijectionsChunkFutureJson extends BijectionsChunkFutureJson
