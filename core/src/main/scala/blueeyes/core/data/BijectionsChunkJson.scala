package blueeyes.core.data

import blueeyes.json._
import blueeyes.json.JParser
import akka.dispatch.Future
import akka.dispatch.Await
import akka.util.Timeout

trait BijectionsChunkJson{
  import java.io.{InputStreamReader, ByteArrayInputStream, OutputStreamWriter, ByteArrayOutputStream, PrintStream}
  import java.nio.ByteBuffer

  implicit val JValueToChunk = new Bijection[JValue, ByteChunk] {
    def unapply(s: ByteChunk) = {
      val bb = ByteBuffer.wrap(s.data)
      val r = JParser.parseFromByteBuffer(bb)
      r.valueOr(e => throw e)
    }

    def apply(t: JValue) = {
      val stream = new ByteArrayOutputStream()
      val printer = new PrintStream(stream, false, "UTF-8")
      printer.append(t.renderCompact)
      printer.close()
      Chunk(stream.toByteArray)
    }

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
