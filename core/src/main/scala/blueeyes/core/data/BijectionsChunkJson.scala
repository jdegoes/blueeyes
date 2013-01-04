package blueeyes.core.data

import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.json.JsonParser
import akka.dispatch.Future
import akka.dispatch.Await
import akka.util.Timeout

trait BijectionsChunkJson {
  import java.io.{InputStreamReader, ByteArrayInputStream, OutputStreamWriter, ByteArrayOutputStream}

  val jsonTimeout = Timeout(200)

  implicit val JValueToChunk = new Bijection[JValue, ByteChunk] {

    def apply(t: JValue) = {
      val stream = new ByteArrayOutputStream()

      compact(render(t), new OutputStreamWriter(stream, "UTF-8"))

      Chunk(stream.toByteArray)
    }

    def unapply(s: ByteChunk) = {
      val futureJson = AggregatedByteChunk(s, None).map(
          chunk =>
            JsonParser.parse(
              new InputStreamReader(new ByteArrayInputStream(chunk.data), "UTF-8")
            )
        )

      Await.result(futureJson, jsonTimeout.duration)
    }
  }

  implicit val ChunkToJValue    = JValueToChunk.inverse
}

object BijectionsChunkJson extends BijectionsChunkJson


trait BijectionsChunkFutureJson {
  import BijectionsChunkJson._

  implicit def futureJValueToChunk(implicit timeout: Timeout = jsonTimeout) = new Bijection[Future[JValue], ByteChunk] {
    def apply(t: Future[JValue]) = Await.result(t.map(JValueToChunk(_)), timeout.duration)

    def unapply(b: ByteChunk) = AggregatedByteChunk(b, None).map(ChunkToJValue(_))
  }

  implicit def chunkToFutureJValue(implicit timeout: Timeout = jsonTimeout) = futureJValueToChunk(timeout).inverse
}

object BijectionsChunkFutureJson extends BijectionsChunkFutureJson
