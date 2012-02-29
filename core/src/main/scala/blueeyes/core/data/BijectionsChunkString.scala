package blueeyes.core.data

import akka.dispatch.Future
import akka.dispatch.Await
import akka.util.Timeout

trait BijectionsChunkString {
  implicit val StringToChunk = new Bijection[String, ByteChunk] {
    def apply(s: String): ByteChunk   = Chunk(s.getBytes("UTF-8"))
    def unapply(t: ByteChunk): String = new String(t.data, "UTF-8")
  }

  implicit val ChunkToString    = StringToChunk.inverse
}
object BijectionsChunkString extends BijectionsChunkString

trait BijectionsChunkFutureString{
  import BijectionsChunkString._
  implicit def futureStringToChunk(implicit timeout: Timeout = Timeout.zero) = new Bijection[Future[String], ByteChunk]{
    def apply(t: Future[String]) = Await.result(t.map(StringToChunk(_)), timeout.duration)

    def unapply(b: ByteChunk) = AggregatedByteChunk(b, None).map(ChunkToString(_))
  }

  implicit def chunkToFutureString(implicit timeout: Timeout = Timeout.zero) = futureStringToChunk(timeout).inverse
}

object BijectionsChunkFutureString extends BijectionsChunkFutureString
