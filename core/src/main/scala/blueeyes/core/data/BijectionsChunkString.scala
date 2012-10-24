package blueeyes.core.data

import akka.dispatch.Future
import akka.dispatch.Await
import akka.util.Timeout

trait BijectionsChunkString {
  implicit val StringToChunk = new Bijection[String, Chunk[Array[Byte]]] {
    def apply(s: String): Chunk[Array[Byte]]   = Chunk(s.getBytes("UTF-8"))
    def unapply(t: Chunk[Array[Byte]]): String = new String(t.data, "UTF-8")
  }

  implicit val ChunkToString    = StringToChunk.inverse
}
object BijectionsChunkString extends BijectionsChunkString

trait BijectionsChunkFutureString{
  import BijectionsChunkString._
  implicit def futureStringToChunk(implicit timeout: Timeout = Timeout.zero) = new Bijection[Future[String], Chunk[Array[Byte]]]{
    def apply(t: Future[String]) = Await.result(t.map(StringToChunk(_)), timeout.duration)

    def unapply(b: Chunk[Array[Byte]]) = AggregatedByteChunk(b, None).map(ChunkToString(_))
  }

  implicit def chunkToFutureString(implicit timeout: Timeout = Timeout.zero) = futureStringToChunk(timeout).inverse
}

object BijectionsChunkFutureString extends BijectionsChunkFutureString
