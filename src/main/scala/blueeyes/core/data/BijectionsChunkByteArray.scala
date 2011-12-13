package blueeyes.core.data

import akka.dispatch.Future
import akka.dispatch.Await
import akka.util.Timeout

trait BijectionsChunkByteArray {
  implicit val ArrayByteToChunk = new Bijection[Array[Byte], ByteChunk] {
    def apply(t: Array[Byte]): ByteChunk    = new MemoryChunk(t)
    def unapply(s: ByteChunk): Array[Byte]  = s.data
  }

  implicit val ChunkToArrayByte = ArrayByteToChunk.inverse

  def chunksToChunksArrayByte[T](implicit bijection: Bijection[T, Array[Byte]]) = new Bijection[Chunk[T], ByteChunk]{
    def apply(t: Chunk[T])    = new ByteMemoryChunk(bijection(t.data), () => t.next.map(_.map(apply(_))))
    def unapply(s: ByteChunk) = new MemoryChunk[T](bijection.unapply(s.data), () => s.next.map(_.map(unapply(_))))
  }
}
object BijectionsChunkByteArray extends BijectionsChunkByteArray

trait BijectionsChunkFutureByteArray{
  import BijectionsChunkByteArray._
  implicit def futureByteArrayToChunk(implicit timeout: Timeout = Timeout.zero) = new Bijection[Future[Array[Byte]], ByteChunk]{
    def apply(t: Future[Array[Byte]]) = Await.result(t.map(ArrayByteToChunk(_)), timeout.duration)

    def unapply(b: ByteChunk) = AggregatedByteChunk(b, None).map(ChunkToArrayByte(_))
  }

  implicit def chunkToFutureByteArray(implicit timeout: Timeout = Timeout.zero) = futureByteArrayToChunk(timeout).inverse
}

object BijectionsChunkFutureByteArray extends BijectionsChunkFutureJson
