package blueeyes.core.data

import blueeyes.concurrent.Future

trait BijectionsChunkString {
  implicit val StringToChunk = new Bijection[String, ByteChunk] {
    def apply(s: String): ByteChunk   = new MemoryChunk(s.getBytes("UTF-8"))
    def unapply(t: ByteChunk): String = new String(t.data, "UTF-8")
  }

  implicit val ChunkToString    = StringToChunk.inverse
}
object BijectionsChunkString extends BijectionsChunkString

trait BijectionsChunkFutureString{
  import BijectionsChunkString._
  implicit val FutureStringToChunk = new Bijection[Future[String], ByteChunk]{
    def apply(t: Future[String]) = t.map(StringToChunk(_)).value.getOrElse(new MemoryChunk(Array[Byte]()))

    def unapply(b: ByteChunk) = AggregatedByteChunk(b, None).map(ChunkToString(_))
  }

  implicit val ChunkToFutureString  = FutureStringToChunk.inverse
}

object BijectionsChunkFutureString extends BijectionsChunkFutureString