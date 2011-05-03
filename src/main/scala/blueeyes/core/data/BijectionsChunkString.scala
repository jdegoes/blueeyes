package blueeyes.core.data

trait BijectionsChunkString {
  implicit val StringToChunk = new Bijection[String, ByteChunk] {
    def apply(s: String): ByteChunk   = new MemoryChunk(s.getBytes("UTF-8"))
    def unapply(t: ByteChunk): String = new String(t.data, "UTF-8")
  }

  implicit val ChunkToString    = StringToChunk.inverse
}
object BijectionsChunkString extends BijectionsChunkString