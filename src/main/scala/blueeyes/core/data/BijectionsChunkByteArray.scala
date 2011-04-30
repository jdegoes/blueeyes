package blueeyes.core.data

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