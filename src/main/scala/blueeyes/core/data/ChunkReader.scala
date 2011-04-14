package blueeyes.core.data

trait ChunkReader{
  def hasNextChunk(): Boolean

  def nextChunk: Array[Byte]
}

class OneChunkReader(val chunk: Array[Byte]) extends ChunkReader{
  private var done = false
  def nextChunk = {
    done = true
    chunk
  }

  def hasNextChunk() = !done
}