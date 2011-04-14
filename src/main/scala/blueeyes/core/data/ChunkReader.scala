package blueeyes.core.data

trait ChunkReader{
  def hasNextChunk(): Boolean

  def nextChunk: Array[Byte]

  def close: Unit

  def contentLength: Long
}

class OneChunkReader(val chunk: Array[Byte]) extends ChunkReader{
  private var done = false
  def nextChunk = {
    done = true
    chunk
  }

  def hasNextChunk() = !done

  def contentLength = chunk.length

  def close = {}
}

class MultiChunkReader(val contentLength: Long, f: () => Option[Array[Byte]])  extends ChunkReader{
  private var currentChunk :Option[Array[Byte]] = None
  def close = {}

  def nextChunk = currentChunk.getOrElse(f().get)

  def hasNextChunk() = {
    currentChunk = f()

    currentChunk.map(v => true).getOrElse(false)
  }
}