package blueeyes.core.data

import blueeyes.concurrent.Future

trait Chunk{
  def data: Array[Byte]

  def next: Option[Future[Chunk]]
}

class MemoryChunk(val data: Array[Byte], f:() => Option[Future[Chunk]])extends Chunk{
  def this(data: Array[Byte]) = this(data, () => None)
  def next = f()
}