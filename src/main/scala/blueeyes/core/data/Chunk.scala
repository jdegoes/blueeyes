package blueeyes.core.data

import blueeyes.concurrent.Future

trait Chunk[T]{
  def data: T

  def next: Option[Future[Chunk[T]]]
}

class MemoryChunk[T](val data: T, f: () => Option[Future[Chunk[T]]]) extends Chunk[T]{
  def this(data: T) = this(data, () => None)
  def next = f()
}
