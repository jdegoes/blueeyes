package blueeyes.core.data

import blueeyes.concurrent.{FutureDeliveryStrategy, Future}

trait Chunk{
  def data: Array[Byte]

  def next: Option[Future[Chunk]]
}

class MemoryChunk(val data: Array[Byte], f:() => Option[Future[Chunk]])(implicit deliveryStrategy: FutureDeliveryStrategy) extends Chunk{
  def this(data: Array[Byte])(implicit deliveryStrategy: FutureDeliveryStrategy) = this(data, () => None)
  def next = f()
}