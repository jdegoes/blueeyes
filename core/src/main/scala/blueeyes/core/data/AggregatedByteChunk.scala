package blueeyes.core.data

import blueeyes.bkka.AkkaDefaults
import blueeyes.util.metrics.DataSize
import akka.dispatch.Future
import akka.dispatch.Promise
import java.io.ByteArrayOutputStream
import scalaz.Scalaz._

object AggregatedByteChunk extends AkkaDefaults {
  def apply(chunk: Chunk[Array[Byte]], chunkSizeInBytes: Option[DataSize] = None): Future[Chunk[Array[Byte]]] = {
    val promise = Promise[Chunk[Array[Byte]]]()  
    aggregateContent(chunk, new ByteArrayOutputStream(), promise, chunkSizeInBytes)
    promise
  }

  private def aggregateContent(chunk: Chunk[Array[Byte]], buffer: ByteArrayOutputStream, result: Promise[Chunk[Array[Byte]]], chunkSizeInBytes: Option[DataSize]) {
    def done = result.success {
      Chunk(buffer.toByteArray, chunk.next)
    }

    buffer.write(chunk.data)
    chunkSizeInBytes match {
      case Some(x) if (buffer.size >= x.size) => done
      case _ => chunk.next match {
        case None    => done
        case Some(e) => 
          e onSuccess { case nextChunk => aggregateContent(nextChunk, buffer, result, chunkSizeInBytes) }
          e onFailure { case error => result.failure(error) }
          result onFailure { case error => e.asInstanceOf[Promise[Chunk[Array[Byte]]]].failure(error) }
      }
    }
  }
}
