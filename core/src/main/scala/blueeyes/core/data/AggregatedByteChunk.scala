package blueeyes.core.data

import blueeyes.bkka.AkkaDefaults
import blueeyes.util.metrics.DataSize
import akka.dispatch.Future
import akka.dispatch.Promise
import java.io.ByteArrayOutputStream
import scalaz.Scalaz._

object AggregatedByteChunk extends AkkaDefaults {
  def apply(chunk: ByteChunk, chunkSizeInBytes: Option[DataSize] = None): Future[ByteChunk] = {
    val promise = Promise[ByteChunk]()  
    aggregateContent(chunk, new ByteArrayOutputStream(), promise, chunkSizeInBytes)
    promise
  }

  private def aggregateContent(chunk: ByteChunk, buffer: ByteArrayOutputStream, result: Promise[ByteChunk], chunkSizeInBytes: Option[DataSize]) {
    def done = result.success {
      Chunk(buffer.toByteArray, chunk.next)
    }

    buffer.write(chunk.data)
    chunkSizeInBytes match {
      case Some(x) if (buffer.size >= x.size) => done
      case _ => chunk.next match{
        case None    => done
        case Some(e) => 
          e onSuccess { case nextChunk => aggregateContent(nextChunk, buffer, result, chunkSizeInBytes) }
          e onFailure { case error => result.failure(error) }
          result onFailure { case error => e.asInstanceOf[Promise[ByteChunk]].failure(error) }
      }
    }
  }
}
