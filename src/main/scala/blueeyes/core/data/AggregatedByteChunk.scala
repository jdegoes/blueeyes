package blueeyes.core.data

import blueeyes.util.metrics.DataSize
import blueeyes.concurrent.Future
import java.io.ByteArrayOutputStream
import scalaz.Scalaz._

object AggregatedByteChunk {
  def apply(chunk: ByteChunk, chunkSizeInBytes: Option[DataSize] = None): Future[ByteChunk] = new Future[ByteChunk]  ->- {aggregateContent(chunk, new ByteArrayOutputStream(), _, chunkSizeInBytes)}

  private def aggregateContent(chunk: ByteChunk, buffer: ByteArrayOutputStream, result: Future[ByteChunk], chunkSizeInBytes: Option[DataSize]) {
    def done = {
      val aggregated = new MemoryChunk(buffer.toByteArray){
        override def next = chunk.next
      }
      result.deliver(aggregated)
    }

    buffer.write(chunk.data)
    chunkSizeInBytes match {
      case Some(x) if (buffer.size >= x.size) => done
      case _ => chunk.next match{
        case None    => done
        case Some(e) => {
          e.deliverTo(nextChunk => aggregateContent(nextChunk, buffer, result, chunkSizeInBytes))
          e.ifCanceled(error => result.cancel(error))
          result.ifCanceled(error => e.cancel(error))
        }
      }
    }
  }

}