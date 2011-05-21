package blueeyes.core.service

import java.io.ByteArrayOutputStream
import blueeyes.core.http.{HttpResponse, HttpRequest}
import blueeyes.concurrent.{Future, FutureDeliveryStrategySequential}
import blueeyes.core.data.{MemoryChunk, ByteChunk}

trait HttpClientByteChunk extends HttpClient[ByteChunk] with FutureDeliveryStrategySequential{ self =>
  def aggregate(chunkSize: Option[Int]) = new HttpClient[ByteChunk] {
    def isDefinedAt(request: HttpRequest[ByteChunk]) = self.isDefinedAt(request)

    def apply(request: HttpRequest[ByteChunk]) = {
      self.apply(request) flatMap { response =>
        def aggregateContent(chunk: ByteChunk, buffer: ByteArrayOutputStream, result: Future[HttpResponse[ByteChunk]]) {
          def done = {
            val content = new MemoryChunk(buffer.toByteArray){
              override def next = chunk.next
            }
            result.deliver(response.copy(content = Some(content)))
          }

          buffer.write(chunk.data)
          chunkSize match {
            case Some(size) if (buffer.size >= size) => done
            case _ => chunk.next match{
              case None    => done
              case Some(e) => e.deliverTo(nextChunk => aggregateContent(nextChunk, buffer, result))
            }
          }
        }
        response.content match {
          case Some(chunk) =>
            val newResponse = new Future[HttpResponse[ByteChunk]]()
            aggregateContent(chunk, new ByteArrayOutputStream(), newResponse)
            newResponse
          case None        => Future(response)
        }
      }
    }
  }
}