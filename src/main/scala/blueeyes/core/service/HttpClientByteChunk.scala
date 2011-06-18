package blueeyes.core.service

import java.io.ByteArrayOutputStream
import blueeyes.core.http.{HttpResponse, HttpRequest}
import blueeyes.concurrent.Future
import blueeyes.core.data.{MemoryChunk, ByteChunk}
import blueeyes.util.metrics.DataSize

trait HttpClientByteChunk extends HttpClient[ByteChunk]{ self =>
  def aggregate(chunkSize: Option[DataSize]) = new HttpClient[ByteChunk] {
    val chunkSizeInBytes = chunkSize.map(_.bytes)
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
          chunkSizeInBytes match {
            case Some(x) if (buffer.size >= x.size) => done
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
          case None        => Future.sync(response)
        }
      }
    }
  }
}
