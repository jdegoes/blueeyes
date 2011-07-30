package blueeyes.core.service

import blueeyes.core.http.{HttpResponse, HttpRequest}
import blueeyes.concurrent.Future
import blueeyes.util.metrics.DataSize
import blueeyes.core.data.{AggregatedByteChunk, ByteChunk}

trait HttpClientByteChunk extends HttpClient[ByteChunk]{ self =>
  def aggregate(chunkSize: Option[DataSize]) = new HttpClient[ByteChunk] {
    val chunkSizeInBytes = chunkSize.map(_.bytes)
    def isDefinedAt(request: HttpRequest[ByteChunk]) = self.isDefinedAt(request)

    def apply(request: HttpRequest[ByteChunk]) = {
      self.apply(request) flatMap { response =>
        response.content match {
          case Some(chunk) =>
            val newResponse = new Future[HttpResponse[ByteChunk]]()

            val aggregatedFuture = AggregatedByteChunk(chunk, chunkSize)
            aggregatedFuture.deliverTo{aggregated =>
              newResponse.deliver(response.copy(content = Some(aggregated)))
            }
            aggregatedFuture.ifCanceled(th => newResponse.cancel(th))
            newResponse.ifCanceled(th => aggregatedFuture.cancel(th))

            newResponse
          case None        => Future.sync(response)
        }
      }
    }
  }
}
