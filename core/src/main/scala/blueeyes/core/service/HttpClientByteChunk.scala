package blueeyes.core.service

import akka.dispatch.Future
import akka.dispatch.Promise
import blueeyes.bkka.AkkaDefaults
import blueeyes.core.http.{HttpResponse, HttpRequest}
import blueeyes.util.metrics.DataSize
import blueeyes.core.data.{AggregatedByteChunk, ByteChunk}

trait HttpClientByteChunk extends HttpClient[ByteChunk] with AkkaDefaults { self =>
  def aggregate(chunkSize: Option[DataSize]) = new HttpClient[ByteChunk] {
    val chunkSizeInBytes = chunkSize.map(_.bytes)
    def isDefinedAt(request: HttpRequest[ByteChunk]) = self.isDefinedAt(request)

    def apply(request: HttpRequest[ByteChunk]) = {
      self.apply(request) flatMap { response =>
        response.content match {
          case Some(chunk) => AggregatedByteChunk(chunk, chunkSize).map(c => response.copy(content = Some(c)))
          case None        => Promise.successful(response)
        }
      }
    }
  }
}
