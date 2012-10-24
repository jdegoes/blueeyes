package blueeyes.core.service

import akka.dispatch.Future
import akka.dispatch.Promise

import blueeyes.bkka.AkkaDefaults
import blueeyes.core.data._
import blueeyes.core.http.{HttpResponse, HttpRequest}
import blueeyes.util.metrics.DataSize

trait HttpClientByteChunk extends HttpClient[Chunk[Array[Byte]]] with AkkaDefaults { self =>
  def aggregate(chunkSize: Option[DataSize]) = new HttpClient[Chunk[Array[Byte]]] {
    val chunkSizeInBytes = chunkSize.map(_.bytes)
    def isDefinedAt(request: HttpRequest[Chunk[Array[Byte]]]) = self.isDefinedAt(request)

    def apply(request: HttpRequest[Chunk[Array[Byte]]]) = {
      self.apply(request) flatMap { response =>
        response.content match {
          case Some(chunk) => AggregatedByteChunk(chunk, chunkSize).map(c => response.copy(content = Some(c)))
          case None        => Promise.successful(response)
        }
      }
    }
  }
}
