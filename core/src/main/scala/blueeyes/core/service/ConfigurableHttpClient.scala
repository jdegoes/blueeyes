package blueeyes.core.service

import blueeyes.bkka.AkkaDefaults
import akka.dispatch.Future

import blueeyes.core.data.Chunk
import blueeyes.core.http._
import blueeyes.core.service._
import engines.HttpClientXLightWeb

import scalaz.{Failure, Success}
import scalaz.Validation._
import blueeyes.Environment

trait ConfigurableHttpClient extends AkkaDefaults {
  private lazy val InternalServerError = HttpResponse[Chunk[Array[Byte]]](HttpStatus(HttpStatusCodes.InternalServerError))
  private lazy val NotFound            = Future(HttpResponse[Chunk[Array[Byte]]](HttpStatus(HttpStatusCodes.NotFound)))

  lazy implicit val httpClient: HttpClientByteChunk = {
    val isMock = sys.props.getOrElse(Environment.MockSwitch, "false").toBoolean
    if (isMock) mockClient(mockServer) else realClient
  }

  protected def realClient: HttpClientByteChunk = new HttpClientXLightWeb{}

  private def mockClient(h: AsyncHttpService[Chunk[Array[Byte]]]): HttpClientByteChunk = new HttpClientByteChunk {
    def isDefinedAt(r: HttpRequest[Chunk[Array[Byte]]]) = true
    def apply(r: HttpRequest[Chunk[Array[Byte]]]): Future[HttpResponse[Chunk[Array[Byte]]]] = h.service(r) match {
      case Success(rawFuture) => rawFuture recover { case throwable => convertErrorToResponse(throwable) }
      case Failure(DispatchError(throwable)) => Future(convertErrorToResponse(throwable))
      case failure => NotFound
    }
  }

  private def convertErrorToResponse(th: Throwable): HttpResponse[Chunk[Array[Byte]]] = th match {
    case e: HttpException => HttpResponse[Chunk[Array[Byte]]](HttpStatus(e.failure, e.reason))
    case e => HttpResponse[Chunk[Array[Byte]]](HttpStatus(HttpStatusCodes.InternalServerError, Option(e.getMessage).getOrElse("")))
  }

  protected def mockServer: AsyncHttpService[Chunk[Array[Byte]]] = new CustomHttpService[Chunk[Array[Byte]], Future[HttpResponse[Chunk[Array[Byte]]]]] {
    def service = (request: HttpRequest[Chunk[Array[Byte]]]) => success(Future(HttpResponse[Chunk[Array[Byte]]](status = HttpStatus(HttpStatusCodes.NotFound))))
    val metadata = None
  }
}
