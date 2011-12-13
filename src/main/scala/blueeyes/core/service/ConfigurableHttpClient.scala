package blueeyes.core.service

import blueeyes.bkka.AkkaDefaults
import akka.dispatch.Future

import blueeyes.core.data.ByteChunk
import blueeyes.core.http._
import blueeyes.core.service._
import engines.HttpClientXLightWeb

import scalaz.{Failure, Success}
import scalaz.Scalaz._

trait ConfigurableHttpClient extends AkkaDefaults {
  private lazy val InternalServerError = HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.InternalServerError))
  private lazy val NotFound            = Future(HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.NotFound)))

  lazy implicit val httpClient: HttpClientByteChunk = {
    val isMock = sys.props.getOrElse(ConfigurableHttpClient.HttpClientSwitch, "false").toBoolean
    if (isMock) mockClient(mockServer) else realClient
  }

  protected def realClient: HttpClientByteChunk = new HttpClientXLightWeb{}

  private def mockClient(h: AsyncHttpService[ByteChunk]): HttpClientByteChunk = new HttpClientByteChunk {
    def isDefinedAt(r: HttpRequest[ByteChunk]) = true
    def apply(r: HttpRequest[ByteChunk]): Future[HttpResponse[ByteChunk]] = h.service(r) match {
      case Success(rawFuture) => rawFuture recover { case throwable => convertErrorToResponse(throwable) }
      case Failure(DispatchError(throwable)) => Future(convertErrorToResponse(throwable))
      case failure => NotFound
    }
  }

  private def convertErrorToResponse(th: Throwable): HttpResponse[ByteChunk] = th match {
    case e: HttpException => HttpResponse[ByteChunk](HttpStatus(e.failure, e.reason))
    case e => HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.InternalServerError, Option(e.getMessage).getOrElse("")))
  }

  protected def mockServer: AsyncHttpService[ByteChunk] = new AsyncCustomHttpService[ByteChunk]{
    def service = (request: HttpRequest[ByteChunk]) => success(Future(HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.NotFound))))
    val metadata = None
  }
}

object ConfigurableHttpClient{
 val HttpClientSwitch = "httpclient.mock"
}
