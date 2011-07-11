package blueeyes.core.service

import blueeyes.concurrent.Future
import blueeyes.core.data.ByteChunk
import blueeyes.core.http.{HttpStatus, HttpResponse, HttpRequest, HttpStatusCodes}
import engines.HttpClientXLightWeb

trait ConfigurableHttpClient{

  private val isMock = sys.props.getOrElse(ConfigurableHttpClient.HttpClientSwitch, "true").toBoolean
  lazy val client: HttpClient[ByteChunk] = if (isMock) mockClient(mockServer) else realClient

  protected def realClient: HttpClient[ByteChunk] = new HttpClientXLightWeb{}

  private def mockClient(h: HttpRequestHandler[ByteChunk]): HttpClient[ByteChunk] = new HttpClient[ByteChunk] {
    def isDefinedAt(r: HttpRequest[ByteChunk]) = h.isDefinedAt(r)
    def apply(r: HttpRequest[ByteChunk]): Future[HttpResponse[ByteChunk]] = h.apply(r)
  }

  protected def mockServer: HttpRequestHandler[ByteChunk] = new HttpRequestHandler[ByteChunk]{
    def isDefinedAt(r: HttpRequest[ByteChunk]) = true
    def apply(r: HttpRequest[ByteChunk]): Future[HttpResponse[ByteChunk]] = Future.sync(HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.NotFound)))
  }
}

object ConfigurableHttpClient{
 val HttpClientSwitch = "httpclient.mock"
}