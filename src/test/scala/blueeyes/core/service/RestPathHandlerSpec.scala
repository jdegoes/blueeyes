package blueeyes.core.service

import blueeyes.util.Future
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito.when
import org.specs.Specification

class RestPathHandlerSpec extends Specification with MockitoSugar {
  private val url         = "foo/bar"
  private val pattern     = mock[PartialFunction[String, Map[Symbol, String]]]
  private val handler     = mock[HttpRequest[Any] => Future[HttpResponse[Any]]]
  private val symbols     = Map[Symbol, String]('foo -> "bar")
  private val request     = HttpRequest[Any](HttpMethods.GET, "foo")
  private val request2    = HttpRequest[Any](HttpMethods.GET, "foo", symbols)
  private val pathHandler = RestPathHandler(pattern, handler)
  private val future      = mock[Future[HttpResponse[Any]]]

  "RestPathHandler must use pattern.isDefinedAt" in {
    when(pattern.isDefinedAt(url)).thenReturn(true)

    pathHandler.isDefinedAt(url) mustEqual(true)
  }
  "RestPathHandler create symbols and call handler" in {
    when(pattern.apply(url)).thenReturn(symbols)
    when(handler(request2)).thenReturn(future)

    pathHandler.apply(url)(request) mustEqual(future)
  }
}