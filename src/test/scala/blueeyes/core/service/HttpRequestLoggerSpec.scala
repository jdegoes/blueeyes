package blueeyes.core.service

import org.specs.Specification
import blueeyes.parsers.W3ExtendedLogAST._
import blueeyes.core.http._
import blueeyes.concurrent.{Future, FutureDeliveryStrategySequential}
import blueeyes.util.ClockMock
import org.joda.time.format.DateTimeFormat
import java.net.InetAddress
import blueeyes.core.http.HttpStatusCodes._

class HttpRequestLoggerSpec extends Specification with ClockMock with FutureDeliveryStrategySequential{

  private val DateFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  private val TimeFormatter = DateTimeFormat.forPattern("HH:mm:ss.S")

  private val request        = HttpRequest[String](method = HttpMethods.GET, uri = "/foo/bar?param=value", remoteHost = Some(InetAddress.getLocalHost), headers = Map[String, String]("content-language" -> "en"))
  private val response       = HttpResponse[String](status = HttpStatus(Created), headers = Map[String, String]("content-length" -> "1000", "age" -> "3"))
  private val responseFuture = Future.lift(response)

  "HttpRequestLogger: logs multiple values" in {
    log(DateIdentifier, TimeIdentifier).value must eventually (beSome(DateFormatter.print(clockMock.now()) + " " + TimeFormatter.print(clockMock.now())))
  }
  "HttpRequestLogger: logs date" in {
    log(DateIdentifier).value must eventually (beSome(DateFormatter.print(clockMock.now())))
  }
  "HttpRequestLogger: logs time" in {
    log(TimeIdentifier).value must eventually (beSome(TimeFormatter.print(clockMock.now())))
  }
  "HttpRequestLogger: logs time taken" in {
    log(TimeTakenIdentifier).value must eventually (beSome("0.0"))
  }
  "HttpRequestLogger: logs bytes" in {
    log(BytesIdentifier).value must eventually (beSome("1000"))
  }
  "HttpRequestLogger: logs cached" in {
    log(CachedIdentifier).value must eventually (beSome("1"))
  }
  "HttpRequestLogger: logs client ip" in {
    log(IpIdentifier(ClientPrefix)).value must eventually (is_==(request.remoteHost.map(_.getHostAddress)))
  }
  "HttpRequestLogger: logs server ip" in {
    log(IpIdentifier(ServerPrefix)).value must eventually (beSome(InetAddress.getLocalHost.getHostAddress))
  }
  "HttpRequestLogger: logs client dns" in {
    log(DnsNameIdentifier(ClientPrefix)).value must eventually (is_==(request.remoteHost.map(_.getHostName)))
  }
  "HttpRequestLogger: logs server dns" in {
    log(DnsNameIdentifier(ServerPrefix)).value must eventually (beSome(InetAddress.getLocalHost.getHostName))
  }
  "HttpRequestLogger: logs Status" in {
    log(StatusIdentifier(ServerToClientPrefix)).value must eventually (beSome(response.status.code.name))
  }
  "HttpRequestLogger: logs comment" in {
    log(CommentIdentifier(ServerToClientPrefix)).value must eventually (beSome(response.status.reason))
  }
  "HttpRequestLogger: logs method" in {
    log(MethodIdentifier(ClientToServerPrefix)).value must eventually (beSome(request.method.value))
  }
  "HttpRequestLogger: logs uri" in {
    log(UriIdentifier(ClientToServerPrefix)).value must eventually (beSome(request.uri))
  }
  "HttpRequestLogger: logs uri-stem" in {
    log(UriStemIdentifier(ClientToServerPrefix)).value must eventually (beSome(request.path))
  }
  "HttpRequestLogger: logs uri-query" in {
    log(UriQueryIdentifier(ClientToServerPrefix)).value must eventually (beSome(request.query))
  }
  "HttpRequestLogger: logs request header" in {
    log(HeaderIdentifier(ClientToServerPrefix, "content-language")).value must eventually (beSome("en"))
  }
  "HttpRequestLogger: logs response header" in {
    log(HeaderIdentifier(ServerToClientPrefix, "age")).value must eventually (beSome("3"))
  }

  private def log(fieldIdentifiers: FieldIdentifier*): Future[String] = HttpRequestLogger[String, String](FieldsDirective(List(fieldIdentifiers: _*)))(clockMock)(request, responseFuture)
}