package blueeyes.core.service

import org.specs2.mutable.Specification
import blueeyes.parsers.W3ExtendedLogAST._
import blueeyes.core.http._
import blueeyes.concurrent.Future
import blueeyes.util.ClockMock
import org.joda.time.format.DateTimeFormat
import java.net.InetAddress
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.data.BijectionsChunkString._
import org.apache.commons.codec.binary.Base64

class HttpRequestLoggerSpec extends Specification with ClockMock{

  private val DateFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  private val TimeFormatter = DateTimeFormat.forPattern("HH:mm:ss.S")

  private val request        = HttpRequest[String](content = Some("request content"), method = HttpMethods.GET, uri = "/foo/bar?param=value", remoteHost = Some(InetAddress.getLocalHost), headers = Map[String, String]("content-language" -> "en"))
  private val response       = HttpResponse[String](content = Some("response content"), status = HttpStatus(Created), headers = Map[String, String]("content-length" -> "1000", "age" -> "3"))
  private val responseFuture = Future.sync(response)

  "HttpRequestLogger: logs multiple values" in {
    log(DateIdentifier, TimeIdentifier).value must eventually (beSome((DateIdentifier, Left(DateFormatter.print(clockMock.now()))) :: (TimeIdentifier, Left(TimeFormatter.print(clockMock.now()))) :: Nil))
  }
  "HttpRequestLogger: logs date" in {
    log(DateIdentifier).value must eventually (beSome((DateIdentifier, Left(DateFormatter.print(clockMock.now()))) :: Nil))
  }
  "HttpRequestLogger: logs time" in {
    log(TimeIdentifier).value must eventually (beSome((TimeIdentifier, Left(TimeFormatter.print(clockMock.now()))) :: Nil))
  }
  "HttpRequestLogger: logs time taken" in {
    log(TimeTakenIdentifier).value must eventually (beSome((TimeTakenIdentifier, Left("0.0")) :: Nil))
  }
  "HttpRequestLogger: logs bytes" in {
    log(BytesIdentifier).value must eventually (beSome((BytesIdentifier, Left("1000")) :: Nil))
  }
  "HttpRequestLogger: logs cached" in {
    log(CachedIdentifier).value must eventually (beSome((CachedIdentifier, Left("1")) :: Nil))
  }
  "HttpRequestLogger: logs client ip" in {
    log(IpIdentifier(ClientPrefix)).value must eventually (beEqualTo(request.remoteHost.map(v => (IpIdentifier(ClientPrefix), Left(v.getHostAddress)) :: Nil)))
  }
  "HttpRequestLogger: logs server ip" in {
    log(IpIdentifier(ServerPrefix)).value must eventually (beSome((IpIdentifier(ServerPrefix), Left(InetAddress.getLocalHost.getHostAddress)) :: Nil))
  }
  "HttpRequestLogger: logs client dns" in {
    log(DnsNameIdentifier(ClientPrefix)).value must eventually (beEqualTo(request.remoteHost.map(v => (DnsNameIdentifier(ClientPrefix), Left(v.getHostName)) :: Nil)))
  }
  "HttpRequestLogger: logs server dns" in {
    log(DnsNameIdentifier(ServerPrefix)).value must eventually (beSome((DnsNameIdentifier(ServerPrefix), Left(InetAddress.getLocalHost.getHostName)) :: Nil))
  }
  "HttpRequestLogger: logs Status" in {
    log(StatusIdentifier(ServerToClientPrefix)).value must eventually (beSome((StatusIdentifier(ServerToClientPrefix), Left(response.status.code.name)) :: Nil))
  }
  "HttpRequestLogger: logs comment" in {
    log(CommentIdentifier(ServerToClientPrefix)).value must eventually (beSome((CommentIdentifier(ServerToClientPrefix), Left(response.status.reason)) :: Nil))
  }
  "HttpRequestLogger: logs method" in {
    log(MethodIdentifier(ClientToServerPrefix)).value must eventually (beSome((MethodIdentifier(ClientToServerPrefix), Left(request.method.value)) :: Nil))
  }
  "HttpRequestLogger: logs uri" in {
    log(UriIdentifier(ClientToServerPrefix)).value must eventually (beSome((UriIdentifier(ClientToServerPrefix), Left(request.uri.toString)) :: Nil))
  }
  "HttpRequestLogger: logs uri-stem" in {
    log(UriStemIdentifier(ClientToServerPrefix)).value must eventually (beEqualTo(request.uri.path.map(v => (UriStemIdentifier(ClientToServerPrefix), Left(v)) :: Nil)))
  }
  "HttpRequestLogger: logs uri-query" in {
    log(UriQueryIdentifier(ClientToServerPrefix)).value must eventually (beEqualTo(request.uri.query.map(v => (UriQueryIdentifier(ClientToServerPrefix), Left(v)) :: Nil)))
  }
  "HttpRequestLogger: logs request header" in {
    log(HeaderIdentifier(ClientToServerPrefix, "content-language")).value must eventually (beSome((HeaderIdentifier(ClientToServerPrefix, "content-language"), Left("en")) :: Nil))
  }
  "HttpRequestLogger: logs response header" in {
    log(HeaderIdentifier(ServerToClientPrefix, "age")).value must eventually (beSome((HeaderIdentifier(ServerToClientPrefix, "age"), Left("3.0")) :: Nil))
  }
  "HttpRequestLogger: logs request content" in {
    log(ContentIdentifier(ClientToServerPrefix)).value.get.map{toStringValues(_)} must eventually (beEqualTo(List((ContentIdentifier(ClientToServerPrefix), Right(request.content.get)))))
  }
  "HttpRequestLogger: logs response content" in {
    log(ContentIdentifier(ServerToClientPrefix)).value.get.map{toStringValues(_)} must eventually (beEqualTo(List((ContentIdentifier(ServerToClientPrefix), Right(response.content.get)))))
  }
  private def toStringValues(v: (FieldIdentifier, Either[String, Array[Byte]])): Tuple2[FieldIdentifier, Either[String, String]] = {
    val value = v._2 match{
      case Right(value) => Right[String, String](new String(value, "UTF-8"))
      case Left(value) => Left[String, String](value)
    }
    Tuple2[FieldIdentifier, Either[String, String]](v._1, value)
  }
  private def log(fieldIdentifiers: FieldIdentifier*): Future[List[(FieldIdentifier, Either[String, Array[Byte]])]] = HttpRequestLogger[String, String](FieldsDirective(List(fieldIdentifiers: _*))).apply(request, responseFuture)
}
