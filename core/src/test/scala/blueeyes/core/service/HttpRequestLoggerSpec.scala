package blueeyes.core.service

import org.specs2.mutable.Specification
import blueeyes.parsers.W3ExtendedLogAST._
import blueeyes.core.http._
import akka.dispatch.Future
import blueeyes.util.ClockMock
import org.joda.time.format.DateTimeFormat
import java.net.InetAddress
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.data.DefaultBijections._
import org.apache.commons.codec.binary.Base64

import blueeyes.bkka.AkkaDefaults
import blueeyes.akka_testing.FutureMatchers

class HttpRequestLoggerSpec extends Specification with ClockMock with FutureMatchers with AkkaDefaults {

  private val DateFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  private val TimeFormatter = DateTimeFormat.forPattern("HH:mm:ss.S")

  private val request        = HttpRequest[String](content = Some("request content"), method = HttpMethods.GET, uri = "/foo/bar?param=value", remoteHost = Some(InetAddress.getLocalHost), headers = Map[String, String]("content-language" -> "en"))
  private val response       = HttpResponse[String](content = Some("response content"), status = HttpStatus(Created), headers = Map[String, String]("content-length" -> "1000", "age" -> "3"))
  private val responseFuture = Future(response)

  "HttpRequestLogger: logs multiple values" in {
    log(DateIdentifier, TimeIdentifier) must whenDelivered { be_==((DateIdentifier, Left(DateFormatter.print(clockMock.now()))) :: (TimeIdentifier, Left(TimeFormatter.print(clockMock.now()))) :: Nil) }
  }
  "HttpRequestLogger: logs date" in {
    log(DateIdentifier) must whenDelivered { be_==((DateIdentifier, Left(DateFormatter.print(clockMock.now()))) :: Nil) }
  }
  "HttpRequestLogger: logs time" in {
    log(TimeIdentifier) must whenDelivered { be_==((TimeIdentifier, Left(TimeFormatter.print(clockMock.now()))) :: Nil) }
  }
  "HttpRequestLogger: logs time taken" in {
    log(TimeTakenIdentifier) must whenDelivered { be_==((TimeTakenIdentifier, Left("0.0")) :: Nil) }
  }
  "HttpRequestLogger: logs bytes" in {
    log(BytesIdentifier) must whenDelivered { be_==((BytesIdentifier, Left("1000")) :: Nil) }
  }
  "HttpRequestLogger: logs cached" in {
    log(CachedIdentifier) must whenDelivered { be_==((CachedIdentifier, Left("1")) :: Nil) }
  }
  "HttpRequestLogger: logs client ip" in {
    log(IpIdentifier(ClientPrefix)).map(Some(_)) must whenDelivered { be_==(request.remoteHost.map(v => (IpIdentifier(ClientPrefix), Left(v.getHostAddress)) :: Nil)) }
  }
  "HttpRequestLogger: logs server ip" in {
    log(IpIdentifier(ServerPrefix)) must whenDelivered { be_==((IpIdentifier(ServerPrefix), Left(InetAddress.getLocalHost.getHostAddress)) :: Nil) }
  }
  "HttpRequestLogger: logs client dns" in {
    log(DnsNameIdentifier(ClientPrefix)).map(Some(_)) must whenDelivered { be_==(request.remoteHost.map(v => (DnsNameIdentifier(ClientPrefix), Left(v.getHostName)) :: Nil)) }
  }
  "HttpRequestLogger: logs server dns" in {
    log(DnsNameIdentifier(ServerPrefix)) must whenDelivered { be_==((DnsNameIdentifier(ServerPrefix), Left(InetAddress.getLocalHost.getHostName)) :: Nil) }
  }
  "HttpRequestLogger: logs Status" in {
    log(StatusIdentifier(ServerToClientPrefix)) must whenDelivered { be_==((StatusIdentifier(ServerToClientPrefix), Left(response.status.code.name)) :: Nil) }
  }
  "HttpRequestLogger: logs comment" in {
    log(CommentIdentifier(ServerToClientPrefix)) must whenDelivered { be_==((CommentIdentifier(ServerToClientPrefix), Left(response.status.reason)) :: Nil) }
  }
  "HttpRequestLogger: logs method" in {
    log(MethodIdentifier(ClientToServerPrefix)) must whenDelivered { be_==((MethodIdentifier(ClientToServerPrefix), Left(request.method.value)) :: Nil) }
  }
  "HttpRequestLogger: logs uri" in {
    log(UriIdentifier(ClientToServerPrefix)) must whenDelivered { be_==((UriIdentifier(ClientToServerPrefix), Left(request.uri.toString)) :: Nil) }
  }
  "HttpRequestLogger: logs uri-stem" in {
    log(UriStemIdentifier(ClientToServerPrefix)).map(Some(_)) must whenDelivered { be_==(request.uri.path.map(v => (UriStemIdentifier(ClientToServerPrefix), Left(v)) :: Nil)) }
  }
  "HttpRequestLogger: logs uri-query" in {
    log(UriQueryIdentifier(ClientToServerPrefix)).map(Some(_)) must whenDelivered { be_==(request.uri.query.map(v => (UriQueryIdentifier(ClientToServerPrefix), Left(v)) :: Nil)) }
  }
  "HttpRequestLogger: logs request header" in {
    log(HeaderIdentifier(ClientToServerPrefix, "content-language")) must whenDelivered { be_==((HeaderIdentifier(ClientToServerPrefix, "content-language"), Left("en")) :: Nil) }
  }
  "HttpRequestLogger: logs response header" in {
    log(HeaderIdentifier(ServerToClientPrefix, "age")) must whenDelivered { be_==((HeaderIdentifier(ServerToClientPrefix, "age"), Left("3.0")) :: Nil) }
  }
  "HttpRequestLogger: logs request content" in {
    log(ContentIdentifier(ClientToServerPrefix)).map(_.map(toStringValues)) must whenDelivered (beEqualTo(List((ContentIdentifier(ClientToServerPrefix), Right(request.content.get)))))
  }
  "HttpRequestLogger: logs response content" in {
    log(ContentIdentifier(ServerToClientPrefix)).map(_.map(toStringValues)) must whenDelivered (beEqualTo(List((ContentIdentifier(ServerToClientPrefix), Right(response.content.get)))))
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
