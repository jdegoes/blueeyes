package blueeyes.core.service

import org.specs.Specification
import blueeyes.parsers.W3ExtendedLogAST._
import blueeyes.core.http._
import blueeyes.util.{Future, ClockMock}
import org.joda.time.format.DateTimeFormat

class HttpRequestLoggerSpec extends Specification with ClockMock{

  private val DateFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  private val TimeFormatter = DateTimeFormat.forPattern("HH:mm:ss.S")

  private val request  = HttpRequest[String](method = HttpMethods.GET, uri = "/foo/bar?param=value")
  private val response = Future.lift(HttpResponse[String]())

  "HttpRequestLogger: logs multiple values" in {
    log(DateIdentifier, TimeIdentifier).value must eventually (beSome(DateFormatter.print(clockMock.now()) + " " + TimeFormatter.print(clockMock.now())))
  }
  "HttpRequestLogger: logs date" in {
    log(DateIdentifier).value must eventually (beSome(DateFormatter.print(clockMock.now())))
  }
  "HttpRequestLogger: logs time" in {
    log(TimeIdentifier).value must eventually (beSome(TimeFormatter.print(clockMock.now())))
  }

  private def log(fieldIdentifiers: FieldIdentifier*): Future[String] = HttpRequestLogger[String, String](FieldsDirective(List(fieldIdentifiers: _*)))(clockMock)(request, response)
}