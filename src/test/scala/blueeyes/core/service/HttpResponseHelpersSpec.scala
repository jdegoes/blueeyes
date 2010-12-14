package blueeyes.core.service

import org.spex.Specification
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.util.RichThrowableImplicits._
import org.specs.matcher.Matchers._
import blueeyes.util.Future

class HttpResponseHelpersSpec extends Specification with HttpResponseHelpers{
  
  "HttpResponseHelpers respond: creates Future with the specified parameters" in {
    val status  = HttpStatus(InternalServerError)
    val headers = Map("foo" -> "bar")
    val content = Some("zoo")
    respond(status, headers, content).value.get mustEqual(HttpResponse[String](status, headers, content))
  }
  "HttpResponseHelpers respondLater: creates Future when response is OK" in {
    val headers = Map("foo" -> "bar")
    val content = Future("zoo")
    respondLater[String](content, headers).value.get mustEqual(HttpResponse[String](HttpStatus(OK), headers, Some("zoo")))
  }
  "HttpResponseHelpers respondLater: creates Future when response is error (Future is cancelled with error)" in {
    val error   = new NullPointerException()
    val content = Future.dead[String](error)
    respondLater[String](content).value.get mustEqual(HttpResponse[String](HttpStatus(InternalServerError, error.fullStackTrace)))
  }
  "HttpResponseHelpers respondLater: creates Future when response is error (Future is cancelled without error)" in {
    val error   = new NullPointerException()
    val content = new Future[String]
    content.cancel(None)

    val status = respondLater[String](content).value.get.status
    status.code mustEqual(InternalServerError)
  }
}