package blueeyes.core.service

import org.specs2.mutable.Specification
import blueeyes.core.http._
import blueeyes.core.http.test._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.util.RichThrowableImplicits._
import akka.dispatch.Future
import akka.dispatch.Promise

class HttpResponseHelpersSpec extends Specification with HttpResponseHelpers with HttpRequestMatchers {
  
  "HttpResponseHelpers respond: creates Future with the specified parameters" in {
    val statusCode  = InternalServerError
    val headers = Map("foo" -> "bar")
    val content = "zoo"

    respond(HttpStatus(statusCode), headers, Some(content)) must whenDelivered {
      beLike {
        case HttpResponse(HttpStatus(code, _), HttpHeaders(h), c, _) => 
          (code must_== statusCode) and (h must_== headers) and (c must beSome(content))
      }
    }
  }

  "HttpResponseHelpers respondLater: creates Future when response is OK" in {
    val headers = Map("foo" -> "bar")
    val content = "zoo"

    respondLater[String](Future(content), headers) must whenDelivered {
      beLike { 
        case HttpResponse(HttpStatus(code, _), HttpHeaders(h), c, _) => 
          (code must_== OK) and (h must_== headers) and (c must beSome(content))
      }
    }
  }

  "HttpResponseHelpers respondLater: creates Future when response is error (Future is cancelled with error)" in {
    val error   = new NullPointerException()
    val promise = Promise.failed[String](error)
    respondLater[String](promise) must respondWithCode(InternalServerError)
  }

  "HttpResponseHelpers respondLater: creates Future when response is error (Future is cancelled without error)" in {
    val error   = new NullPointerException()
    val promise = Promise[String]
    promise.failure(new RuntimeException())

    respondLater[String](promise) must respondWithCode(InternalServerError)
  }
}
