package blueeyes.core.service.test

import org.specs.Specification
import blueeyes.core.service.RestPathPatternImplicits._
import blueeyes.core.service._
import blueeyes.util.Future
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus, HttpStatusCodes}
import blueeyes.BlueEyesServiceBuilderString

class BlueEyesServiceSpecificationSampleSpec extends BlueEyesServiceSpecification[String] with SampleService{

  "SampleService" should{
    "when using GET /get/'foo return foo value as response content" in {
      path("/get/foo-value"){
        get{
          responseStatus  mustEqual(HttpStatus(OK))
          responseContent mustEqual(Some("foo-value"))
          ()
        }
      }
    }
    "when using POST /post/foo should return request content as response content" in {
      path("/post/foo"){
        post({
          responseStatus  mustEqual(HttpStatus(OK))
          responseContent mustEqual(Some("post-content"))
          ()
        }, Map(), Map(), Some("post-content"))
      }
    }
  }
}

trait SampleService extends BlueEyesServiceBuilderString {
  import blueeyes.core.http.MimeTypes._
  
  val sampleService = service("sample", "1.32") { context =>
    request {
      contentType(text/plain) {
        path("/get/'foo") {
          get [String]{ request: HttpRequest[String] =>
            val fooValue = request.parameters.get('foo).getOrElse("")
            val response = HttpResponse[String](status = HttpStatus(HttpStatusCodes.OK), content = Some(fooValue))
            new Future[HttpResponse[String]]().deliver(response)
          }
        } ~
        path("/post/foo") {
          post [String]{ request: HttpRequest[String] =>
            val response = HttpResponse[String](status = HttpStatus(HttpStatusCodes.OK), content = request.content)
            new Future[HttpResponse[String]]().deliver(response)
          }
        }
      }
    }
  }
}
