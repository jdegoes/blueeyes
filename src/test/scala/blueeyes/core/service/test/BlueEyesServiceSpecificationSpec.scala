package blueeyes.core.service.test

import org.specs.Specification
import blueeyes.core.service.RestPathPatternImplicits._
import blueeyes.core.service._
import blueeyes.util.Future
import blueeyes.core.http.MimeTypes._
import blueeyes.BlueEyesServiceBuilderString
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http._
import TestService._

class BlueEyesServiceSpecificationSpec extends BlueEyesServiceSpecification[String] with TestService{

  path$("/bar/id/bar.html"){
    get${ response: HttpResponse[String] =>
      response mustEqual(serviceResponse)
    }
  } should "calls test function"

  path$("/asynch/future"){
    get${ response: HttpResponse[String] =>
      response mustEqual(serviceResponse)
    }
  } should "gets responce when future is set asynchronously"
}

trait TestService extends BlueEyesServiceBuilderString {
  val sampleService = service("sample", "1.32") { context =>
    request {
      contentType(text/html) {
        path("/bar/'foo/bar.html") {
          get { request: HttpRequest[String] =>
            serviceResponse
          }
        }~
        path("/asynch/future") {
          get { request: HttpRequest[String] =>
            Future.async {
              serviceResponse
            }
          }
        }
      }
    }
  }
} 

object TestService{
  val serviceResponse = HttpResponse[String](HttpStatus(HttpStatusCodes.OK), Map("Content-Type" -> "text/html"), Some("context"), HttpVersions.`HTTP/1.1`)
}
