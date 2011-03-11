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
import org.specs.util.TimeConversions._

class BlueEyesServiceSpecificationSpec extends BlueEyesServiceSpecification[String] with TestService {

  path$("/bar/id/bar.html"){
    contentType$[String, String, Unit](text/html){
      get${ response: HttpResponse[String] =>
        response mustEqual(serviceResponse)
      }
    }
  } should "calls test function"

  path$("/asynch/future"){
    contentType$[String, String, Unit](text/html){
      get${ response: HttpResponse[String] =>
        response mustEqual(serviceResponse)
      }
    }
  } should eventually (40, 1000.milliseconds)("gets response when future is set asynchronously")

  path$("/asynch/eventually"){
    contentType$[String, String, Unit](text/html){
      get${ response: HttpResponse[String] =>
        response mustEqual(serviceResponse)
      }
    }
  } should eventually (10, 1000.milliseconds)("retry requests")
}

trait TestService extends BlueEyesServiceBuilderString {
  private var eventuallyCondition = false
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
            async {
              serviceResponse
            }
          }
        }~
        path("/asynch/eventually") {
          get { request: HttpRequest[String] =>
            if (eventuallyCondition) {
              serviceResponse
            } else {
              eventuallyCondition = true
              HttpResponse[String](HttpStatus(HttpStatusCodes.NotFound))
            }
          }
        }
      }
    }
  }

  private def async[T](f: => T): Future[T] = {
    val result = new Future[T]

    import scala.actors.Actor.actor
    actor {
      Thread.sleep(10000)
      result.deliver(f)
    }

    result
  }
} 

object TestService{
  val serviceResponse = HttpResponse[String](HttpStatus(HttpStatusCodes.OK), Map("Content-Type" -> "text/html"), Some("context"), HttpVersions.`HTTP/1.1`)
}
