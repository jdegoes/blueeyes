package blueeyes.core.service.test

import org.specs.Specification
import blueeyes.core.service.RestPathPatternImplicits._
import blueeyes.core.service._
import blueeyes.concurrent.Future
import blueeyes.core.http.MimeTypes._
import blueeyes.BlueEyesServiceBuilderString
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http._
import TestService._
import org.specs.util._
import org.specs.util.TimeConversions._

class BlueEyesServiceSpecificationSpec extends BlueEyesServiceSpecification[String] with TestService {
  "Service Specification" should {
    def client = service.contentType[String](text/html)
    "support get by valid URL" in {
      val f = client.get("/bar/id/bar.html")
      f.value must eventually (beSome(serviceResponse))
    }
    "support asynch get by valid URL" in {
      val f = client.get("/asynch/future")
      f.value must eventually(5, new Duration(10000)) (beSome(serviceResponse))
    }
    "support eventually asynch get by valid URL" in { client: HttpClient[String] =>
      client.get("/asynch/eventually").value must eventually (beSome(serviceResponse))
    }
  }
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
