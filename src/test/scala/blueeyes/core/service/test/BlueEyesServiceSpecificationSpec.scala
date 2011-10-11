package blueeyes.core.service.test

import org.specs.Specification
import blueeyes.core.service.RestPathPatternImplicits._
import blueeyes.core.service._
import blueeyes.concurrent.Future
import blueeyes.core.http.MimeTypes._
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http._
import blueeyes.core.data.{ByteChunk, BijectionsChunkString}
import TestService._
import org.specs.util._
import org.specs.util.TimeConversions._
import blueeyes.concurrent.test.FutureMatchers

class BlueEyesServiceSpecificationSpec extends BlueEyesServiceSpecification with TestService with BijectionsChunkString with FutureMatchers{
  "Service Specification" should {
    "support get by valid URL" in {
      val f = service.get[String]("/bar/id/bar.html")
      f.value must eventually (beSome(serviceResponse))
    }
    "support asynch get by valid URL" in {
      val f = service.get[String]("/asynch/future")
      f.value must eventually(5, new Duration(10000)) (beSome(serviceResponse))
    }
    "support eventually asynch get by valid URL" in {
      service.get[String]("/asynch/eventually") must whenDelivered {
        verify {
          _ mustEqual(serviceResponse)
        }
      }
    }
  }
}

trait TestService extends BlueEyesServiceBuilder with BijectionsChunkString{
  private var eventuallyCondition = false
  val sampleService = service("sample", "1.32") { context =>
    request {
      produce(text/html) {
        path("/bar/'foo/bar.html") {
          get { request: HttpRequest[ByteChunk] =>
            serviceResponse.future
          }
        }~
        path("/asynch/future") {
          get { request: HttpRequest[ByteChunk] =>
            async {
              serviceResponse
            }
          }
        }~
        path("/asynch/eventually") {
          get { request: HttpRequest[ByteChunk] =>
            if (eventuallyCondition) {
              serviceResponse.future
            } else {
              eventuallyCondition = true
              HttpResponse[String](HttpStatus(HttpStatusCodes.NotFound)).future
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
