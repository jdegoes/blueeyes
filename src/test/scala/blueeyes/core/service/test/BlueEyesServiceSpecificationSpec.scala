package blueeyes.core.service.test

import org.specs.Specification
import blueeyes.core.service.RestPathPatternImplicits._
import blueeyes.core.service._
import blueeyes.util.Future

class BlueEyesServiceSpecificationSpec extends Specification with BlueEyesServiceSpecification[String]{
  val serviceResponse = HttpResponse[String](HttpStatus(HttpStatusCodes.OK), Map("Content-Type" -> "text/html"), Some("context"), HttpVersions.`HTTP/1.1`)

  "calls test function" in {
    var executed = false
    path("/bar/'id/bar.html"){
      get{
        executed = true
      }
    }
    executed mustEqual (true)
  }
  "gets responce" in {
    path("/bar/'id/bar.html"){
      get{
        response mustEqual (serviceResponse)
      }
    }
  }
  "gets responce when future is set asynchronously" in {
    path("/asynch/future"){
      get{
        response mustEqual (serviceResponse)
      }
    }
  }

  val service = new Service()

  class Service extends RestHierarchyBuilder[String] {
    path("/bar/'foo/bar.html") {get(new Handler())}
    path("/asynch/future") {get(new AsynchHandler())}
  }
  class Handler extends Function1[HttpRequest[String], Future[HttpResponse[String]]]{
    def apply(request: HttpRequest[String]) = new Future[HttpResponse[String]]().deliver(serviceResponse)
  }
  class AsynchHandler extends Function1[HttpRequest[String], Future[HttpResponse[String]]]{
    def apply(request: HttpRequest[String]) = {
      import scala.actors.Actor._

      val future = new Future[HttpResponse[String]]()
      val actor2 = actor {
        Thread.sleep(1000)
        future.deliver(serviceResponse)
      }
      future
    }
  }  
}

