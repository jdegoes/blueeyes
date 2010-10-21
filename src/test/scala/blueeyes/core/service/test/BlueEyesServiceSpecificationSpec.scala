package blueeyes.core.service.test

import org.specs.Specification
import blueeyes.core.service.RestPathPatternImplicits._
import blueeyes.core.service._
import blueeyes.util.Future

class BlueEyesServiceSpecificationSpec extends Specification {
  private val specification = new TestBlueEyesServiceSpecification()

  "calls test function" in {
    var executed = false
    specification.path("/bar/'id/bar.html"){
      specification.get{
        executed = true
      }
    }
    executed mustEqual (true)
  }
  "gets responce" in {
    specification.path("/bar/'id/bar.html"){
      specification.get{
        specification.response mustEqual (specification.serviceResponse)         
      }
    }
  }
  "gets responce when future is set asynchronously" in {
    specification.path("/asynch/future"){
      specification.get{
        specification.response mustEqual (specification.serviceResponse)
      }
    }
  }
}

class TestBlueEyesServiceSpecification extends BlueEyesServiceSpecification[String]{
  val service = new Service()
  val serviceResponse = HttpResponse[String](HttpStatus(HttpStatusCodes.OK), Map("Content-Type" -> "text/html"), Some("context"), HttpVersions.`HTTP/1.1`)

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


