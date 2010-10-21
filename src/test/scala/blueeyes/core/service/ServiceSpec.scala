package blueeyes.core.service

import org.specs.Specification
import blueeyes.core.service.RestPathPatternImplicits._
import collection.mutable.Stack
import test.BlueEyesServiceSpecification
import util.DynamicVariable
import blueeyes.util.Future
import HttpMethods._

class ServiceSpec extends Specification with BlueEyesServiceSpecification[String] {
  var service = new SpecTestService()

  "MyScalatraServlet when using GET" should {
    "/path/to/something should return 'hi!'" in {
      path("/bar") {
        path("/22/adCode.html") {
          get{
            status mustEqual (HttpStatus(HttpStatusCodes.OK))
          }
        }
      }
    }
  }
}

class SpecTestService extends RestHierarchyBuilder[String] {
  path("/bar/'adId/adCode.html") {get(new SpecHandler())}
}

class SpecHandler extends Function1[HttpRequest[String], Future[HttpResponse[String]]]{
  def apply(request: HttpRequest[String]) = new Future[HttpResponse[String]]().deliver(HttpResponse[String](HttpStatus(HttpStatusCodes.OK), Map("Content-Type" -> "text/html"), Some(Context.context), HttpVersions.`HTTP/1.1`))
}

