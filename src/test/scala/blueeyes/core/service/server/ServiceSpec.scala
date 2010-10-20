package blueeyes.core.service.server

import org.specs.Specification
import blueeyes.core.service.RestPathPatternImplicits._
import blueeyes.core.service.{RestHierarchy, RestHierarchyBuilder}

class ServiceSpec extends Specification with BlueEyesServiceSpecification[String] {
  var service = new SpecTestService()

//  "MyScalatraServlet when using GET" should {
//    "/path/to/something should return 'hi!'" in {
//      path("/") {
//        println("CALLED 1")
//        get{
//          println("CALLED")
////          status mustEqual (200)
////
////          body mustEqual ("hi!")
//          2 must be (1)
//          null
//        }
//      }
//    }
//  }
}

class SpecTestService extends RestHierarchyBuilder[String] {
  path("bar/'adId/adCode.html") {get(new Handler())}
}

trait BlueEyesServiceSpecification[T] extends RestHierarchyBuilder[T] {
  def service: RestHierarchy[T]
}