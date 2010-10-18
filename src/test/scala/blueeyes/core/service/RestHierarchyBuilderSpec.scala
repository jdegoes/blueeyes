package blueeyes.core.service

import org.specs.Specification
import RestPathPatternImplicits._
import org.scalatest.mock.MockitoSugar
import blueeyes.util.Future

class RestHierarchyBuilderSpec extends Specification with MockitoSugar {
  private val handler       = mock[(Map[Symbol, String], HttpRequest[Any]) => Future[HttpResponse[Any]]]
  private val service       = new TestService
  private val netsedService = new TestNestedService

  "add service to the specified path" in {
    val serviceByPath = service.hierarchy.head

    serviceByPath._1.elementPatterns mustEqual("ads" / 'adId / "adCode.html" elementPatterns)
  }
  "associate handler with specified path" in {
    val serviceByPath = service.hierarchy.head

    serviceByPath._3 mustEqual(handler)
  }
  "add service to the specified nested path" in {

    val serviceByPath = netsedService.hierarchy.head

    serviceByPath._1.elementPatterns mustEqual("blue" / "eyes" / "api" / "v1" / "ads" / 'adId / "adCode.html" elementPatterns)
  }

  "associate handler with specified nested path" in {
    val serviceByPath = netsedService.hierarchy.head

    serviceByPath._3 mustEqual(handler)
  }

  class TestService extends RestHierarchyBuilder[Any]{
    path("ads" / 'adId / "adCode.html"){get(handler)}
  }
  class TestNestedService extends RestHierarchyBuilder[Any]{
    path("blue" / "eyes") {path("api" / "v1"){ path("ads" / 'adId / "adCode.html") {get(handler)}}}
  }
}

