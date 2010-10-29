package blueeyes.core.service

import org.specs.Specification
import RestPathPatternImplicits._
import org.scalatest.mock.MockitoSugar
import blueeyes.core.data.TextToTextBijection
import blueeyes.core.http.MimeTypes._
import blueeyes.util.Future

class RestHierarchyBuilderSpec extends Specification with MockitoSugar {
  private val handler       = mock[HttpRequest[String] => Future[HttpResponse[String]]]
  private val service       = new TestService
  private val netsedService = new TestNestedService
  private implicit val transcoder = new HttpStringDataTranscoder(TextToTextBijection, text / html)

  "add service to the specified path" in {
    val serviceByPath = service.hierarchy.head

    serviceByPath._1.elementPatterns mustEqual("ads" / 'adId / "adCode.html" elementPatterns)
  }
  "associate handler with specified path" in {
    val serviceByPath = service.hierarchy.head._3.asInstanceOf[HttpRequest[String] => Future[HttpResponse[String]]]

    serviceByPath mustEqual(handler)
  }
  "add service to the specified nested path" in {

    val serviceByPath = netsedService.hierarchy.head
    
    val list1 = serviceByPath._1.elementPatterns
    val list2 = "blue/eyes/api/v1/ads/'adId/adCode.html".elementPatterns

    list1 mustEqual(list2)
  }

  "associate handler with specified nested path" in {
    val serviceByPath = netsedService.hierarchy.head._3.asInstanceOf[HttpRequest[String] => Future[HttpResponse[String]]]

    serviceByPath mustEqual(handler)
  }

  class TestService extends RestHierarchyBuilder[String]{
    path("ads" / 'adId / "adCode.html") {
      get {
        handler
      }
    }
  }
  class TestNestedService extends RestHierarchyBuilder[String]{
    path("blue/eyes") {
      path("api/v1") {
        path("ads/'adId/adCode.html") {
          get{
            handler
          }
        }
      }
    }
  }
}

