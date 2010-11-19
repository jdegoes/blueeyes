package blueeyes.core.service.test

import org.specs.Specification
import blueeyes.core.service.RestPathPatternImplicits._
import blueeyes.core.service._
import blueeyes.util.Future
import blueeyes.core.http.MimeTypes._
import blueeyes.BlueEyesServiceBuilderString
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http._

class BlueEyesServiceSpecificationSpec extends Specification with BlueEyesServiceSpecification[String] {
  val serviceResponse = HttpResponse[String](HttpStatus(HttpStatusCodes.OK), Map("Content-Type" -> "text/html"), Some("context"), HttpVersions.`HTTP/1.1`)

  val service = new SampeService().sampleService
  def config = ""  

  shareVariables()

  "SampeService" should{
    doFirst{start(60000)}

  "calls test function" in {
    var executed = false
    path("/bar/id/bar.html"){
      get{
        executed = true
      }
    }
    executed mustEqual (true)
  }
  "gets responce" in {
    path("/bar/id/bar.html"){
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
    doLast{stop(60000)}
  }

  class SampeService extends BlueEyesServiceBuilderString {
    val sampleService = service("sample", "1.32") { context =>
      request { 
        contentType(text/html) {
          path("/bar/'foo/bar.html") {
            get { request: HttpRequest[String] =>
              serviceResponse
            }
          } ~
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
}
