package blueeyes.core.service

import blueeyes.core.http.HttpStatusCodes._
import test.BlueEyesServiceSpecification
import blueeyes.BlueEyesServiceBuilderString
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.json.JsonParser.{parse => j}
import blueeyes.json.JsonAST.{JInt, JNothing}

class HttpServiceDescriptorFactoryCombinatorsSpec extends BlueEyesServiceSpecification[String] with HeatlhMonitorService{
  "HttpServiceDescriptorFactoryCombinators: adds health monitor service" in{
    path("/foo"){
      get{
        responseStatus  mustEqual(HttpStatus(OK))
        responseContent mustEqual(None)
        ()
      }
    }

    path("/blueeyes/services/email/v1/health"){
      get{
        responseStatus  mustEqual(HttpStatus(OK))

        val response  = j(responseContent.get)

        response \ "requests" \ "GET" \ "count" mustEqual(JInt(1))
        response \ "requests" \ "GET" \ "timing" mustNotEq(JNothing)
        ()
      }
    }
  }
}

trait HeatlhMonitorService extends BlueEyesServiceBuilderString with HttpServiceDescriptorFactoryCombinators{
  val emailService = service ("email", "1.01") {
  import blueeyes.health.HealthMonitor

    logging {
        log =>
          healthMonitor {
            monitor =>
              context => {
                request {
                  path("/foo") {
                    get {request: HttpRequest[String] => HttpResponse[String]()}
                  }
                }
              }
          }
    }
  }
}