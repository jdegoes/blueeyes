package blueeyes.core.service

import blueeyes.core.http.HttpStatusCodes._
import test.BlueEyesServiceSpecification
import blueeyes.BlueEyesServiceBuilderString
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus, HttpStatusCodes}

class HttpServiceDescriptorFactoryCombinatorsSpec extends BlueEyesServiceSpecification[String] with HeatlhMonitorService{
  "HttpServiceDescriptorFactoryCombinators: adds health monitor service" in{
    path("/blueeyes/services/email/v1/health"){
      get{
        responseStatus  mustEqual(HttpStatus(OK))
        responseContent mustEqual(Some("{}"))
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