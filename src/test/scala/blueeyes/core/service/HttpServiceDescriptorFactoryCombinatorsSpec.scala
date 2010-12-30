package blueeyes.core.service

import blueeyes.core.http.HttpStatusCodes._
import test.BlueEyesServiceSpecification
import blueeyes.BlueEyesServiceBuilderString
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.json.JsonParser.{parse => j}
import blueeyes.json.JsonAST.{JInt, JNothing}

class HttpServiceDescriptorFactoryCombinatorsSpec extends BlueEyesServiceSpecification[String] with HeatlhMonitorService{

  path$("/foo"){
    get${ response: HttpResponse[String] =>
      response.status  mustEqual(HttpStatus(OK))
      response.content mustEqual(None)
    }
  } should "adds health monitor service"

  path$("/blueeyes/services/email/v1/health"){
    get${ response: HttpResponse[String] =>
      response.status  mustEqual(HttpStatus(OK))

      val content  = j(response.content.get)

      content \ "requests" \ "GET" \ "count" mustEqual(JInt(1))
      content \ "requests" \ "GET" \ "timing" mustNotEq(JNothing)
    }
  } should "adds health monitor statistics"
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