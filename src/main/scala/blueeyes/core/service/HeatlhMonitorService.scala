package blueeyes.core.service

import blueeyes.json.JsonAST._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.BlueEyesServiceBuilder

class HeatlhMonitorService(monitors: List[HealthMonitorImpl]) extends BlueEyesServiceBuilder{
  val healthService = service("health", "1.0.0") {
    context => {
      request {
        path("/blueeyes/health") {
          produce(application/json) {
            get {
              request: HttpRequest[Array[Byte]] => HttpResponse[JValue](content=Some(servicesJObject))
            }
          }
        }
      }
    }
  }

  private def servicesJObject = {
    val services = monitors.foldLeft(JObject(Nil)){(result, element) => result.merge(element.toJValue).asInstanceOf[JObject]}
    JObject(JField("services", services) :: Nil)
  }
}