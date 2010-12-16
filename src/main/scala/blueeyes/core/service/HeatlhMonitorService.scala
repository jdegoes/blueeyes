package blueeyes.core.service

import blueeyes.json.JsonAST._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.BlueEyesServiceBuilder

/*class HeatlhMonitorService(monitors: List[HealthMonitorImpl]) extends BlueEyesServiceBuilder with HealthMonitorsImplicits{
  val healthService = service("health", "1.0.0") {
    context => {
      request {
        path("/blueeyes/health") {
          produce(application/json) {
            get {
              request: HttpRequest[Array[Byte]] => HttpResponse[JValue](content=Some(monitors.toJValue))
            }
          }
        }
      }
    }
  }
}*/