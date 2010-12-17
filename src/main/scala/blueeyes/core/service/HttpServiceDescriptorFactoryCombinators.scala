package blueeyes.core.service

import blueeyes.health.HealthMonitor
import net.lag.logging.Logger
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.util.FutureImplicits
import blueeyes.json.JsonAST.JValue
import blueeyes.core.data.Bijection

trait HttpServiceDescriptorFactoryCombinators extends HttpRequestHandlerCombinators with RestPathPatternImplicits with FutureImplicits{
  def healthMonitor[T, S](f: HealthMonitor => HttpServiceDescriptorFactory[T, S])(implicit jValueBijection: Bijection[JValue, T]): HttpServiceDescriptorFactory[T, S] = {
    (context: HttpServiceContext) => {
      val monitor = new HealthMonitor()

      f(monitor)(context) ~ path("/blueeyes/services/" + context.serviceName + "/v" + context.serviceVersion.majorVersion + "/health") {
        get {
          request: HttpRequest[T] => HttpResponse[T](content=Some(jValueBijection(monitor.toJValue)))
        }
      }
    }
  }

  def logging[T, S](f: Logger => HttpServiceDescriptorFactory[T, S]): HttpServiceDescriptorFactory[T, S] = {
    (context: HttpServiceContext) => {
      val logger = Logger.configure(context.config.configMap("log"), false, false)

      f(logger)(context)
    }
  }
}