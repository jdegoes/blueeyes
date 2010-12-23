package blueeyes.core.service

import blueeyes.json.JPathImplicits
import blueeyes.health.HealthMonitor
import net.lag.logging.Logger
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.json.JsonAST.JValue
import blueeyes.core.data.Bijection
import blueeyes.util.{Future, FutureImplicits}

trait HttpServiceDescriptorFactoryCombinators extends HttpRequestHandlerCombinators with RestPathPatternImplicits with FutureImplicits{
  def healthMonitor[T, S](f: HealthMonitor => HttpServiceDescriptorFactory[T, S])(implicit jValueBijection: Bijection[JValue, T]): HttpServiceDescriptorFactory[T, S] = {
    (context: HttpServiceContext) => {
      val monitor = new HealthMonitor()

      val underlying = f(monitor)(context)
      val descriptor = underlying.copy(request = (state: S) => {new MonitorHttpRequestHandler(underlying.request(state), monitor)})
      
      descriptor ~ path("/blueeyes/services/" + context.serviceName + "/v" + context.serviceVersion.majorVersion + "/health") {
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

  private[service] class MonitorHttpRequestHandler[T](underlying: HttpRequestHandler[T], healthMonitor: HealthMonitor) extends HttpRequestHandler[T] with JPathImplicits{
    def isDefinedAt(request: HttpRequest[T]) = underlying.isDefinedAt(request)

    def apply(request: HttpRequest[T]) = {

      val methodName = request.method.value
      val countPath  = ".requests.%s.count".format(methodName)
      val timePath   = ".requests.%s.timing".format(methodName)
      val errorPath  = ".requests.%s.errors".format(methodName)

      def monitor(response: Future[HttpResponse[T]]) = {
        val methodName = request.method.value

        healthMonitor.count(countPath)
        healthMonitor.timeFuture(timePath)(response)
        healthMonitor.trapFuture(errorPath)(response)

        response
      }

      monitor(healthMonitor.trap(errorPath){underlying.apply(request)})      
    }
  }
}