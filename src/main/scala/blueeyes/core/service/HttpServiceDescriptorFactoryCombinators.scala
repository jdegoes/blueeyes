package blueeyes.core.service

import blueeyes.json.JPathImplicits
import blueeyes.health.HealthMonitor
import net.lag.logging.Logger
import net.lag.configgy.Configgy
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
  
  private type T0[T, S, X] = ((String, HttpServiceVersion) => HttpClientTransformer[T, X] => Future[X]) => HttpServiceDescriptorFactory[T, S]
  
  /**
   * serviceLocator { locator =>
   *   ...
   *   val content = locator("email", "1.01") { 
   *     get$ { response =>
   *       response.content.get
   *     }
   *   }
   * }
   */
  def serviceLocator[T, S](f: T0[T, S, _])(implicit client: HttpClient[T]): HttpServiceDescriptorFactory[T, S] = {
    object TransformerCombinators extends HttpClientTransformerCombinators
    import TransformerCombinators.{path$}
    
    implicit def hack[X1, X2](f: Future[X1]): Future[X2] = f.asInstanceOf[Future[X2]]
    
    (context: HttpServiceContext) => {
      f {
        (name: String, version: HttpServiceVersion) => {
          val serviceRootUrl = Configgy.config("services." + context.serviceName + ".v" + context.serviceVersion.majorVersion.toString + ".serviceRootUrl")
          
          (transformer: HttpClientTransformer[T, _]) => {
            client.exec {
              path$(serviceRootUrl) {
                (client: HttpClient[T]) => {
                  transformer(client)
                }
              }
            }
          }
        }
      } (context)
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