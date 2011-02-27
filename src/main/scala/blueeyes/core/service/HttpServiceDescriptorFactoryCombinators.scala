package blueeyes.core.service

import blueeyes.health.HealthMonitor
import net.lag.logging.Logger
import net.lag.configgy.Configgy
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.json.JsonAST._
import blueeyes.core.data.Bijection
import blueeyes.util.{Future, FutureImplicits}
import blueeyes.json.{JPathField, JPath, JPathImplicits}

trait HttpServiceDescriptorFactoryCombinators extends HttpRequestHandlerCombinators with RestPathPatternImplicits with FutureImplicits{
  private[this] object TransformerCombinators extends HttpClientTransformerCombinators
  import TransformerCombinators.{path$}

  /** Augments the service with health monitoring. By default, various metrics 
   * relating to request type, request timing, and request fulfillment are
   * exported to the health monitor.
   *
   * {{{
   * healthMonitor { monitor =>
   *   request { state =>
   *     ...
   *   }
   * }
   * }}}
   */  
  def healthMonitor[T, S](f: HealthMonitor => HttpServiceDescriptorFactory[T, S])(implicit jValueBijection: Bijection[JValue, T]): HttpServiceDescriptorFactory[T, S] = {
    (context: HttpServiceContext) => {
      val monitor = new HealthMonitor()

      val underlying = f(monitor)(context)
      val descriptor = underlying.copy(request = (state: S) => {new MonitorHttpRequestHandler(underlying.request(state), monitor)})

      descriptor ~> path("/blueeyes/services/" + context.serviceName + "/v" + context.serviceVersion.majorVersion + "/health") {
        get {
          request: HttpRequest[T] => {
            val version = context.serviceVersion
            val who     = JObject(JField("service", JObject(JField("name", JString(context.serviceName)) :: JField("version", JString("%d.%d.%s".format(version.majorVersion, version.minorVersion, version.version))) :: Nil)) :: Nil)
            val health  = monitor.toJValue
            HttpResponse[T](content=Some(jValueBijection(health.merge(who))))
          }
        }
      }
    }
  }

  /** Augments the service with logging.
   *
   * {{{
   * logging { logger =>
   *   request { state =>
   *     ...
   *   }
   * }
   * }}}
   */
  def logging[T, S](f: Logger => HttpServiceDescriptorFactory[T, S]): HttpServiceDescriptorFactory[T, S] = {
    (context: HttpServiceContext) => {
      val logger = Logger.configure(context.config.configMap("log"), false, false)

      f(logger)(context)
    }
  }
  
  /** Augments the service with a configurable root path. If this combinator 
   * is used, the config for the service may contain a "rootPath" setting,
   * which is used as the root path for the service.
   * 
   * {{{
   * configurableRoot {
   *   request { state =>
   *     ...
   *   }
   * }
   * }}}
   */
  def configurableRoot[T, S](f: HttpServiceDescriptorFactory[T, S]): HttpServiceDescriptorFactory[T, S] = {
    (context: HttpServiceContext) => {
      val underlying = f(context)
      
      underlying.copy(
        request = (state: S) => {
          val handler = underlying.request(state)
          
          context.config.getString("rootPath") match {
            case None => handler
            
            case Some(rootPath) => path(rootPath) { handler }
          }
        }
      )
    }
  }
  
  type ServiceLocator[T] = (String, HttpServiceVersion) => ServiceInvoker[T]
  
  case class ServiceInvoker[T](serviceRootUrl: String)(implicit client: HttpClient[T]) {
    def apply[X](transformer: HttpClientTransformer[T, X]): Future[X] = {
      client {
        path$(serviceRootUrl) { transformer }
      }
    }
  }
  
  /**
   * Augments the service with a locator, which is capable of creating HTTP
   * clients that connect to other BlueEyes services based on settings
   * in the config file.
   * 
   * To locate foo/v1, the locator will look at the config setting:
   * services.foo.v1.serviceRootUrl
   * {{{
   * serviceLocator { locator =>
   *   ...
   *   val content = locator("email", "1.01") { 
   *     get$ { response =>
   *       response.content.get
   *     }
   *   }
   * }
   * }}}
   */
  def serviceLocator[T, S](f: ServiceLocator[T] => HttpServiceDescriptorFactory[T, S])(implicit client: HttpClient[T]): HttpServiceDescriptorFactory[T, S] = {
    implicit def hack[X1, X2](f: Future[X1]): Future[X2] = f.asInstanceOf[Future[X2]]
    
    (context: HttpServiceContext) => {
      f {
        (serviceName: String, serviceVersion: HttpServiceVersion) => {
          val serviceRootUrl = Configgy.config("services." + serviceName + ".v" + serviceVersion.majorVersion.toString + ".serviceRootUrl")
          
          ServiceInvoker(serviceRootUrl)
        }
      } (context)
    }
  }

  private[service] class MonitorHttpRequestHandler[T](underlying: HttpRequestHandler[T], healthMonitor: HealthMonitor) extends HttpRequestHandler[T] with JPathImplicits{
    def isDefinedAt(request: HttpRequest[T]) = underlying.isDefinedAt(request)

    def apply(request: HttpRequest[T]) = {

      val methodName  = request.method.value
      val requestPath = List(JPathField("requests"), JPathField(methodName))
      val countPath   = JPath(requestPath ::: List(JPathField("count")))
      val timePath    = JPath(requestPath ::: List(JPathField("timing")))
      val errorPath   = JPath(requestPath ::: List(JPathField("errors")))
      val startTime  = System.nanoTime

      def monitor(response: Future[HttpResponse[T]]) = {
        val methodName = request.method.value

        healthMonitor.count(countPath)
        healthMonitor.trapFuture(errorPath)(response)

        response.deliverTo{v =>
          healthMonitor.trackTime(timePath)(System.nanoTime - startTime)                      
          healthMonitor.count(JPath(List(JPathField("requests"), JPathField("statusCodes"), JPathField(v.status.code.value.toString))))
        }

        response
      }

      monitor(healthMonitor.trap(errorPath){underlying.apply(request)})
    }
  }
}