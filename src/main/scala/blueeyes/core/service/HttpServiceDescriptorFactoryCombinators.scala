package blueeyes.core.service

import blueeyes.health.HealthMonitor
import net.lag.logging.Logger
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.json.JsonAST._
import blueeyes.core.data.Bijection
import blueeyes.json.{JPathField, JPath, JPathImplicits}
import blueeyes.parsers.W3ExtendedLogAST.FieldsDirective
import net.lag.configgy.{Config, ConfigMap, Configgy}
import blueeyes.parsers.W3ExtendedLog
import blueeyes.concurrent._
import blueeyes.util._
import blueeyes.util.logging._
import java.util.Calendar

trait HttpServiceDescriptorFactoryCombinators extends HttpRequestHandlerCombinators with RestPathPatternImplicits with FutureImplicits with blueeyes.json.Implicits{
//  private[this] object TransformerCombinators
//  import TransformerCombinators.{path$}

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
      val startTime = System.currentTimeMillis

      descriptor ~> path("/blueeyes/services/" + context.serviceName + "/v" + context.serviceVersion.majorVersion + "/health") {
        get {
          request: HttpRequest[T] => {
            val version       = context.serviceVersion
            val who           = JObject(JField("service", JObject(JField("name", JString(context.serviceName)) :: JField("version", JString("%d.%d.%s".format(version.majorVersion, version.minorVersion, version.version))) :: Nil)) :: Nil)
            val server        = JObject(JField("server", JObject(JField("hostName", JString(context.hostName)) :: JField("port", context.port) :: JField("sslPort", context.sslPort) :: Nil)) :: Nil)
            val uptimeSeconds = JObject(JField("uptimeSeconds", JInt((System.currentTimeMillis - startTime) / 1000)) :: Nil)
            val health        = monitor.toJValue
            HttpResponse[T](content=Some(jValueBijection(health.merge(who).merge(server).merge(uptimeSeconds))))
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
      val logger = LoggingHelper.initializeLogging(context.config, context.toString)

      f(logger)(context)
    }
  }

  /** Augments the service with request/response logging.
   *
   * {{{
   * requestLogging {
   *   request { state =>
   *     ...
   *   }
   * }
   * }}}
   */
  def requestLogging[T, S](f: => HttpServiceDescriptorFactory[T, S]): HttpServiceDescriptorFactory[T, S] = {
    import RollPolicies._
    (context: HttpServiceContext) => {
      val underlying = f(context)

      val configMap = context.config.getConfigMap("requestLog").getOrElse(new Config())
      val enabled   = configMap.getBool("enabled", true)

      if (enabled){
        def fieldsDirective: FieldsDirective = {
          val configValue = configMap.getString("fields", "time cs-method cs-uri sc-status")
          W3ExtendedLog("#Fields: " + configValue) match {
            case (e: FieldsDirective) :: Nil => e
            case _ => error("Log directives are not specified.")
          }
        }

        val policy = configMap.getString("roll", "never").toLowerCase match {
          case "never"      => Never
          case "hourly"     => Hourly
          case "daily"      => Daily
          case "sunday"     => Weekly(Calendar.SUNDAY)
          case "monday"     => Weekly(Calendar.MONDAY)
          case "tuesday"    => Weekly(Calendar.TUESDAY)
          case "wednesday"  => Weekly(Calendar.WEDNESDAY)
          case "thursday"   => Weekly(Calendar.THURSDAY)
          case "friday"     => Weekly(Calendar.FRIDAY)
          case "saturday"   => Weekly(Calendar.SATURDAY)
          case x            => error("Unknown logfile rolling policy: " + x)
        }

        val fileName = configMap.getString("file", context.toString + "-request.log")

        val writeDelaySeconds = configMap.getInt("writeDelaySeconds", 1)

        val log = W3ExtendedLogger.get(fileName, policy, fieldsDirective, writeDelaySeconds)

        underlying.copy(request = (state: S) => {new HttpRequestLoggerHandler(fieldsDirective, log, underlying.request(state))},
                        shutdown = (state: S) => {
                          log.close.flatMap{(v: Unit) => underlying.shutdown(state)}
                        })
      }
      else underlying
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

  type ServiceLocator[T] = (String, HttpServiceVersion) => HttpClient[T]

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
   *   val email = locator("email", "1.01")
   * }
   * }}}
   */
  def serviceLocator[T, S](f: ServiceLocator[T] => HttpServiceDescriptorFactory[T, S])(implicit httpClient: HttpClient[T]): HttpServiceDescriptorFactory[T, S] = {
    implicit def hack[X1, X2](f: Future[X1]): Future[X2] = f.asInstanceOf[Future[X2]]

    (context: HttpServiceContext) => {
      f {
        (serviceName: String, serviceVersion: HttpServiceVersion) => {
          val serviceRootUrl = Configgy.config("services." + serviceName + ".v" + serviceVersion.majorVersion.toString + ".serviceRootUrl")

          httpClient.path(serviceRootUrl)
        }
      } (context)
    }
  }

  private[service] class HttpRequestLoggerHandler[T](fieldsDirective: FieldsDirective, log: W3ExtendedLogger, underlying: HttpRequestHandler[T]) extends HttpRequestHandler[T] with ClockSystem{
    private val requestLogger = HttpRequestLogger[T, T](fieldsDirective)
    def isDefinedAt(request: HttpRequest[T]) = underlying.isDefinedAt(request)

    def apply(request: HttpRequest[T]) = {
      val response = underlying(request)

      val logRecord = requestLogger(request, response)

      logRecord foreach { log(_)}

      response
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
