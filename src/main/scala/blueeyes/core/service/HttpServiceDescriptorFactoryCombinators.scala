package blueeyes.core.service

import net.lag.logging.Logger
import blueeyes.json.JsonAST._
import blueeyes.json.{JPathField, JPath, JPathImplicits}
import blueeyes.parsers.W3ExtendedLogAST.FieldsDirective
import blueeyes.parsers.W3ExtendedLog
import blueeyes.concurrent._
import blueeyes.core.data._
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.core.http.MimeTypes._
import blueeyes.health.metrics._
import blueeyes.health.{HealthMonitor, CompositeHealthMonitor}
import blueeyes.util._
import blueeyes.util.logging._
import net.lag.configgy.{Config, Configgy}
import java.util.Calendar
import printer.HtmlPrinter
import util.matching.Regex
import IntervalLength._

import scalaz.Scalaz._
import scalaz.{Failure, Success, Validation}
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Actor._
import blueeyes.core.service._

trait ServiceDescriptorFactoryCombinators extends HttpRequestHandlerCombinators with RestPathPatternImplicits with FutureImplicits with blueeyes.json.Implicits{
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
  def healthMonitor[T, S](f: HealthMonitor => ServiceDescriptorFactory[T, S])(implicit jValueBijection: Bijection[JValue, T]): ServiceDescriptorFactory[T, S] = healthMonitor(interval(1.minutes, 1), interval(5.minutes, 1), interval(10.minutes, 1))(f)
  def healthMonitor[T, S](default: IntervalConfig*)(f: HealthMonitor => ServiceDescriptorFactory[T, S])(implicit jValueBijection: Bijection[JValue, T]): ServiceDescriptorFactory[T, S] = {
    (context: ServiceContext) => {

      val intervals = context.config.getConfigMap("healthMonitor").map(_.getList("intervals").map(IntervalParser.parse(_))).getOrElse(default).toList
      val monitor   = new CompositeHealthMonitor(intervals match {
        case x :: xs => intervals
        case Nil => default.toList
      })

      val underlying = f(monitor)(context)
      val descriptor = underlying.copy(request = (state: S) => new MonitorHttpRequestService(underlying.request(state), monitor), shutdown = (state: S) => {
        underlying.shutdown(state).map(_ => monitor.shutdown())
      })
      val startTime = System.currentTimeMillis

      descriptor ~> describe("""Exports real-time metrics on health status, for use in continuous deployment. The default health monitor automatically exports information on number of requests, number and type of errors, and length of requests""")
        { path("/blueeyes/services/" + context.serviceName + "/v" + context.serviceVersion.majorVersion + "/health") {
          get {
            request: HttpRequest[T] => {
              val version       = context.serviceVersion
              val who           = JObject(JField("service", JObject(JField("name", JString(context.serviceName)) :: JField("version", JString("%d.%d.%s".format(version.majorVersion, version.minorVersion, version.version))) :: Nil)) :: Nil)
              val server        = JObject(JField("server", JObject(JField("hostName", JString(context.hostName)) :: JField("port", context.port) :: JField("sslPort", context.sslPort) :: Nil)) :: Nil)
              val uptimeSeconds = JObject(JField("uptimeSeconds", JInt((System.currentTimeMillis - startTime) / 1000)) :: Nil)
              val health        = monitor.toJValue.map(value => JObject(JField("requests", value) :: Nil))

              health map {health => HttpResponse[T](content=Some(jValueBijection(health.merge(who).merge(server).merge(uptimeSeconds))))}
            }
          }
        }
      }
    }
  }

  /** Augments the service with help service.
   *
   * {{{
   * help {
   *   request { state =>
   *     ...
   *   }
   * }
   * }}}
   */
  def help[T, S](f: => ServiceDescriptorFactory[T, S])(implicit stringBijection: Bijection[String, T]): ServiceDescriptorFactory[T, S] = {
    (context: ServiceContext) => {
      val underlying = f(context)
      underlying.copy(request = (state: S) => {
        val service = underlying.request(state)
        service ~ path("/blueeyes/services/" + context.serviceName + "/v" + context.serviceVersion.majorVersion + "/docs/api") {
        get {
          produce(text/html){
            HttpHandlerService{
              request: HttpRequest[T] => {
                Future.sync(HttpResponse[String](content = Some(ServiceDocumenter.printFormatted(context, service)(Metadata.StringFormatter, HtmlPrinter))))
              }
            }
          }
        }
      }})
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
  def logging[T, S](f: Logger => ServiceDescriptorFactory[T, S]): ServiceDescriptorFactory[T, S] = {
    (context: ServiceContext) => {
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
  def requestLogging[T, S](f: => ServiceDescriptorFactory[T, S])(implicit contentBijection: Bijection[T, ByteChunk]): ServiceDescriptorFactory[T, S] = {
    import RollPolicies._
    (context: ServiceContext) => {
      val underlying = f(context)

      val configMap = context.config.getConfigMap("requestLog").getOrElse(new Config())
      val enabled   = configMap.getBool("enabled", false)

      if (enabled){
        def fieldsDirective: FieldsDirective = {
          val configValue = configMap.getString("fields", "time cs-method cs-uri sc-status")
          W3ExtendedLog("#Fields: " + configValue) match {
            case (e: FieldsDirective) :: Nil => e
            case _ => sys.error("Log directives are not specified.")
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
          case x            => sys.error("Unknown logfile rolling policy: " + x)
        }

        val fileName = configMap.getString("file", context.toString + "-request.log")

        val writeDelaySeconds = configMap.getInt("writeDelaySeconds", 10)
        val includePaths      = configMap.getList("includePaths").map(new Regex(_)).toList
        val excludePaths      = configMap.getList("excludePaths").map(new Regex(_)).toList
        val formatter         = HttpRequestLoggerFormatter(configMap.getString("formatter", "w3c"))

        def fileHeader() = formatter.formatHeader(fieldsDirective)
        val log          = RequestLogger.get(fileName, policy, fileHeader _, writeDelaySeconds)
        val actor        = actorOf(new HttpRequestLoggerActor[T](fieldsDirective, includePaths, excludePaths, log, formatter)).start()

        underlying.copy(request = (state: S) => {new HttpRequestLoggerService(actor, underlying.request(state))},
                        shutdown = (state: S) => {
                          underlying.shutdown(state).flatMap{_ =>
                          actor.stop()
                            log.close
                          }
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
  def configurableRoot[T, S](f: ServiceDescriptorFactory[T, S]): ServiceDescriptorFactory[T, S] = {
    (context: ServiceContext) => {
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

  type ServiceLocator[T] = (String, ServiceVersion) => HttpClient[T]

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
  def serviceLocator[T, S](f: ServiceLocator[T] => ServiceDescriptorFactory[T, S])(implicit httpClient: HttpClient[T]): ServiceDescriptorFactory[T, S] = {
    implicit def hack[X1, X2](f: Future[X1]): Future[X2] = f.asInstanceOf[Future[X2]]

    (context: ServiceContext) => {
      f {
        (serviceName: String, serviceVersion: ServiceVersion) => {
          val serviceRootUrl = Configgy.config("services." + serviceName + ".v" + serviceVersion.majorVersion.toString + ".serviceRootUrl")

          httpClient.path(serviceRootUrl)
        }
      } (context)
    }
  }

  private[service] class HttpRequestLoggerService[T](actor: ActorRef, underlying: AsyncHttpService[T]) extends AsyncCustomHttpService[T]{
    def service = (request: HttpRequest[T]) => {
      try {
        val validation = underlying.service(request)
        for (response <- validation) actor ! ((request, response))
        validation
      } catch {
        case ex: Throwable => 
          actor ! ((request, Future.dead[HttpResponse[T]]))
          throw ex
      }
    }

    val metadata = None
  }

  private[service] class HttpRequestLoggerActor[T](fieldsDirective: FieldsDirective, includePaths: List[Regex], excludePaths: List[Regex], log: RequestLogger, formatter: HttpRequestLoggerFormatter)(implicit contentBijection: Bijection[T, ByteChunk]) extends Actor with ClockSystem{
    private val includeExcludeLogic = new IncludeExcludeLogic(includePaths, excludePaths)
    private val requestLogger       = HttpRequestLogger[T, T](fieldsDirective)

    def receive = {
      case (request: HttpRequest[T], response: Future[HttpResponse[T]]) => {
        if (includeExcludeLogic(request.subpath)){
          val logRecord = requestLogger(request, response)
          logRecord.map(formatter.formatLog(_)) foreach { log(_) }
        }
      }
    }

    val metadata = None
  }

  private[service] class MonitorHttpRequestService[T](val delegate: AsyncHttpService[T], healthMonitor: HealthMonitor) extends DelegatingService[T, Future[HttpResponse[T]], T, Future[HttpResponse[T]]] with JPathImplicits{
    def service = {request: HttpRequest[T] =>
      val methodName    = request.method.value
      val requestPath   = JPathField(methodName)
      val countPath     = JPath(requestPath :: List(JPathField("count")))
      val timePath      = JPath(requestPath :: List(JPathField("timing")))
      val errorPath     = JPath(requestPath :: List(JPathField("errors")))
      val overagePath   = JPath(requestPath :: List(JPathField("timing")))
      val startTime     = System.nanoTime

      def monitor(validation: Validation[NotServed, Future[HttpResponse[T]]]) = {
        validation match{
          case Success(response) =>
            healthMonitor.call(overagePath)
            healthMonitor.count(countPath)
            healthMonitor.trapFuture(errorPath)(response)

            response.deliverTo{v =>
              healthMonitor.trackTime(timePath)(System.nanoTime - startTime)
              healthMonitor.count(JPath(List(JPathField("statusCodes"), JPathField(v.status.code.value.toString))))
            }
          case Failure(DispatchError(error)) =>
            healthMonitor.call(overagePath)
            healthMonitor.count(countPath)
            healthMonitor.error(errorPath)(error)
          case failure =>
        }
        validation
      }

      monitor(healthMonitor.trap(errorPath){delegate.service(request)})
    }

    val metadata = None
  }
}

private[service] class IncludeExcludeLogic(includePaths: List[Regex], excludePaths: List[Regex]){
  def apply(path: String) = (includePaths, excludePaths) match {
    case (Nil, Nil) => true
    case _ => includePaths match {
      case x :: xs => includePaths.foldLeft(false){(result, regex) =>
        {
          val pattern = regex.pattern
          val matcher = pattern.matcher(path)
          val matches = matcher.matches
          result || matches
        }
      }
      case Nil => !excludePaths.foldLeft(false){(result, regex) => result || regex.pattern.matcher(path).matches}
    }
  }
}
