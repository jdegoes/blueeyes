package blueeyes.core.service

import blueeyes.bkka._
import blueeyes.core.data._
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.core.http.MimeTypes._
import blueeyes.core.service._
import blueeyes.health.metrics._
import blueeyes.health.{HealthMonitor, CompositeHealthMonitor}
import blueeyes.json._
import blueeyes.logging._
import blueeyes.parsers.W3ExtendedLogAST.FieldsDirective
import blueeyes.parsers.W3ExtendedLog
import blueeyes.util._

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef
import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext
import akka.util.Timeout

import org.streum.configrity.Configuration
import com.weiglewilczek.slf4s.Logger
import com.weiglewilczek.slf4s.Logging

import java.util.Calendar
import printer.HtmlPrinter
import util.matching.Regex
import IntervalLength._

import scalaz._

trait ServiceDescriptorFactoryCombinators extends HttpRequestHandlerCombinators with RestPathPatternImplicits with Logging {
  def defaultHealthMonitorConfig = Seq(interval(1.minutes, 1), interval(5.minutes, 1), interval(10.minutes, 1))
  def defaultShutdownTimeout     = akka.util.Timeout(10000)

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
  def healthMonitor[T, S](f: HealthMonitor => ServiceDescriptorFactory[T, S])
                         (implicit jv2t: JValue => T, executor: ExecutionContext): ServiceDescriptorFactory[T, S] = healthMonitor("/health", defaultShutdownTimeout)(f)

  def healthMonitor[T, S](pathPrefix: RestPathPattern)
                         (f: HealthMonitor => ServiceDescriptorFactory[T, S])
                         (implicit jv2t: JValue => T, executor: ExecutionContext): ServiceDescriptorFactory[T, S] = healthMonitor(pathPrefix, defaultShutdownTimeout)(f)

  def healthMonitor[T, S](pathPrefix: RestPathPattern, shutdownTimeout: Timeout, config: Seq[IntervalConfig] = defaultHealthMonitorConfig)
                         (f: HealthMonitor => ServiceDescriptorFactory[T, S])
                         (implicit jv2t: JValue => T, executor: ExecutionContext): ServiceDescriptorFactory[T, S] = {
    (context: ServiceContext) => {
      implicit val stop: Stop[HealthMonitor] = HealthMonitor.stop(shutdownTimeout)

      val intervals = context.config.detach("healthMonitor").get[List[String]]("intervals").map(_.map(IntervalParser.parse(_))).getOrElse(config).toList
      val monitor: HealthMonitor = new CompositeHealthMonitor(intervals)

      val serviceLifecycle = f(monitor)(context)
      serviceLifecycle.copy(
        runningState = (state: S) => {
          val startTime = System.currentTimeMillis

          val (service, stoppable) = serviceLifecycle.runningState(state)
          val monitorStoppable = stoppable map { s => Stoppable(monitor, s :: Nil) } orElse { Some(Stoppable(monitor)) }

          val monitoredService = new MonitorHttpRequestService(service, monitor)

          val monitorService = path(pathPrefix) {
            get { new HealthMonitorService[T, T](context, monitor, startTime) }
          }

          (monitorService ~ monitoredService, monitorStoppable)
        }
      )
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
  def help[T, S](f: => ServiceDescriptorFactory[T, S])(implicit s2t: String => T, executor: ExecutionContext): ServiceDescriptorFactory[T, S] = help("/docs/api")(f)

  def help[T, S](pathPrefix: RestPathPattern)(f: => ServiceDescriptorFactory[T, S])(implicit s2t: String => T, executor: ExecutionContext): ServiceDescriptorFactory[T, S] = {
    implicit val F: scalaz.Functor[Future] = new blueeyes.bkka.FutureMonad(executor)
    import HttpRequestHandlerImplicits._
    (context: ServiceContext) => {
      val underlying = f(context)
      underlying.copy(
        runningState = (state: S) => {
          val (service, stoppable) = underlying.runningState(state)
          val helpService = {
            service ~ 
            path(pathPrefix) {
              get {
                produce(text/html){
                  (request: HttpRequest[T]) => {
                    Future(HttpResponse[String](content = Some(ServiceDocumenter.printFormatted(context, service)(Metadata.StringFormatter, HtmlPrinter))))
                  }
                }
              }
            }
          }

          (helpService, stoppable)
        }
      )
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
      val logger = Logger(context.toString)

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
  def requestLogging[T, S](f: => ServiceDescriptorFactory[T, S])(implicit t2c: Bijection[T, ByteChunk], executor: ExecutionContext): ServiceDescriptorFactory[T, S] = 
    requestLogging(defaultShutdownTimeout)(f)

  def requestLogging[T, S](shutdownTimeout: Timeout)(f: => ServiceDescriptorFactory[T, S])(implicit t2c: Bijection[T, ByteChunk], executor: ExecutionContext): ServiceDescriptorFactory[T, S] = {
    import RollPolicies._
    (context: ServiceContext) => {
      val underlying = f(context)

      val logConfig = context.config.detach("requestLog")
      val enabled   = logConfig[Boolean]("enabled", false)

      if (enabled){
        def fieldsDirective: FieldsDirective = {
          val configValue = logConfig[String]("fields", "time cs-method cs-uri sc-status")
          W3ExtendedLog("#Fields: " + configValue) match {
            case (e: FieldsDirective) :: Nil => e
            case _ => sys.error("Log directives are not specified.")
          }
        }

        val policy = logConfig[String]("roll", "never").toLowerCase match {
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

        val fileName = logConfig[String]("file", context.toString + "-request.log")

        val writeDelaySeconds = logConfig[Int]("writeDelaySeconds", 10)
        val includePaths      = logConfig.get[List[String]]("includePaths").getOrElse(List()).map(new Regex(_)).toList
        val excludePaths      = logConfig.get[List[String]]("excludePaths").getOrElse(List()).map(new Regex(_)).toList
        val formatter         = HttpRequestLoggerFormatter(logConfig[String]("formatter", "w3c"))

        def fileHeader() = formatter.formatHeader(fieldsDirective)
        underlying.copy(
          runningState = (state: S) => {
            val log = RequestLogger.get(fileName, policy, fileHeader _, writeDelaySeconds)

            implicit val logStop = new Stop[RequestLogger] {
              def stop(log: RequestLogger) = log.close(shutdownTimeout)
            }

            val actorSystem = ActorSystem("blueeyes-request-logger")
            val actor = actorSystem.actorOf(Props(new HttpRequestLoggerActor[T](fieldsDirective, includePaths, excludePaths, log, formatter)))
            
            implicit val actorStop = ActorRefStop(actorSystem, shutdownTimeout)

            val (service, stoppable) = underlying.runningState(state)
            val loggerService = new HttpRequestLoggerService(actor, service)
            val loggerStoppable = stoppable map { s =>
              Stoppable(actor, Stoppable(log, Stoppable(s) :: Nil) :: Nil)
            } orElse {
              Some(Stoppable(actor, Stoppable(log) :: Nil))
            }

            (loggerService, loggerStoppable)
          }
        )
      } else {
        underlying
      }
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
        runningState = (state: S) => {
          val (service, stoppable) = underlying.runningState(state)
          val newService = context.config.get[String]("rootPath") match {
            case None => service
            case Some(rootPath) => path(rootPath) { service }
          }
          (newService, stoppable)
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
          val serviceRootUrl = context.rootConfig[String]("services." + serviceName + ".v" + serviceVersion.majorVersion.toString + ".serviceRootUrl")

          httpClient.path(serviceRootUrl)
        }
      } (context)
    }
  }

  private[service] class HttpRequestLoggerService[T](actor: ActorRef, underlying: AsyncHttpService[T])(implicit executor: ExecutionContext) 
      extends CustomHttpService[T, Future[HttpResponse[T]]]{
    def service = (request: HttpRequest[T]) => {
      try {
        val validation = underlying.service(request)
        for (response <- validation) actor ! ((request, response))
        validation
      } catch {
        case ex: Throwable => 
          actor ! ((request, Promise.failed[HttpResponse[T]](ex)))
          throw ex
      }
    }

    val metadata = NoMetadata
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

    val metadata = NoMetadata
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

            response onSuccess {
              case v => 
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

    val metadata = NoMetadata
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
