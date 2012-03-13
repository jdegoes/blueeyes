package blueeyes.core.service

import blueeyes.bkka.Stoppable
import akka.dispatch.Future
import akka.dispatch.Promise
import akka.util.Timeout

import blueeyes.bkka.AkkaDefaults
import blueeyes.core.data.ByteChunk
import blueeyes.core.http._
import blueeyes.core.service._
import blueeyes.util.RichThrowableImplicits._
import blueeyes.util.CommandLineArguments

import java.lang.reflect.{Method}
import java.util.concurrent.CountDownLatch
import java.net.InetAddress

import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat

import com.weiglewilczek.slf4s.Logger
import scalaz.{Failure, Success}
import scalaz.Validation._
import scalaz.syntax.validation._

/** A trait that grabs services reflectively from the fields of the class it is
 * mixed into.
 */
trait HttpReflectiveServiceList[T] { self =>
  lazy val services: List[Service[T]] = {
    val c = self.getClass
    
    val allMethods: List[Method] = c.getDeclaredMethods.toList
    
    val serviceMethods: List[Method] = allMethods.reverse.filter { method =>
      classOf[Service[T]].isAssignableFrom(method.getReturnType) && method.getParameterTypes.length == 0
    }
    
    serviceMethods.map { method =>
      method.invoke(self).asInstanceOf[Service[T]]
    }
  }
}

/** An http server acts as a container for services. A server can be stopped
 * and started, and has a main function so it can be mixed into objects.
 */
trait HttpServer extends AsyncCustomHttpService[ByteChunk] with AkkaDefaults { self =>
  private lazy val NotFound            = HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.NotFound))
  private lazy val InternalServerError = HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.InternalServerError))

  /**
   * The default timeout to be used when stopping dependent services is forever. Override this value
   * to provide a different timeout.
   */
  val stopTimeout = akka.util.Timeout(Long.MaxValue)

  /** The root configuration. This is simply configritty's root config 
   * object, so this should not be used until the config has been loaded 
   */
  def rootConfig: Configuration = rootConfiguration 

  // I would like a better way to do this, but given the current relationship
  // to a locally defined main that bootstraps the server I didn't see a way
  // around the mutable var with a much more significant refactoring. NDM
  var rootConfiguration: Configuration = null
  
  /** The list of services that this server is supposed to run.
   */
  def services: List[Service[ByteChunk]]

  val service = {r: HttpRequest[ByteChunk] =>
    def convertErrorToResponse(th: Throwable): HttpResponse[ByteChunk] = th match {
      case e: HttpException => HttpResponse[ByteChunk](HttpStatus(e.failure, e.reason))
      case e => {
        log.error("Error handling request", e)
        HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.InternalServerError, Option(e.getMessage).getOrElse("")))
      }
    }
    
    // The raw future may die due to error:
    val rawValidation = try {
       _handler.service(r)
    } catch {
      // An error during invocation of the request handler, convert to
      // proper response:
      case error: Throwable => success(Promise.successful(convertErrorToResponse(error)))
    }

    // Convert the raw future into one that cannot die:
    rawValidation match {
      case Success(rawFuture) => success((rawFuture recover { case error => convertErrorToResponse(error) }))
      case Failure(DispatchError(throwable)) => success(Future(convertErrorToResponse(throwable)))
      case failure => success(Promise.successful(NotFound))
    }
  }

  val metadata = None
  
  /** Starts the server.
   */
  def start: Future[Any] = {
    log.info("Starting server")
    
    _status = RunningStatus.Starting
    
    // Start all the services:
    for (descriptor <- descriptors) {
      log.info("Starting service " + descriptor.service.toString)
      
      try {
        descriptor.startup onSuccess { 
          case _ => log.info("Successfully started service " + descriptor.service.toString)
        } onFailure { 
          case error => log.error("Failed to start service " + descriptor.service.toString, error)
        }
      } catch {
        case throwable => log.error("Failed to start service " + descriptor.service.toString, throwable)
      }
    }
    
    // As each handler becomes available, incorporate it into the master handler:
    val handlerFutures = descriptors.map(_.request)
    
    handlerFutures.foreach { future =>
      for (handler <- future) {
        handlerLock.writeLock.lock()
        try {
          _handler = _handler ~ handler
        } finally {
          handlerLock.writeLock.unlock()
        }
      }
    }
    
    Future.sequence(handlerFutures) onSuccess { 
      case _ =>
        log.info("Server started")
        _status = RunningStatus.Started
    } onFailure { 
      case ex =>
        _status = RunningStatus.Errored
        log.error("Unable to start server.", ex)
    }
  }
  
  /** Stops the server.
   */
  def stop: Future[Any] = {
    log.info("Stopping server")
    
    _status = RunningStatus.Stopping
    
    // Turn off the request handler so we don't handle any more requests for any service:
    handlerLock.writeLock.lock()
    
    try {
      _handler = OrService[ByteChunk, Future[HttpResponse[ByteChunk]]]()
    }
    finally {
      handlerLock.writeLock.unlock()
    }
    
    val shutdownFutures: List[Future[Any]] = descriptors.map { descriptor =>
      log.info("Shutting down service " + descriptor.service.toString)
      
      descriptor.shutdown.flatMap { stoppables => 
        stoppables.map(Stoppable.stop(_, stopTimeout)).getOrElse(Future(()))
      } onSuccess { 
        case _ => log.info("Successfully shut down service " + descriptor.service.toString)
      } onFailure { 
        case error => log.error("Failed to shut down service " + descriptor.service.toString, error)
      }
    }
    
    Future.sequence(shutdownFutures) onSuccess { 
      case _ => 
        log.info("Server stopped")
        _status = RunningStatus.Stopped
    } onFailure { 
      case error =>
        _status = RunningStatus.Errored
        log.error("Unable to stop server.", error)
    }
  }
  
  /** Retrieves the server configuration, which is always located in the 
   * "server" block of the root configuration object.
   */
  lazy val config: Configuration = rootConfig.detach("server")
  
  /** Retrieves the logger for the server, which is configured directly from
   * the server's "log" configuration block.
   */
  lazy val log: Logger = Logger("blueeyes.server")

  /** Retrieves the port the server should be running at, which defaults to
   * 8888.
   */
  lazy val port: Int    = config[Int]("port", 8888)

  /** Retrieves the ssl port the server should be running at, which defaults to
   * 8889.
   */
  lazy val sslPort: Int = config[Int]("sslPort", 8889)

  /** Retrieves the host the server should be running at.
   */
  lazy val host = config.get[String]("address").getOrElse(InetAddress.getLocalHost().getHostName())

  /** Retrieves the chunk size.
   */
  lazy val chunkSize = config[Int]("chunkSize", 1048576)

  /** Retrieves if the ssl should be running, which defaults to
   * true.
   */
  lazy val sslEnable: Boolean = config[Boolean]("sslEnable", true)

  /** The status of the server.
   */
  def status: RunningStatus = _status

  /** A default main function, which accepts the configuration file from the
   * command line, with flag "--configFile".
   */
  def main(args: Array[String]) {
    val arguments = CommandLineArguments(args: _*)
    
    if (arguments.size == 0 || !arguments.parameters.get("help").isEmpty) {
      println("Usage: --configFile [config file]")
      
      System.exit(-1)
    }
    else {    
      rootConfiguration = Configuration.load(
        arguments.parameters.get("configFile").getOrElse(sys.error("Expected --configFile option")),
        BlockFormat
      )
      
      start.onSuccess { case _ =>
        Runtime.getRuntime.addShutdownHook { new Thread {
          override def start() {
            val doneSignal = new CountDownLatch(1)
            
            self.stop.onSuccess { case _ => 
              doneSignal.countDown()
            }.onFailure { case e =>
              doneSignal.countDown()
            }
            
            doneSignal.await()
          }
        }}
      }
    }
  }

  private var _status: RunningStatus = RunningStatus.Stopped
  
  private val handlerLock = new java.util.concurrent.locks.ReentrantReadWriteLock
  
  private var _handler: AsyncHttpService[ByteChunk] = OrService[ByteChunk, Future[HttpResponse[ByteChunk]]]()
  
  private lazy val descriptors: List[BoundStateDescriptor[ByteChunk, _]] = services.map { service =>
    val config = rootConfig.detach("services." + service.name + ".v" + service.version.majorVersion)

    val context = ServiceContext(rootConfig, config, service.name, service.version, service.desc, host, port, sslPort)

    BoundStateDescriptor(context, service)
  }

  private case class BoundStateDescriptor[ByteChunk, S](context: ServiceContext, service: Service[ByteChunk]) {
    private val descriptor: ServiceDescriptor[ByteChunk, S] = service.descriptorFactory(context).asInstanceOf[ServiceDescriptor[ByteChunk, S]]

    lazy val startup: Future[S] = descriptor.startup()
    lazy val request: Future[AsyncHttpService[ByteChunk]] = startup.map(descriptor.request)
    lazy val shutdown: Future[Option[Stoppable]] = startup.flatMap(descriptor.shutdown)
  }
}
