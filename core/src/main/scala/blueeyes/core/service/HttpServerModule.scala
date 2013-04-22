package blueeyes.core.service

import blueeyes.bkka.Stoppable
import akka.actor.ActorSystem
import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext
import akka.util.Timeout

import blueeyes.core.data.ByteChunk
import blueeyes.core.http._
import blueeyes.core.service._
import blueeyes.util.RichThrowableImplicits._
import blueeyes.util.CommandLineArguments

import java.lang.reflect.{Method}
import java.util.concurrent.CountDownLatch
import java.util.zip.Deflater
import java.net.InetAddress
import java.nio.ByteBuffer

import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat

import com.weiglewilczek.slf4s.Logging
import com.weiglewilczek.slf4s.Logger


import scalaz._
import scalaz.Validation._
import scalaz.std.function._
import scalaz.syntax.arrow._
import scalaz.syntax.show._
import scalaz.syntax.validation._
import scalaz.syntax.std.boolean._

object HttpServerConfig {
  type CompressionLevel = Int
}

class HttpServerConfig(val config: Configuration) {
  import HttpServerConfig._

  /** Retrieves the port the server should be running at, which defaults to
   * 8888.
   */
  def port: Int = config[Int]("port", 8888)

  /** Retrieves the ssl port the server should be running at, which defaults to
   * 8889.
   */
  def sslPort: Int = config[Int]("sslPort", 8889)

  /** Retrieves the host the server should be running at.
   */
  def host = config.get[String]("address").getOrElse(InetAddress.getLocalHost().getHostName())

  /** Retrieves the chunk size.
   */
  def chunkSize = config[Int]("chunkSize", 1048576)

  /** Retrieves if the ssl should be running, which defaults to
   * true.
   */
  def sslEnable: Boolean = config[Boolean]("sslEnable", true)

  /**
   * Set compressionEnable to false to specifically disable compression.
   */
  def enableCompression: Boolean = config[Boolean]("compressionEnable", true)

  /**
   * A compression level for deflate encoding, which must be a value between 1 and 9. 
   * GZip encoding does not require a specific level to be set, but the value must be specified.
   */
  def compressionLevel: Option[CompressionLevel] = config.get[CompressionLevel]("compressionLevel")
                                                   .orElse(enableCompression.option(Deflater.BEST_SPEED))
                                                   .filter(l => l >= 1 && l <= 9)

  /**
   * The default timeout to be used when stopping dependent services is forever. Override this value
   * to provide a different timeout.
   */
  def stopTimeout = Timeout(config[Long]("shutdownTimeout", Long.MaxValue))
}

/** An http server acts as a container for services. A server can be stopped
 * and started, and has a main function so it can be mixed into objects.
 */
trait HttpServerModule extends Logging {
  type HttpServer <: HttpServerLike

  def server(rootConfig: Configuration, executionContext: ExecutionContext): HttpServer

  class HttpServerLike(rootConfig: Configuration, services: List[Service[ByteChunk, _]], executor0: ExecutionContext) { self =>
    implicit protected val executor: ExecutionContext = executor0

    val config = new HttpServerConfig(rootConfig.detach("server"))
    import config._
    
    private def context[T, S](service: Service[T, S]): ServiceContext = {
      val serviceConfig = rootConfig.detach("services." + service.name + ".v" + service.version.majorVersion)
      ServiceContext(rootConfig, serviceConfig, service.name, service.version, service.desc, host, port, sslPort)
    }

    def start: Option[Future[(AsyncHttpService[ByteChunk, ByteChunk], Option[Stoppable])]] = {
      def append[S](lifecycle: ServiceLifecycle[ByteChunk, S], tail: List[Service[ByteChunk, _]]): ServiceLifecycle[ByteChunk, _] = {
        tail match {
          case x :: xs => append(lifecycle ~ x.lifecycle(context(x)), xs)
          case Nil => lifecycle 
        }
      }

      val lifecycle = services match {
        case x :: xs => Some(append(x.lifecycle(context(x)), xs))
        case Nil => None
      }

      lifecycle map { _.run map { (trapErrors _).first } }
    }

    def trapErrors(delegate: AsyncHttpService[ByteChunk, ByteChunk]): AsyncHttpService[ByteChunk, ByteChunk] = new CustomHttpService[ByteChunk, Future[HttpResponse[ByteChunk]]] {
      private def convertErrorToResponse(th: Throwable): HttpResponse[ByteChunk] = th match {
        case e: HttpException => HttpResponse[ByteChunk](HttpStatus(e.failure, e.reason))
        case e => {
          logger.error("Error handling request", e)
          HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.InternalServerError, Option(e.getMessage).getOrElse("")))
        }
      }

      val service = (r: HttpRequest[ByteChunk]) => {
        // The raw future may die due to error:
        val rawValidation = try {
           delegate.service(r)
        } catch {
          // An error during invocation of the request handler, convert to
          // proper response:
          case error: Throwable => success(Promise.successful(convertErrorToResponse(error)))
        }

        // Convert the raw future into one that cannot die:
        rawValidation match {
          case Success(rawFuture) => success((rawFuture recover { case error => convertErrorToResponse(error) }))
          case Failure(DispatchError(throwable)) => success(Future(convertErrorToResponse(throwable)))
          case failure => 
            val message = "No handler could be found for your request: " + r.show
            success(
              Promise.successful(HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.NotFound, message)))
            )
        }
      }

      val metadata = delegate.metadata
    }
  }
}


trait HttpServerMain extends HttpServerModule {
  /** A default main function, which accepts the configuration file from the
   * command line, with flag "--configFile".
   */
  def main(args: Array[String]) {
    val arguments = CommandLineArguments(args: _*)
    
    if (arguments.size == 0 || !arguments.parameters.get("help").isEmpty) {
      println("Usage: --configFile [config file]")
      
      System.exit(-1)
    } else {    
      val rootConfiguration = Configuration.load(
        arguments.parameters.get("configFile").getOrElse(sys.error("Expected --configFile option")),
        BlockFormat
      )
      run(rootConfiguration)
    }
  }

  /**
   * The entry point to run HTTP server with the given root configuration.
   * It can be useful to replace the default configuration from the {@link HttpServerMain#main} method.
   */
  def run(rootConfiguration: Configuration) = {
    val actorSystem: ActorSystem = ActorSystem(rootConfiguration[String]("blueeyes.actor_system", "blueeyes-actors"))
    val executionContext: ExecutionContext = ExecutionContext.defaultExecutionContext(actorSystem)

    /** Retrieves the logger for the server, which is configured directly from
      * the server's "log" configuration block.
      */
    val log: Logger = Logger("blueeyes.server")
    server(rootConfiguration, executionContext).start map {
      _ onSuccess { case (runningState, stoppable) =>
        log.info("Services started.")
        Runtime.getRuntime.addShutdownHook { new Thread {
          override def start() {
            val doneSignal = new CountDownLatch(1)

            stoppable map { stop =>
              Stoppable.stop(stop)(executionContext)
              doneSignal.countDown()
            } getOrElse {
              doneSignal.countDown()
            }

            doneSignal.await()
            actorSystem.shutdown()
          }
        }}
      } onFailure {
        case ex =>
          System.err.println("Startup of BlueEyes failed due to an unhandled exception.")
          ex.printStackTrace(System.err)
          actorSystem.shutdown()
      }
    }
  }
}

/** 
 * Reflectively discovers service handlers from the fields of the class into which it is mixed.
 */
trait ReflectiveServiceList[T] { self =>
  def services: List[Service[T, _]] = {
    val c = self.getClass
    
    val allMethods: List[Method] = c.getDeclaredMethods.toList
    
    val serviceMethods: List[Method] = allMethods.reverse.filter { method =>
      classOf[Service[T, _]].isAssignableFrom(method.getReturnType) && method.getParameterTypes.length == 0
    }
    
    serviceMethods.map { method =>
      method.invoke(self).asInstanceOf[Service[T, Any]]
    }
  }
}

