package blueeyes.core.service

/**
 * An http service - the fundamental concept in Blue Eyes.
 */
trait HttpService[S] extends RestHierarchy[S] {
  /** The short name of the service, e.g. "email"
   */
  def name: String
  
  /** The version of the service, e.g. "32" 
   */
  def version: Int
  
  /** The root path of the service, which defaults to "/"
   */
  def rootPath: String = "/"
}

import blueeyes.core.http._
import blueeyes.util.Future
import net.lag.configgy.{Config, ConfigMap}
import net.lag.logging.Logger
import scala.reflect.Manifest

case class HttpServiceContext(config: ConfigMap, log: Logger)

case class HttpServiceDescriptor[T, S](startup: () => Future[S], request: S => HttpRequestHandler[T], shutdown: S => Future[Unit]) { self =>
  def + [R](that: HttpServiceDescriptor[T, R]): HttpServiceDescriptor[T, (S, R)] = {
    HttpServiceDescriptor[T, (S, R)](
      startup  = () => self.startup().zip(that.startup()),
      request  = (pair: (S, R)) => self.request(pair._1).orElse(that.request(pair._2)),
      shutdown = (pair: (S, R)) => self.shutdown(pair._1).zip(that.shutdown(pair._2)).map(_ => Unit)
    )
  }
}

/**
val emailService = {
  service("email", "1.23") { context =>
    startup {
  
    } -> 
    request { state =>
  
    } ->
    shutdown { state =>
  
    }
  
  }
}
*/
trait HttpServiceBuilder {
  case class StartupDescriptor[T, S](startup: () => Future[S]) {
    def -> (request: RequestDescriptor[T, S]) = new {
      def -> (shutdown: ShutdownDescriptor[T, S]) = HttpServiceDescriptor[T, S](startup, request.request, shutdown.shutdown)
    }
  }
  case class RequestDescriptor[T, S](request: S => HttpRequestHandler[T])
  case class ShutdownDescriptor[T, S](shutdown: S => Future[Unit])
  
  def startup[T, S](startup: => Future[S]): StartupDescriptor[T, S] = {
    val thunk = () => startup
    
    StartupDescriptor[T, S](thunk)
  }
  
  def request[T, S](request: S => HttpRequestHandler[T]): RequestDescriptor[T, S] = RequestDescriptor[T, S](request)
  
  def shutdown[T, S](shutdown: S => Future[Unit]): ShutdownDescriptor[T, S] = ShutdownDescriptor[T, S](shutdown)
  
  implicit def statelessRequestDescriptorToServiceDescriptor[T](rd: RequestDescriptor[T, Unit]): HttpServiceDescriptor[T, Unit] = 
    HttpServiceDescriptor[T, Unit](() => Future(()), rd.request, _ => Future(()))

  def service[T](name: String, version: String)(descriptorFactory: HttpServiceContext => HttpServiceDescriptor[T, _])(implicit m: Manifest[T]): HttpService2[T] = new HttpService2[T]{
    def name = name
    
    def version = version
    
    def descriptorFactory: HttpServiceContext => HttpServiceDescriptor[T, _] = descriptorFactory
    
    def ioClass: Class[T] = m.erasure.asInstanceOf[Class[T]]
  }
}

/**
 * An http service, which responds to http requests with http responses. 
 * Services are typed in whatever type is required by the server implementation.
 * For example, some server implementations might only deal with strings.
 */
trait HttpService2[T] {
  def name: String
  
  def version: String
  
  def majorVersion: Int = version.split(".").drop(0).headOption.map(_.toInt).getOrElse(0)
  
  def minorVersion: Int = version.split(".").drop(1).headOption.map(_.toInt).getOrElse(0)
  
  def descriptorFactory: HttpServiceContext => HttpServiceDescriptor[T, _]
  
  def ioClass: Class[T]
  
  override def toString = name + "." + majorVersion
}