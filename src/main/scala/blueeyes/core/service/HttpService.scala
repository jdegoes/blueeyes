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
trait HttpServiceBuilder[T] {
  protected case class StartupDescriptor[S](startup: () => Future[S]) {
    def -> (request: RequestDescriptor[S]) = new {
      def -> (shutdown: ShutdownDescriptor[S]) = HttpServiceDescriptor[T, S](startup, request.request, shutdown.shutdown)
    }
  }
  protected case class RequestDescriptor[S](request: S => HttpRequestHandler[T])
  protected case class ShutdownDescriptor[S](shutdown: S => Future[Unit])
  
  protected def startup[S](startup: => Future[S]): StartupDescriptor[S] = {
    val thunk = () => startup
    
    StartupDescriptor[S](thunk)
  }
  
  protected def request[S](request: S => HttpRequestHandler[T]): RequestDescriptor[S] = RequestDescriptor[S](request)
  
  protected def request(request: => HttpRequestHandler[T]): RequestDescriptor[Unit] = RequestDescriptor[Unit]((u) => request)
  
  protected def shutdown[S](shutdown: S => Future[Unit]): ShutdownDescriptor[S] = ShutdownDescriptor[S](shutdown)
  
  protected def shutdown(shutdown: => Future[Unit]): ShutdownDescriptor[Unit] = ShutdownDescriptor[Unit]((u) => shutdown)
  
  protected implicit def statelessRequestDescriptorToServiceDescriptor(rd: RequestDescriptor[Unit]): HttpServiceDescriptor[T, Unit] = 
    HttpServiceDescriptor[T, Unit](() => Future(()), rd.request, _ => Future(()))

  def service(name: String, version: String)(descriptorFactory: HttpServiceContext => HttpServiceDescriptor[T, _])(implicit m: Manifest[T]): HttpService2[T] = new HttpService2[T]{
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