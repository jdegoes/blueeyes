package blueeyes.core.service

import blueeyes.core.http._
import blueeyes.util.Future
import scala.reflect.Manifest

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