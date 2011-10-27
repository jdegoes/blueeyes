package blueeyes.core.service

import blueeyes.core.http._
import scala.reflect.Manifest
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._

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
trait ServiceBuilder[T] {
  protected case class StartupDescriptor[S](startup: () => Future[S]) {
    def -> (request: RequestDescriptor[S]) = new StartupAndShutdownDescriptor(request)
    class StartupAndShutdownDescriptor(request: RequestDescriptor[S]){
      def -> (shutdown: ShutdownDescriptor[S]) = ServiceDescriptor[T, S](startup, request.request, shutdown.shutdown)
    }
  }
  protected case class RequestDescriptor[S](request: S => AsyncHttpService[T])
  protected case class ShutdownDescriptor[S](shutdown: S => Future[Unit])
  
  protected def startup[S](startup: => Future[S]): StartupDescriptor[S] = {
    val thunk = () => startup
    
    StartupDescriptor[S](thunk)
  }
  
  protected def request[S](request: S => AsyncHttpService[T]): RequestDescriptor[S] = RequestDescriptor[S](request)
  
  protected def request(request: => AsyncHttpService[T]): RequestDescriptor[Unit] = RequestDescriptor[Unit]((u) => request)
  
  protected def shutdown[S](shutdown: S => Future[Unit]): ShutdownDescriptor[S] = ShutdownDescriptor[S](shutdown)
  
  protected def shutdown(shutdown: => Future[Unit]): ShutdownDescriptor[Unit] = ShutdownDescriptor[Unit]((u) => shutdown)
  
  protected implicit def statelessRequestDescriptorToServiceDescriptor(rd: RequestDescriptor[Unit]): ServiceDescriptor[T, Unit] = 
    ServiceDescriptor[T, Unit](() => ().future, rd.request, _ => ().future)

  def service(name: String, version: String, desc: Option[String] = None)(descriptorFactory: ServiceContext => ServiceDescriptor[T, _])(implicit m: Manifest[T]): Service[T] = new ServiceImpl[T](name, version, desc, descriptorFactory)

  private class ServiceImpl[T](val name: String, val version: ServiceVersion, val desc: Option[String] = None, val descriptorFactory: ServiceContext => ServiceDescriptor[T, _])(implicit m: Manifest[T]) extends Service[T]{
    def ioClass: Class[T] = m.erasure.asInstanceOf[Class[T]]
  }
}
