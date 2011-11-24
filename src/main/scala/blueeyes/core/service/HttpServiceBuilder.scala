package blueeyes.core.service

import blueeyes.bkka.Stoppable
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
  protected def startup[S](startup: => Future[S]): StartupDescriptor[T, S] = {
    val thunk = () => startup
    
    StartupDescriptor[T, S](thunk)
  }
  
  protected def request[S](request: S => AsyncHttpService[T]): RequestDescriptor[T, S] = RequestDescriptor[T, S](request)
  
  protected def request(request: => AsyncHttpService[T]): RequestDescriptor[T, Unit] = RequestDescriptor[T, Unit]((u) => request)
  
  trait ShutdownConverter[S,T] {
    def convert(f : S => Future[T]) : S => Future[Option[Stoppable]]
    def convert(f : => Future[T]) : S => Future[Option[Stoppable]]
  }

  implicit def unitConverter[S] = new ShutdownConverter[S,Unit] {
    def convert(f : S => Future[Unit]) = { (s : S) => f(s).flatMap(_ => Future.sync(None)) }
    def convert(f : => Future[Unit]) = { _ => f.flatMap(_ => Future.sync(None)) }
  }

  implicit def optStopConverter[S] = new ShutdownConverter[S,Option[Stoppable]] {
    def convert(f : S => Future[Option[Stoppable]]) = f
    def convert(f : => Future[Option[Stoppable]]) = { _ => f }
  }

  protected def shutdown[S,T](shutdown: S => Future[T])(implicit c : ShutdownConverter[S,T]): ShutdownDescriptor[S] = 
    ShutdownDescriptor[S](c.convert(shutdown))

  protected def shutdown[T](shutdown: => Future[T])(implicit c : ShutdownConverter[Unit,T]): ShutdownDescriptor[Unit] = 
    ShutdownDescriptor[Unit](c.convert(shutdown))
  
  protected implicit def statelessRequestDescriptorToServiceDescriptor(rd: RequestDescriptor[T, Unit]): ServiceDescriptor[T, Unit] =
    ServiceDescriptor[T, Unit](() => ().future, rd.request, _ => Future.sync(None))

  def service(name: String, version: String, desc: Option[String] = None)(descriptorFactory: ServiceContext => ServiceDescriptor[T, _])(implicit m: Manifest[T]): Service[T] = new ServiceImpl[T](name, version, desc, descriptorFactory)

  private class ServiceImpl[T](val name: String, val version: ServiceVersion, val desc: Option[String] = None, val descriptorFactory: ServiceContext => ServiceDescriptor[T, _])(implicit m: Manifest[T]) extends Service[T]{
    def ioClass: Class[T] = m.erasure.asInstanceOf[Class[T]]
  }
}

case class StartupDescriptor[T, S](startup: () => Future[S]) {
  def -> (request: RequestDescriptor[T, S]) = new StartupAndShutdownDescriptor(request)
  class StartupAndShutdownDescriptor(request: RequestDescriptor[T, S]){
    def -> (shutdown: ShutdownDescriptor[S]) = ServiceDescriptor[T, S](startup, request.request, shutdown.shutdown)
  }
}
case class RequestDescriptor[T, S](request: S => AsyncHttpService[T])
case class ShutdownDescriptor[S](shutdown: S => Future[Option[Stoppable]])

