package blueeyes.core.service

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext

import blueeyes.bkka.AkkaDefaults
import blueeyes.bkka.Stoppable
import blueeyes.core.http._

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
trait ServiceBuilder[T] extends AkkaDefaults {
  protected def startup[S](startup: => Future[S]): StartupDescriptor[T, S] = {
    val thunk = () => startup
    
    StartupDescriptor[T, S](thunk)
  }
  
  protected def request[S](request: S => AsyncHttpService[T]): RequestDescriptor[T, S] = RequestDescriptor[T, S](request)
  
  protected def request(request: => AsyncHttpService[T]): RequestDescriptor[T, Unit] = RequestDescriptor[T, Unit]((u) => request)
  
  def service[S](sname: String, sversion: ServiceVersion, sdesc: Option[String] = None)(slifecycle: ServiceContext => ServiceLifecycle[T, S]): Service[T, S] = {
    new Service[T, S] {
      val name = sname
      val version = sversion
      val desc = sdesc
      def lifecycle(context: ServiceContext) = slifecycle(context)
    }
  }
  
  def shutdown[S](shutdown: S => Future[Any])(implicit c : ShutdownConverter[S, Any]): ShutdownDescriptor[S] = 
    ShutdownDescriptor[S](c.convert(shutdown))

  def shutdown(shutdown: => Future[Any])(implicit c : ShutdownConverter[Unit, Any]): ShutdownDescriptor[Unit] = 
    ShutdownDescriptor[Unit](c.convert(shutdown))
  
  protected implicit def statelessRequestDescriptorToServiceLifecycle(rd: RequestDescriptor[T, Unit])(implicit ctx: ExecutionContext): ServiceLifecycle[T, Unit] =
    ServiceLifecycle[T, Unit](() => Promise.successful(()), (_: Unit) => (rd.serviceBuilder(()), None))
}

case class StartupDescriptor[T, S](startup: () => Future[S]) {
  def -> (request: RequestDescriptor[T, S]) = new StartupAndShutdownDescriptor(request)
  class StartupAndShutdownDescriptor(request: RequestDescriptor[T, S]){
    def -> (shutdown: ShutdownDescriptor[S]) = ServiceLifecycle[T, S](startup, s => (request.serviceBuilder(s), shutdown.shutdownBuilder(s)))
  }
}

case class RequestDescriptor[T, S](serviceBuilder: S => AsyncHttpService[T])
case class ShutdownDescriptor[S](shutdownBuilder: S => Option[Stoppable])

trait ShutdownConverter[S, T] {
  def convert(f : S => Future[T]) : S => Option[Stoppable]
  def convert(f : => Future[T])   : S => Option[Stoppable]
}

object ShutdownConverter {
  implicit def unitConverter[S] = new ShutdownConverter[S, Any] {
    def convert(f : S => Future[Any]) = { (s: S) => Some(Stoppable.fromFuture(f(s))) }
    def convert(f : => Future[Any])   = { (_: S) => Some(Stoppable.fromFuture(f)) }
  }

/*
  implicit def optStopConverter[S] = new ShutdownConverter[S, Option[Stoppable]] {
    def convert(f : S => Future[Option[Stoppable]]) = f
    def convert(f : => Future[Option[Stoppable]]) = { _ => f }
  }
*/
}

