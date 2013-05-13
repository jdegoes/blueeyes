package blueeyes.core.service

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext

import blueeyes.bkka.Stoppable
import blueeyes.core.http._

import com.weiglewilczek.slf4s.Logging

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
  
  protected def request[S](request: S => AsyncHttpService[T, T]): RequestDescriptor[T, S] = RequestDescriptor[T, S](request)
  
  protected def request(request: => AsyncHttpService[T, T]): RequestDescriptor[T, Unit] = RequestDescriptor[T, Unit]((u) => request)
  
  def service[S](sname: String, sversion: ServiceVersion, sdesc: Option[String] = None)(slifecycle: ServiceContext => ServiceLifecycle[T, S]): Service[T, S] = {
    new Service[T, S] {
      val name = sname
      val version = sversion
      val desc = sdesc

      def lifecycle(context: ServiceContext) = {
        import HttpRequestHandlerCombinators._

        // Service paths are always prefixed by service name and major version string.
        val ServiceLifecycle(startup, runningState) = slifecycle(context)
        ServiceLifecycle(
          startup, 
          (s: S) => {
            val (service, stoppable) = runningState(s)
            val service0 = path("/%s".format(sname)) { path("/%s".format(sversion.vname)) { service } }
            (service0, stoppable)
          }
        )
      }
    }
  }

  def shutdown[S](shutdown: S => Future[Any]): ShutdownDescriptor[S] = 
    ShutdownDescriptor(s => Some(Stoppable.fromFuture(shutdown(s))))

  def shutdown(shutdown: => Future[Any]): ShutdownDescriptor[Any] = 
    ShutdownDescriptor(_ => Some(Stoppable.fromFuture(shutdown)))
  
  def stop[S](stop: S => Stoppable): ShutdownDescriptor[S] = 
    ShutdownDescriptor(s => Some(stop(s)))

  def stop(stop: Stoppable): ShutdownDescriptor[Any] = 
    ShutdownDescriptor(_ => Some(stop))

  protected implicit def statelessRequestDescriptorToServiceLifecycle(rd: RequestDescriptor[T, Unit])(implicit ctx: ExecutionContext): ServiceLifecycle[T, Unit] =
    ServiceLifecycle[T, Unit](() => Promise.successful(()), (_: Unit) => (rd.serviceBuilder(()), None))
}

case class StartupDescriptor[T, S](startup: () => Future[S]) {
  def -> (request: RequestDescriptor[T, S]) = new StartupAndShutdownDescriptor(request)
  class StartupAndShutdownDescriptor(request: RequestDescriptor[T, S]){
    def -> (shutdown: ShutdownDescriptor[S]) = ServiceLifecycle[T, S](startup, s => (request.serviceBuilder(s), shutdown.shutdownBuilder(s)))
  }
}

case class RequestDescriptor[T, S](serviceBuilder: S => AsyncHttpService[T, T])
case class ShutdownDescriptor[-S](shutdownBuilder: S => Option[Stoppable])
