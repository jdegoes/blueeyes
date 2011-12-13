package blueeyes.core.service

import blueeyes.bkka._
import akka.dispatch.Future

case class ServiceDescriptor[T, S](startup: () => Future[S], request: S => AsyncHttpService[T], shutdown: S => Future[Option[Stoppable]]) extends AkkaDefaults { self =>
  def ~ [R](that: ServiceDescriptor[T, R]): ServiceDescriptor[T, S ~ R] = {
    ServiceDescriptor[T, S ~ R](
      startup  = () => self.startup() ~ that.startup(),
      request  = { case s ~ r => self.request(s) ~ that.request(r) },
      shutdown = { case s ~ r => self.shutdown(s).flatMap(_ => that.shutdown(r)) }
    )
  }

  def ~> (that: AsyncHttpService[T]): ServiceDescriptor[T, S] = {
    ServiceDescriptor[T, S](
      startup  = self.startup,
      request  = (s: S) => that ~ self.request(s),
      shutdown = self.shutdown
    )
  }
  
  def ~ (that: AsyncHttpService[T]): ServiceDescriptor[T, S] = {
    ServiceDescriptor[T, S](
      startup  = self.startup,
      request  = (s: S) => self.request(s) ~ that,
      shutdown = self.shutdown
    )
  }
}
