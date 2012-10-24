package blueeyes.core.service

import akka.dispatch.Future
import blueeyes.bkka.Stoppable
import scalaz.std.option._
import scalaz.syntax.semigroup._

/**
 * An http service, which responds to http requests with http responses. 
 * Services are typed in whatever type is required by the server engine.
 * For example, some server engines might only deal with strings.
 */
trait Service[T, S] {
  def name: String
  
  def version: ServiceVersion

  def desc: Option[String]

  def lifecycle(context: ServiceContext): ServiceLifecycle[T, S]
  
  override def toString = name + "." + version.majorVersion
}

case class ServiceLifecycle[T, S](startup: () => Future[S], runningState: S => (AsyncHttpService[T], Option[Stoppable])) { self =>
  def ~ [S0](that: ServiceLifecycle[T, S0]): ServiceLifecycle[T, (S, S0)] = {
    ServiceLifecycle[T, (S, S0)](
      startup  = () => self.startup() zip that.startup(),
      runningState = { 
        case (s, r) => 
          val (handlerS, stoppableS) = self.runningState(s) 
          val (handlerR, stoppableR) = that.runningState(r)

          ((handlerS ~ handlerR), stoppableS |+| stoppableR)
      }
    )
  }

  def ~> (that: AsyncHttpService[T]): ServiceLifecycle[T, S] = {
    self.copy(
      runningState = (s: S) => {
        val (service, stoppable) = self.runningState(s)
        (that ~ service, stoppable)
      }
    )
  }
  
  def ~ (that: AsyncHttpService[T]): ServiceLifecycle[T, S] = {
    self.copy(
      runningState = (s: S) => {
        val (service, stoppable) = self.runningState(s)
        (service ~ that, stoppable)
      }
    )
  }

  def run = startup() map runningState
}
