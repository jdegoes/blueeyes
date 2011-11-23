package blueeyes.bkka

import akka.actor.Actor
import akka.dispatch.Future
import scala.collection.immutable.Queue

trait Stop[-A] {
  def stop(a: A): Future[Any]
}

sealed trait Stoppable {
  protected def stop: Future[Any]
  def dependents: List[Stoppable]
}

object Stoppable {
  def apply[A](a: A, deps: List[Stoppable] = Nil)(implicit stopa: Stop[A], timeout: Actor.Timeout): Stoppable = new Stoppable {
    protected def stop = stopa.stop(a)
    def dependents = deps
  }

  def stop(stoppable: Stoppable)(implicit timeout: Actor.Timeout) = {
    def _stop(q: Queue[List[Stoppable]]): Future[List[Any]] = {
      if (q.isEmpty) Future(Nil)
      else {
        val (xs, remainder) = q.dequeue
        Future.sequence(xs.map(_.stop), timeout.duration.toMillis).flatMap(r => _stop(remainder ++ xs.map(_.dependents)).map(r ::: _))
      }
    }
    
    _stop(Queue(List(stoppable)))
  }
}


// vim: set ts=4 sw=4 et:
