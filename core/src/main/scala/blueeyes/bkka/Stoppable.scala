package blueeyes.bkka

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.PoisonPill
import akka.actor.Props
import akka.actor.Terminated
import akka.actor.ReceiveTimeout
import akka.dispatch.Await
import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.MessageDispatcher
import akka.util.Timeout
import scala.collection.immutable.Queue
import com.weiglewilczek.slf4s.Logging

/**
 * A base trait for typeclass instances that describe how to stop a given type of service.
 * TODO: make polymorphic in the type of the Future, perhaps using an ADT such as StopResult { StopSuccess / StopFailure }
 * 
 * In order for implementations of this trait to work properly in a graph where the same service is a dependant of multiple other services, 
 * it should be safe for stop to be called multiple times with the same target.
 */
trait Stop[-A] {
  def stop(a: A): Future[Any]
}

/** 
 * A DAG of things that can be stopped. Dependents are traversed in breadth-first order.
 */
sealed trait Stoppable {
  protected def stop: Future[Any]
  def dependents: List[Stoppable]
}

object Stoppable extends Logging {
  def apply[A](a: A, deps: List[Stoppable] = Nil)(implicit stopa: Stop[A], messageDispatcher: MessageDispatcher): Stoppable = new Stoppable {
    protected def stop = {
      Future(logger.info("About to stop " + a)) flatMap { _ =>
        stopa.stop(a).onSuccess {
          case v => logger.info("Stopped " + a + " with result " + v)
        } recover { case ex => 
          logger.error("Stop failed", ex)
        } 
      }
    }

    def dependents = deps
  }

  /**
   * Stops the specified stoppable, returning a future containing a list of the results
   * of the stoppable graph in breadth-first order. When this future is completed, 
   * everything will be stopped.
   *
   * TODO: Make it possible to specify whether failure to stop any given service
   * should prevent the stopping of its dependants. At present, any exception encountered
   * in stopping will stop the stopping process, leaving the system in a potentially 
   * indeterminate state.
   */
  implicit def stoppableStop(timeout: Timeout)(implicit messageDispatcher: MessageDispatcher): Stop[Stoppable] = new Stop[Stoppable] {
    def stop(stoppable: Stoppable) = {
      def _stop(q: Queue[List[Stoppable]]): Future[List[Any]] = {
        if (q.isEmpty) Future(Nil)
        else {
          val (xs, remainder) = q.dequeue
          Future.sequence(xs.map(s => Future(Await.result(s.stop, timeout.duration)))).flatMap(r => _stop(remainder ++ xs.map(_.dependents)).map(r ::: _))
                 //Future.sequence(xs.map(_.stop)).flatMap(r => _stop(remainder ++ xs.map(_.dependents)).map(r ::: _))
        }
      }
      
      _stop(Queue(List(stoppable)))
    }
  }

  implicit def stoppableStop(implicit messageDispatcher: MessageDispatcher): Stop[Stoppable]  = stoppableStop(Timeout.never)


  def stop(stoppable: Stoppable, timeout: Timeout = Timeout.never)(implicit messageDispatcher: MessageDispatcher) = 
    stoppableStop(timeout).stop(stoppable)
}

case class ActorRefStop(actorSystem: ActorSystem, timeout: Timeout) extends Stop[ActorRef] {
  class StopMonitor(target: ActorRef, result: Promise[Unit], timeout: Timeout) extends Actor {
    // Terminated will be received when target has been stopped
    context watch target
    // ReceiveTimeout will be received if nothing else is received within the timeout
    context.setReceiveTimeout(timeout.duration)

    def receive = {
      case Terminated(a) ⇒ 
        result.complete(Right(()))
        self ! PoisonPill

      case ReceiveTimeout ⇒ 
        result.complete(Left(new RuntimeException("Failed to stop [%s] within [%s]".format(target.path, context.receiveTimeout))))
        self ! PoisonPill
    }
  }

  override def stop(target: ActorRef): Future[Unit] = {
    val exitFuture = Promise[Unit]()(actorSystem.dispatcher)
    val stopMonitor = actorSystem.actorOf(Props(new StopMonitor(target, exitFuture, timeout)))
    target ! PoisonPill
    exitFuture
  }
}


// vim: set ts=4 sw=4 et:
