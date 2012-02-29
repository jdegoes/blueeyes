package blueeyes.health.metrics

import blueeyes.bkka.Stop
import blueeyes.bkka.ActorRefStop
import blueeyes.util.Clock

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.ActorKilledException
import akka.actor.Props
import akka.actor.PoisonPill
import akka.dispatch.Future
import akka.dispatch.Promise
import akka.util.Timeout

import histogram.{DynamicHistogram, ValueStrategy}

abstract class TimedSample[V](val config: interval)(implicit valueStrategy: ValueStrategy[V], clock: Clock, m: Manifest[V])
extends AsyncStatistic[Long, Map[Long, V]]{
  val actorSystem = ActorSystem("timed_sample") //TODO: Specialize
  private[TimedSample] val actor = actorSystem.actorOf(Props(new TimedSampleActor(DynamicHistogram.empty(config.granularity.length, config.samples + 1, config.granularity.unit))))

  def +=(elem: Long): this.type = {
    actor ! DataRequest(clock.now().getMillis(), elem)
    this
  }

  def count: Future[Long] = {
    val promise = Promise[Long]()
    actor ! CountRequest(promise)
    promise
  }

  def details: Future[Map[Long, V]] = {
    val promise = Promise[Map[Long, V]]()
    actor ! DetailsRequest(promise)
    promise
  }

  def shutdown(timeout: Timeout) = ActorRefStop(actorSystem, timeout).stop(actor)
}

object TimedSample {
  implicit def stop[A](implicit timeout: Timeout): Stop[TimedSample[A]] = new Stop[TimedSample[A]] {
    def stop(t: TimedSample[A]) = t.shutdown(timeout)
  }
}

private[metrics] class TimedSampleActor[V](var histogram: DynamicHistogram[V]) extends Actor {
  def receive = {
    case DataRequest(time, data) => 
      histogram = histogram += (time, data)

    case DetailsRequest(promise) =>
      val details    = histogram.histogram
      val incomplete = details.keySet.toList.sortWith(_ > _).head
      promise.complete(Right(details - incomplete))

    case CountRequest(promise) => 
      promise.complete(Right(histogram.count))
  }
}

private[metrics] sealed trait TimedSampleRequest
private[metrics] case class DataRequest(timeMs: Long, data: Long)
private[metrics] case class DetailsRequest[V](promise: Promise[Map[Long, V]])
private[metrics] case class CountRequest(promise: Promise[Long])
