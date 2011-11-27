package blueeyes.health.metrics

import blueeyes.bkka.Stop
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import blueeyes.util.Clock
import akka.actor.Actor
import akka.actor.Actor._
import akka.actor.ActorKilledException
import akka.actor.PoisonPill
import histogram.{DynamicHistogram, ValueStrategy}

abstract class TimedSample[V](val config: interval)(implicit valueStrategy: ValueStrategy[V], clock: Clock, m: Manifest[V])
extends AsyncStatistic[Long, Map[Long, V]]{
  private[TimedSample] val actor = actorOf(new TimedSampleActor(DynamicHistogram.empty(config.granularity.length, config.samples + 1, config.granularity.unit))).start()

  def +=(elem: Long): this.type = {
    actor ! DataRequest(clock.now().getMillis(), elem)
    this
  }

  def count = (actor ? CountRequest).mapTo[Long].toBlueEyes

  def details = (actor ? DetailsRequest).mapTo[Map[Long, V]].toBlueEyes

  lazy val shutdown = (actor ? PoisonPill).mapTo[Unit].recover({ case ex: ActorKilledException => () })
}

object TimedSample {
  implicit def stop[A]: Stop[TimedSample[A]] = new Stop[TimedSample[A]] {
    def stop(t: TimedSample[A]) = t.shutdown
  }
}

private[metrics] class TimedSampleActor[V](var histogram: DynamicHistogram[V]) extends Actor {
  def receive = {
    case DataRequest(time, data)  => histogram = histogram += (time, data)
    case DetailsRequest     =>
      val details    = histogram.histogram
      val incomplete = details.keySet.toList.sortWith(_ > _).head
      self.reply(details - incomplete)
    case CountRequest       => self.reply(histogram.count)
  }
}

private[metrics] sealed trait TimedSampleRequest
private[metrics] case class DataRequest(timeMs: Long, data: Long)
private[metrics] case  object DetailsRequest
private[metrics] case object CountRequest
