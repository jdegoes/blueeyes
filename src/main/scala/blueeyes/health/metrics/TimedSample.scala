package blueeyes.health.metrics

import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import akka.actor.Actor
import akka.actor.Actor._
import histogram.{DynamicHistogram, ValueStrategy}

abstract class TimedSample[V](val config: interval)(implicit valueStrategy: ValueStrategy[V], clock: () => Long) extends AsyncStatistic[Long, Map[Long, V]]{

  private val actor = actorOf(new TimedSampleActor(DynamicHistogram.empty(config.granularity.length, config.samples, config.granularity.unit))).start()

  def +=(elem: Long): this.type = {
    actor ! DataRequest(clock(), elem)
    this
  }

  def count = actor.!!![Long](CountRequest, 1000 * 60 * 60).toBlueEyes

  def details: Future[Map[Long, V]] = actor.!!![Map[Long, V]](DetailsRequest, 1000 * 60 * 60).toBlueEyes

  def shutdown() = actor.stop()
}

private[metrics] class TimedSampleActor[V](var histogram: DynamicHistogram[V]) extends Actor {
  def receive = {
    case data: DataRequest  => histogram = histogram += (data.timeMs, data.data)
    case DetailsRequest     => self.reply(histogram.histogram)
    case CountRequest       => self.reply(histogram.count)
    case _ => sys.error("wrong message.")
  }
}

private[metrics] sealed trait TimedSampleRequest
private[metrics] case class DataRequest(timeMs: Long, data: Long)
private[metrics] case object DetailsRequest
private[metrics] case object CountRequest
