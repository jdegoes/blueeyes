package blueeyes.health.metrics

import java.util.concurrent.atomic.AtomicLong
import blueeyes.concurrent.Future
import blueeyes.util.Clock
import akka.actor.Actor

import histogram.ValueStrategy._

abstract class TimedNumbersSample(config: interval)(implicit clock: Clock) extends TimedSample[Double](config)

abstract class EternityTimedNumbersSample(implicit clock: Clock) extends AsyncStatistic[Long, Map[Long, Double]] {
  private val startTime = clock.now().getMillis
  private val _count    = new AtomicLong(0)

  def +=(element: Long) = {
    _count.getAndAdd(element)
    this
  }

  def count = Future.sync(_count.get)

  def details = Future.sync(Map[Long, Double](startTime -> _count.get))

  def shutdown = akka.dispatch.Future(())

  def config = eternity
}
