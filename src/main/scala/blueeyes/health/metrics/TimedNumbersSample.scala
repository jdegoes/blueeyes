package blueeyes.health.metrics

import java.util.concurrent.atomic.AtomicLong
import blueeyes.concurrent.Future

import histogram.ValueStrategy._

abstract class TimedNumbersSample(config: interval)(implicit clock: () => Long) extends TimedSample[Double](config)

abstract class EternityTimedNumbersSample(implicit clock: () => Long) extends AsyncStatistic[Long, Map[Long, Double]]{
  private val startTime = clock()
  private val _count    = new AtomicLong(0)

  def +=(element: Long) = {
    _count.getAndAdd(element)
    this
  }

  def count = Future.sync(_count.get)

  def details = Future.sync(Map[Long, Double](startTime -> _count.get))


  def shutdown() {}

  def config = eternity
}