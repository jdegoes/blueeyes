package blueeyes.health.metrics

import akka.actor.Actor
import akka.dispatch.Future
import akka.util.Timeout

import java.util.concurrent.atomic.AtomicLong

import blueeyes.util.Clock
import blueeyes.json.JsonAST._
import histogram.ValueStrategy._

abstract class TimedNumbersSample(config: interval)(implicit clock: Clock) extends TimedSample[Double](config)

class EternityTimedNumbersSample(implicit clock: Clock) extends SyncStatistic[Long, Map[Long, Double]] {
  private val startTime = clock.now().getMillis
  private val _count    = new AtomicLong(0)

  def +=(element: Long) = {
    _count.getAndAdd(element)
    this
  }

  def count = _count.get

  def details = Map[Long, Double](startTime -> _count.get)

  def config = eternity

  def toJValue = JNum(count)
}
