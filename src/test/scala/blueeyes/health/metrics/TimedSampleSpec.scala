package blueeyes.health.metrics

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import blueeyes.concurrent.Future
import org.specs2.matcher.MustThrownMatchers
import java.util.concurrent.TimeUnit

class TimedSampleSpec extends Specification with MustThrownMatchers{
  private val clock = new Clock()
  "TimedSample" should{
    "create histogram" in{
      val timedSample = new TimedSampleImpl(interval(IntervalLength(3, TimeUnit.SECONDS), 7))(clock.now _)
      fill(timedSample)

      val histogram = timedSample.details
      histogram.value must eventually (beSome(Map(96 -> 0.0, 99 -> 2.0, 102 -> 5.0, 105 -> 0.0, 108 -> 0.0, 111 -> 3.0, 114 -> 4.0)))
    }
    "removes expired data" in{
      val timedSample = new TimedSampleImpl(interval(IntervalLength(3, TimeUnit.SECONDS), 3))(clock.now _)
      fill(timedSample)

      val histogram = timedSample.details
      histogram.value must eventually (beSome(Map(108 -> 0.0, 111 -> 3.0, 114 -> 4.0)))
    }
  }

  private def fill(timedSample: Statistic[Long, Map[Long, Double]]){
    set(timedSample, 100000)
    set(timedSample, 101000)
    set(timedSample, 102000)
    set(timedSample, 102100)

    set(timedSample, 103000)
    set(timedSample, 104000)
    set(timedSample, 104020)

    set(timedSample, 112000)
    set(timedSample, 112100)
    set(timedSample, 112020)

    set(timedSample, 114000)
    set(timedSample, 114100)
    set(timedSample, 114020)
    set(timedSample, 115000)
  }

  private def set(timedSample: Statistic[Long, Map[Long, Double]], now: Long) = {
    clock.setNow(now)
    timedSample += 1

    Thread.sleep(50)
  }

  class Clock{
    private var _now: Long = 0

    def now() = _now

    def setNow(value: Long){_now = value}
  }

  class TimedSampleImpl(intervalConfig: interval)(implicit clock: () => Long) extends TimedNumbersSample(intervalConfig)(clock){
    def toJValue = Future.sync(JNothing)
  }
}