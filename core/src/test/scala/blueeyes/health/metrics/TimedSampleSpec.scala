package blueeyes.health.metrics

import org.specs2.mutable.Specification
import blueeyes.json._
import akka.dispatch.Future
import blueeyes.util.Clock
import java.util.concurrent.TimeUnit
import blueeyes.akka_testing.FutureMatchers

class TimedSampleSpec extends Specification with TimedStatFixtures with FutureMatchers {
  "TimedSample" should{
    "create histogram" in{
      val timedSample = new TimedSampleImpl(interval(IntervalLength(3, TimeUnit.SECONDS), 7))
      fill(timedSample)

      val histogram = timedSample.details
      histogram must whenDelivered (be_==(Map(96 -> 0.0, 99 -> 2.0, 102 -> 5.0, 105 -> 0.0, 108 -> 0.0, 111 -> 3.0, 114 -> 4.0)))
    }
    "removes expired data" in{
      val timedSample = new TimedSampleImpl(interval(IntervalLength(3, TimeUnit.SECONDS), 3))
      fill(timedSample)

      val histogram = timedSample.details
      histogram must whenDelivered (be_==(Map(108 -> 0.0, 111 -> 3.0, 114 -> 4.0)))
    }
  }

  private def fill(timedSample: Statistic[Long]){
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
    set(timedSample, 118000)
  }

  private def set(timedSample: Statistic[Long], now: Long) = {
    clock.setNow(now)
    timedSample += 1

    Thread.sleep(50)
  }

  class TimedSampleImpl(intervalConfig: interval)(implicit clock: Clock) extends TimedNumbersSample(intervalConfig) {
    def toJValue = Future(JUndefined)
  }
}
