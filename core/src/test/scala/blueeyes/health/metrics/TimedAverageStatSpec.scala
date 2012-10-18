package blueeyes.health.metrics

import org.specs2.mutable.Specification
import blueeyes.json._
import java.util.concurrent.TimeUnit

class TimedAverageStatSpec extends Specification with TimedStatFixtures with blueeyes.concurrent.test.FutureMatchers {
  implicit val healthMonitorTimeout = akka.util.Timeout(10000)

  "TimedAverageStat" should{
    "creates JValue" in{
      val config = interval(IntervalLength(3, TimeUnit.SECONDS), 3)
      val timedSample = TimedAverageStat(config)
      fill(timedSample)

      val histogram      = timedSample.toJValue
      val histogramValue = JArray(List(JNum(1.3333333333333333), JNum(1.0), JNum(0.0)))
      histogram must whenDelivered (be_==(JObject(JField("perSecond", JObject(JField(config.toString, histogramValue) :: Nil)) :: Nil)))
    }
  }

  private def fill(timedSample: Statistic[Long]){
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
}
