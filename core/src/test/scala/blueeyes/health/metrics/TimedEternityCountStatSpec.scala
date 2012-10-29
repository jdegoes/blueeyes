package blueeyes.health.metrics

import blueeyes.json._
import org.specs2.mutable.Specification
import blueeyes.akka_testing.FutureMatchers

class TimedEternityCountStatSpec extends Specification with TimedStatFixtures with FutureMatchers {
  implicit val healthMonitorTimeout = akka.util.Timeout(10000)

  "EternityTimedCountStat" should{
    "creates JValue" in{
      val timedSample = TimedCountStat(eternity)
      fill(timedSample)

      timedSample.toJValue must whenDelivered (be_==(JObject(JField(eternity.toString, JArray(List(JNum(4)))) :: Nil)))
    }
  }

  private def fill(timedSample: Statistic[Long]){
    set(timedSample, 1001)
    set(timedSample, 1001)
    set(timedSample, 1002)
    set(timedSample, 1002)
  }

  private def set(timedSample: Statistic[Long], now: Long) = {
    clock.setNow(now)
    timedSample += 1
  }
}
