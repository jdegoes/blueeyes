package blueeyes.health.metrics

import blueeyes.json._
import blueeyes.json.JParser.parse
import org.specs2.mutable.Specification
import blueeyes.akka_testing.FutureMatchers

class TimedEternityAverageStatSpec extends Specification with TimedStatFixtures with FutureMatchers {
  "TimedEternityAverageStat" should{
    "creates JValue" in{
      val timedSample = TimedAverageStat(eternity)
      fill(timedSample)

      val histogramValue = parse("[4.0]")
      val future = timedSample.toJValue.map(_.renderCanonical)
      val expected = (JObject(JField("perSecond", JObject(JField(eternity.toString, histogramValue) :: Nil)) :: Nil)).renderCanonical
      future must whenDelivered (be_==(expected))
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
