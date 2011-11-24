package blueeyes.health.metrics

import blueeyes.json.JsonAST._
import org.specs2.mutable.Specification

class TimedEternityAverageStatSpec extends Specification with TimedStatFixtures {
  "TimedEternityAverageStat" should{
    "creates JValue" in{
      val timedSample = TimedAverageStat(eternity)
      fill(timedSample)

      val histogramValue = JArray(List(JDouble(4)))
      timedSample.toJValue.value must eventually (beSome(JObject(JField("perSecond", JObject(JField(eternity.toString, histogramValue) :: Nil)) :: Nil)))
    }
  }

  private def fill(timedSample: Statistic[Long, Map[Long, Double]]){
    set(timedSample, 1001)
    set(timedSample, 1001)
    set(timedSample, 1002)
    set(timedSample, 1002)
  }

  private def set(timedSample: Statistic[Long, Map[Long, Double]], now: Long) = {
    clock.setNow(now)
    timedSample += 1
  }
}
