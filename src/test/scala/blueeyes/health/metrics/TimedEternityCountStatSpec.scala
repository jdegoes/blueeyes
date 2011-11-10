package blueeyes.health.metrics

import blueeyes.json.JsonAST._
import org.specs2.mutable.Specification
import org.specs2.matcher.MustThrownMatchers

class TimedEternityCountStatSpec extends Specification with MustThrownMatchers{
  private val clock = new Clock()
  "EternityTimedCountStat" should{
    "creates JValue" in{
      val timedSample = TimedCountStat(eternity)(clock.now _)
      fill(timedSample)


      timedSample.toJValue.value must eventually (beSome(JObject(JField(eternity.toString, JArray(List(JInt(4)))) :: Nil)))
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

  class Clock{
    private var _now: Long = 0

    def now() = _now

    def setNow(value: Long){_now = value}
  }
}