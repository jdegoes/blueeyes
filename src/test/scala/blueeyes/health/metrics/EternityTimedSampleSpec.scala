package blueeyes.health.metrics

import blueeyes.json.JsonAST._
import org.specs.Specification
import blueeyes.json.Printer

class EternityTimedSampleSpec extends Specification{
  private val clock = new Clock()
  "EternityTimedSample" should{
    "creates JValue" in{
      val timedSample = new EternityTimedSample()(clock.now _)
      fill(timedSample)

      val histogram = timedSample.toJValue
      println(Printer.pretty(Printer.render(histogram)))
      val histogramValue = JObject(List(JField("0", JInt(4))))
      histogram mustEqual (JObject(JField("interval", JObject(JField("length", JString("eternity")) :: JField("count", JInt(1)) :: Nil)) :: JField("perSecond", histogramValue) :: Nil))
    }
  }

  private def fill(timedSample: EternityTimedSample){
    set(timedSample, 1001)
    set(timedSample, 1001)
    set(timedSample, 1002)
    set(timedSample, 1002)
  }

  private def set(timedSample: EternityTimedSample, now: Long) = {
    clock.setNow(now)
    timedSample += 1
  }

  class Clock{
    private var _now: Long = 0

    def now() = _now

    def setNow(value: Long){_now = value}
  }
}