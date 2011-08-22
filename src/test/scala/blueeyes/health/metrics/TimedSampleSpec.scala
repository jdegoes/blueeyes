package blueeyes.health.metrics

import org.specs.Specification
import IntervalLength._
import blueeyes.json.JsonAST._
import blueeyes.json.Printer

class TimedSampleSpec extends Specification{
  private val clock = new Clock()
  "TimedSample" should{
    "create histogram" in{
      val timedSample = new TimedSample(interval(3.seconds, 7))(clock.now _)
      fill(timedSample)

      val histogram = timedSample.details
      histogram mustEqual(Map(94000 -> 0, 97000 -> 0, 100000 -> 4, 103000 -> 3, 106000 -> 0, 109000 -> 0, 112000 -> 6, 115000 -> 1))
    }
    "removes expired data" in{
      val timedSample = new TimedSample(interval(3.seconds, 3))(clock.now _)
      fill(timedSample)

      val histogram = timedSample.details
      histogram mustEqual(Map(106000 -> 0, 109000 -> 0, 112000 -> 6, 115000 -> 1))
    }
    "creates JValue" in{
      val timedSample = new TimedSample(interval(3.seconds, 3))(clock.now _)
      fill(timedSample)

      val histogram = timedSample.toJValue
      val histogramValue = JObject(List(JField("112000", JInt(2)), JField("115000", JInt(0)), JField("109000", JInt(0)), JField("106000", JInt(0))))
      histogram mustEqual (JObject(JField("interval", JObject(JField("length", JString(3.seconds.toString)) :: JField("count", JInt(3)) :: Nil)) :: JField("perSecond", histogramValue) :: Nil))
    }
    "creates TimedSample if the configuration is interval" in{
      TimedSample(interval(3.seconds, 7))(clock.now _).isInstanceOf[TimedSample] must be (true)
    }
    "creates EternityTimedSample if the configuration is eternity" in{
      TimedSample(eternity)(clock.now _).isInstanceOf[EternityTimedSample] must be (true)
    }
  }

  private def fill(timedSample: TimedSample){
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

  private def set(timedSample: TimedSample, now: Long) = {
    clock.setNow(now)
    timedSample += 1
  }

  class Clock{
    private var _now: Long = 0

    def now() = _now

    def setNow(value: Long){_now = value}
  }
}