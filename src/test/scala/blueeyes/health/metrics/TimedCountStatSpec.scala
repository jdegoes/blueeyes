package blueeyes.health.metrics

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import java.util.concurrent.TimeUnit

class TimedCountStatSpec extends Specification{
  private val clock = new Clock()
  "TimedCountStat" should{
    "creates JValue" in{
      val config      = interval(IntervalLength(3, TimeUnit.SECONDS), 3)
      val timedSample = TimedCountStat(config)(clock.now _)
      fill(timedSample)

      val jValue = timedSample.toJValue
      jValue.value must eventually (beSome(JObject(JField(config.toString, (JArray(List(JInt(4), JInt(3), JInt(0))))) :: Nil)))
    }
    "creates TimedSample if the configuration is interval" in{
      TimedCountStat(interval(IntervalLength(3, TimeUnit.SECONDS), 7))(clock.now _).isInstanceOf[TimedSample[_]] must be_==(true)
    }
    "creates EternityTimedSample if the configuration is eternity" in{
      TimedCountStat(eternity)(clock.now _).isInstanceOf[EternityTimedNumbersSample] must be_==(true)
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
    set(timedSample, 118000)
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