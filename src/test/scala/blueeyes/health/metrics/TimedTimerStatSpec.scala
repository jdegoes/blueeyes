package blueeyes.health.metrics

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import blueeyes.json.Printer
import java.util.concurrent.TimeUnit

class TimedTimerStatSpec extends Specification{
  private val clock = new Clock()
  "TimedTimerStat" should{
    "creates JValue" in{
      val config = interval(IntervalLength(3, TimeUnit.SECONDS), 3)
      val timedSample = TimedTimerStat(config)(clock.now _)
      fill(timedSample)

      val values = ("minimumTime", List(JDouble(1.0E-6), JDouble(1.0E-6), JDouble(0.0))) :: ("maximumTime", List(JDouble(1.0E-6), JDouble(1.0E-6), JDouble(0.0))) :: ("averageTime", List(JDouble(1.0E-6), JDouble(1.0E-6), JDouble(0.0))) :: ("standardDeviation", List(JDouble(0.0), JDouble(0.0), JDouble(0.0))) :: Nil
      val jValue = timedSample.toJValue
      jValue.value must eventually (beSome(JObject(values.map(kv => JField(kv._1, JObject(JField(config.toString, JArray(kv._2)) :: Nil))))))
    }
    "creates TimedSample if the configuration is interval" in{
      TimedTimerStat(interval(IntervalLength(3, TimeUnit.SECONDS), 7))(clock.now _).isInstanceOf[TimedSample[_]] must be_==(true)
    }
    "creates EternityTimedSample if the configuration is eternity" in{
      TimedTimerStat(eternity)(clock.now _).isInstanceOf[EternityTimedTimersSample] must be_==(true)
    }
  }

  private def fill(timedSample: Statistic[Long, Map[Long, Timer]]){
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

  private def set(timedSample: Statistic[Long, Map[Long, Timer]], now: Long) = {
    clock.setNow(now)
    timedSample += 1

    Thread.sleep(50)
  }

  class Clock{
    private var _now: Long = 0

    def now() = _now

    def setNow(value: Long){_now = value}
  }
}