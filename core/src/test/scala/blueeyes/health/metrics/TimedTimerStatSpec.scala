package blueeyes.health.metrics

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import java.util.concurrent.TimeUnit

class TimedTimerStatSpec extends Specification with TimedStatFixtures with blueeyes.concurrent.test.FutureMatchers {
  "TimedTimerStat" should{
    "creates JValue" in{
      val config = interval(IntervalLength(3, TimeUnit.SECONDS), 3)
      val timedSample = TimedTimerStat(config)
      fill(timedSample)

      val values = ("minimumTime", List(JNum(1.0E-6), JNum(1.0E-6), JNum(0.0))) :: ("maximumTime", List(JNum(1.0E-6), JNum(1.0E-6), JNum(0.0))) :: ("averageTime", List(JNum(1.0E-6), JNum(1.0E-6), JNum(0.0))) :: ("standardDeviation", List(JNum(0.0), JNum(0.0), JNum(0.0))) :: Nil
      val jValue = timedSample.toJValue
      jValue must whenDelivered (be_==(JObject(values.map(kv => JField(kv._1, JObject(JField(config.toString, JArray(kv._2)) :: Nil))))))
    }

    "creates TimedSample if the configuration is interval" in{
      TimedTimerStat(interval(IntervalLength(3, TimeUnit.SECONDS), 7)) must beAnInstanceOf[TimedSample[_]] 
    }

    "creates EternityTimedSample if the configuration is eternity" in{
      TimedTimerStat(eternity) must beAnInstanceOf[EternityTimedTimersSample] 
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
}
