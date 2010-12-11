package blueeyes.core.service

import org.specs.Specification
import blueeyes.json.JsonAST._
import blueeyes.health.metrics._

class StatisticComposerSpec extends Specification{
  private val composer = new StatisticComposer{}

  "composes Counter" in{
    val counter = new Counter(0)
    counter.inc(2)

    composer.composeCounter(counter) mustEqual(JInt(2))
  }

  "composes ErrorStat" in{
    val stats = new ErrorStat()

    stats += new NullPointerException()
    stats += new NullPointerException()

    composer.composeErrorStat(stats) mustEqual (JObject(JField("errorCount", JInt(2)) :: JField("errorDistribution", JObject(JField(classOf[NullPointerException].getName, JInt(2)) :: Nil)) :: Nil))
  }

  "composes Sample" in{
    val sample = new Sample(1)
    sample += 1.1

    composer.composeSample(sample) mustEqual (JObject(JField("count", JInt(1)) :: JField("histogram", JObject(JField("1", JInt(1)) :: Nil)) :: Nil))
  }
//  "composes Timer" in{
//    val timer = new Timer
//    timer.time { 1 + 1 }
//
//    composer.composeTimer(timer) mustEqual (JObject(JField("minimumTime", JDouble(2.0)) :: JField("maximumTime", JDouble(2.0)) :: JField("averageTime", JDouble(2.0)) :: JField("standardDeviation", JDouble(0.0)) :: Nil))
//  }
}