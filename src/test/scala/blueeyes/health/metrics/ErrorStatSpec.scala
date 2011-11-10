package blueeyes.health.metrics

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import org.specs2.matcher.MustThrownMatchers

class ErrorStatSpec extends Specification with MustThrownMatchers{
  "counts errors" in{
    val stats = new ErrorStat()

    stats += new NullPointerException()
    stats += new NullPointerException()

    stats.count mustEqual (2)
  }
  "creates details" in{
    val stats = new ErrorStat()

    stats += new NullPointerException()
    stats += new NullPointerException()
    stats += new RuntimeException()

    stats.details.get(classOf[NullPointerException]).get mustEqual (2)
    stats.details.get(classOf[RuntimeException]).get mustEqual (1)
  }

  "composes ErrorStat" in{
    val stats = new ErrorStat()

    stats += new NullPointerException()
    stats += new NullPointerException()

    stats.toJValue mustEqual (JObject(JField("errorCount", JInt(2)) :: JField("errorDistribution", JObject(JField(classOf[NullPointerException].getName, JInt(2)) :: Nil)) :: Nil))
  }
}