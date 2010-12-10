package blueeyes.health.metrics

import org.spex.Specification
import blueeyes.json.JsonAST.{JInt, JField, JObject}

class ErrorStatSpec extends Specification{
  "counts errors" in{
    val stats = new ErrorStat()

    stats += new NullPointerException()
    stats += new NullPointerException()

    stats.count mustEqual (2)
  }
  "counts JValue" in{
    val stats = new ErrorStat()

    stats += new NullPointerException()
    stats += new NullPointerException()

    stats.toJValue mustEqual (JObject(JField("errorCount", JInt(2)) :: JField("errorDistribution", JObject(JField(classOf[NullPointerException].getName, JInt(2)))) :: Nil))
  }
  "creates distribution" in{
    val stats = new ErrorStat()

    stats += new NullPointerException()
    stats += new NullPointerException()
    stats += new RuntimeException()

    stats.distribution.get(classOf[NullPointerException]).get mustEqual (2)
    stats.distribution.get(classOf[RuntimeException]).get mustEqual (1)
  }
}