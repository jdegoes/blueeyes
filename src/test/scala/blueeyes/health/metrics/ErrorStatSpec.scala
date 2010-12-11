package blueeyes.health.metrics

import org.spex.Specification

class ErrorStatSpec extends Specification{
  "counts errors" in{
    val stats = new ErrorStat()

    stats += new NullPointerException()
    stats += new NullPointerException()

    stats.count mustEqual (2)
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