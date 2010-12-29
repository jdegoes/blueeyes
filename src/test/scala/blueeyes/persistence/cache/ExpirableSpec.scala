package blueeyes.persistence.cache

import org.spex.Specification
import java.util.concurrent.TimeUnit.{NANOSECONDS, MILLISECONDS}
import java.lang.System.{currentTimeMillis}

class ExpirableSpec extends Specification{
  private val expirable = Expirable("foo", "bar", ExpirationPolicy(None, None, NANOSECONDS))

  "Expirable: records access time" in{
    expirable.value

    expirable.accessTime(MILLISECONDS) must beCloseTo (currentTimeMillis(), 100)
  }

  "Expirable: can convert access time" in{
    expirable.accessTime(MILLISECONDS) mustEqual(MILLISECONDS.convert(expirable.accessTimeNanos, NANOSECONDS))
  }
  "Expirable: can convert creation time" in{
    expirable.creationTime(MILLISECONDS) mustEqual(MILLISECONDS.convert(expirable.creationTimeNanos, NANOSECONDS))
  }
}