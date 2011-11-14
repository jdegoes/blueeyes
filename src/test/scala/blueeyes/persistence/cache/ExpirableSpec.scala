package blueeyes.persistence.cache

import org.specs2.mutable.Specification
import java.util.concurrent.TimeUnit.{NANOSECONDS, MILLISECONDS}
import java.lang.System.{nanoTime}

class ExpirableSpec extends Specification{
  private val expirable = Expirable("foo", "bar", ExpirationPolicy(None, None, NANOSECONDS))

  "Expirable: records access time" in{

    val lower = nanoTime()
    expirable.value
    val upper = nanoTime()

    expirable.accessTime(NANOSECONDS) must be_>= (lower)
    expirable.accessTime(NANOSECONDS) must be_<= (upper)
  }

  "Expirable: can convert access time" in{
    expirable.accessTime(MILLISECONDS) mustEqual(MILLISECONDS.convert(expirable.accessTimeNanos, NANOSECONDS))
  }
  "Expirable: can convert creation time" in{
    expirable.creationTime(MILLISECONDS) mustEqual(MILLISECONDS.convert(expirable.creationTimeNanos, NANOSECONDS))
  }
}