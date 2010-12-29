package blueeyes.persistence.cache

import org.spex.Specification
import java.util.concurrent.TimeUnit.{NANOSECONDS}

class ExpirationTaskSpec extends Specification{
  private val expirable = Expirable("foo", "bar", ExpirationPolicy(None, None, NANOSECONDS))

  "ExpirationTask: expirable is not evicted when hasExpired is 'false'" in{
    var evicted = false

    new ExpirationTask(expirable, {hasExpired: Expirable[String, String] => true}, {evict: Expirable[String, String] => evicted = true}).run()

    evicted must be (true)
  }
  "ExpirationTask: expirable is evicted when hasExpired is 'true'" in{
    var evicted = false

    new ExpirationTask(expirable, {hasExpired: Expirable[String, String] => false}, {evict: Expirable[String, String] => evicted = true}).run()

    evicted must be (false)
  }
}