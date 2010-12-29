package blueeyes.persistence.cache

import org.spex.Specification
import java.util.concurrent.TimeUnit.{MILLISECONDS}

class ExpirationPredicateSpec extends Specification{
  private val predicate = new ExpirationPredicate[String, String]()

  "ExpirationPredicate: 'false' when policy is 'eternal'" in{
    val expirable = Expirable("foo", "bar", ExpirationPolicy(None, None, MILLISECONDS))

    predicate(expirable) must be (false)
  }
  "ExpirationPredicate: 'true' when Idle time is expired" in{
    val expirable = Expirable("foo", "bar", ExpirationPolicy(Some(1), None, MILLISECONDS))

    Thread.sleep(2)

    predicate(expirable) must be (true)
  }
  "ExpirationPredicate: 'true' when live time is expired" in{
    val expirable = Expirable("foo", "bar", ExpirationPolicy(None, Some(1), MILLISECONDS))

    Thread.sleep(2)

    predicate(expirable) must be (true)
  }
}