package blueeyes.persistence.cache

import org.specs.Specification
import java.util.concurrent.TimeUnit.{MILLISECONDS}

class ExpirationPredicateSpec extends Specification{
  private val predicate = new ExpirationPredicate[String, String]()

  "ExpirationPredicate: 'false' when policy is 'eternal'" in{
    val expirable = Expirable("foo", "bar", ExpirationPolicy(None, None, MILLISECONDS))

    predicate(expirable) must eventually(be (false))
  }
  "ExpirationPredicate: 'true' when Idle time is expired" in{
    val expirable = Expirable("foo", "bar", ExpirationPolicy(Some(1), None, MILLISECONDS))

    predicate(expirable) must eventually(be (true))
  }
  "ExpirationPredicate: 'true' when live time is expired" in{
    val expirable = Expirable("foo", "bar", ExpirationPolicy(None, Some(1), MILLISECONDS))

    predicate(expirable) must eventually(be (true))
  }
}