package blueeyes.persistence.cache

import org.spex.Specification
import java.util.concurrent.TimeUnit.{NANOSECONDS, MILLISECONDS}

class ExpirationPolicySpec extends Specification{
  "ExpirationPolicy: eternal is 'true' when timeToIdleNanos and timeToLiveNanos are not defined" in {
    ExpirationPolicy(None, None, NANOSECONDS).eternal must be (true)  
  }
  "ExpirationPolicy: eternal is 'false' when timeToIdleNanos is defined" in {
    ExpirationPolicy(Some(1), None, NANOSECONDS).eternal must be (false)
  }
  "ExpirationPolicy: eternal is 'false' when timeToLiveNanos is defined" in {
    ExpirationPolicy(None, Some(1), NANOSECONDS).eternal must be (false)
  }
  "ExpirationPolicy: can convert timeToIdle" in {
    ExpirationPolicy(Some(1), None, MILLISECONDS).timeToIdle(NANOSECONDS) must beSome(1000000)
  }
  "ExpirationPolicy: can convert timeToIdle" in {
    ExpirationPolicy(None, Some(1), MILLISECONDS).timeToLive(NANOSECONDS) must beSome(1000000)
  }
}