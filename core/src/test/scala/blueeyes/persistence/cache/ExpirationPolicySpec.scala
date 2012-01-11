package blueeyes.persistence.cache

import org.specs2.mutable.Specification
import java.util.concurrent.TimeUnit.{NANOSECONDS, MILLISECONDS}

import blueeyes.util.ClockMock

class ExpirationPolicySpec extends Specification {
  val TimeToIdlePolicy = ExpirationPolicy(timeToIdle = Some(1), timeToLive = None, timeUnit = NANOSECONDS)
  val TimeToLivePolicy = ExpirationPolicy(timeToLive = Some(1), timeToIdle = None, timeUnit = NANOSECONDS)
  val TimeToAllPolicy  = ExpirationPolicy(timeToLive = Some(1), timeToIdle = Some(1), timeUnit = NANOSECONDS)
  val TimeToNonePolicy = ExpirationPolicy(timeToLive = None, timeToIdle = None, timeUnit = NANOSECONDS)  

  "eternal" should {
    "be true when timeToIdleNanos and timeToLiveNanos are not defined" in {
      ExpirationPolicy(None, None, NANOSECONDS).eternal must be_==(true)
    }
    "ExpirationPolicy: eternal is 'false' when timeToIdleNanos is defined" in {
      ExpirationPolicy(Some(1), None, NANOSECONDS).eternal must be_==(false)
    }
    "ExpirationPolicy: eternal is 'false' when timeToLiveNanos is defined" in {
      ExpirationPolicy(None, Some(1), NANOSECONDS).eternal must be_==(false)
    }
  }

  "timeToIdle" should {
    "" in {
      ExpirationPolicy(Some(1), None, MILLISECONDS).timeToIdle(NANOSECONDS) must beSome(1000000)
    }
  }

  "timeToLive" should {
    "convert" in {
      ExpirationPolicy(None, Some(1), MILLISECONDS).timeToLive(NANOSECONDS) must beSome(1000000)
    }
  }
  
  "isExpired" should {
    "identify an expired value (creationTime/timeToLive)" in {
      implicit val clockMock = ClockMock.newMockClock
      val value = ExpirableValue("foo", 2, NANOSECONDS)

      TimeToLivePolicy.isExpired(value, 4) must_== true
    }

    "identify an unexpired value (creationTime/timeToLive)" in {
      implicit val clockMock = ClockMock.newMockClock
      val value = ExpirableValue("foo", 2, NANOSECONDS)

      TimeToLivePolicy.isExpired(value, 2) must_== false
    }

    "identify an expired value (accessTime/timeToIdle)" in {
      implicit val clockMock = ClockMock.newMockClock
      val value = ExpirableValue("foo", 0, NANOSECONDS)

      clockMock.setNanoTime(2L)

      value.value

      TimeToIdlePolicy.isExpired(value, 4) must_== true
    }
    
    "identify an unexpired value (accessTime/timeToIdle)" in {
      implicit val clockMock = ClockMock.newMockClock
      val value = ExpirableValue("foo", 0, NANOSECONDS)

      value.value

      TimeToIdlePolicy.isExpired(value, 1) must_== false
    }
  }
}
