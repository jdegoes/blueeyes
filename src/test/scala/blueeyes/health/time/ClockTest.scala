package blueeyes.health.time

import org.specs.Specification

class ClockTest extends Specification {

  doAfter{
    Clock.unfreezeTime()
  }

  "the clock" should {
    "returns the current time in nanoseconds (with millisecond precision)" in {
      val before = System.nanoTime
      val time   = Clock.nanoTime
      val after  = System.nanoTime
      
      (time >= before) must be(true)
      (time <= after) must be(true)
    }
  }

  "a frozen clock" should {
    "returns a fixed number" in {
      Clock.freezeTime(100)
      Clock.nanoTime mustEqual(100)
    }
  }

  "a unfrozen clock" should {
    "returns the current time again" in {
      Clock.freezeTime(100)
      Clock.unfreezeTime()
      Clock.nanoTime must beCloseTo(System.nanoTime, 1e8.toLong)
    }
  }
}