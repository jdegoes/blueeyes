package blueeyes.health.metrics

import org.joda.time.DateTime
import org.joda.time.Instant

trait TimedStatFixtures {
  protected implicit val clock = new TestClock

  class TestClock extends blueeyes.util.Clock {
    private var _now: Long = 0

    def now() = new DateTime(_now)
    def instant() = now().toInstant
    def nanoTime() = sys.error("Not required for test.")

    def setNow(value: Long) {
      _now = value
    }
  }
}

// vim: set ts=4 sw=4 et:
