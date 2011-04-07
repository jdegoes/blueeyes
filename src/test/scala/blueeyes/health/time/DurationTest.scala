package blueeyes.health.time

import java.util.concurrent.TimeUnit
import org.specs.Specification

class DurationTest extends Specification {
  val precision = 1e-5

  "a duration of three nanoseconds" should {
    val d = Duration.nanoseconds(3)

    "is in nanoseconds" in {
      d.value must beCloseTo(3.0, precision)
      d.unit mustEqual(TimeUnit.NANOSECONDS)
    }

    "is human-readable" in {
      d.toString mustEqual("3.00ns")
    }
  }

  "a duration of three microseconds" should {
    val d = Duration.microseconds(3)

    "is in microseconds" in {
      d.value must beCloseTo(3.0, precision)
      d.unit mustEqual(TimeUnit.MICROSECONDS)
    }

    "is human-readable" in {
      d.toString mustEqual("3.00us")
    }
  }

  "a duration of three milliseconds" should {
    val d = Duration.milliseconds(3)

    "is in milliseconds" in {
      d.value must beCloseTo(3.0, precision)
      d.unit mustEqual(TimeUnit.MILLISECONDS)
    }

    "is human-readable" in {
      d.toString mustEqual("3.00ms")
    }
  }

  "a duration of three seconds" should {
    val d = Duration.seconds(3)

    "is in seconds" in {
      d.value must beCloseTo(3.0, precision)
      d.unit mustEqual(TimeUnit.SECONDS)
    }

    "is human-readable" in {
      d.toString mustEqual("3.00s")
    }
  }

  "a duration of three minutes" should{
    val d = Duration.minutes(3)

    "is in minutes" in {
      d.value must beCloseTo(3.0, precision)
      d.unit mustEqual(TimeUnit.MINUTES)
    }

    "is equal to 0.002083 days" in {
      d.d.value must beCloseTo(0.002083, precision)
      d.d.unit mustEqual(TimeUnit.DAYS)
    }

    "is equal to 0.05 hours" in {
      d.h.value must beCloseTo(0.05, precision)
      d.h.unit mustEqual(TimeUnit.HOURS)
    }

    "is equal to 3 minutes" in {
      d.m.value must beCloseTo(3.0, precision)
      d.m.unit mustEqual(TimeUnit.MINUTES)
    }

    "is equal to 180 seconds" in {
      d.s.value must beCloseTo(180.0, precision)
      d.s.unit mustEqual(TimeUnit.SECONDS)
    }

    "is equal to 180,000 milliseconds" in {
      d.ms.value must beCloseTo(180000.0, precision)
      d.ms.unit mustEqual(TimeUnit.MILLISECONDS)
    }

    "is equal to 180,000,000 microseconds" in {
      d.us.value must beCloseTo(180000000.0, precision)
      d.us.unit mustEqual(TimeUnit.MICROSECONDS)
    }

    "is equal to 180,000,000,000 nanoseconds" in {
      d.ns.value must beCloseTo(180000000000.0, precision)
      d.ns.unit mustEqual(TimeUnit.NANOSECONDS)
    }

    "is human-readable" in {
      d.toString mustEqual("3.00min")
    }
  }

  "a duration of three hours" should {
    val d = Duration.hours(3)

    "is in hours" in {
      d.value must beCloseTo(3.0, precision)
      d.unit mustEqual(TimeUnit.HOURS)
    }

    "is human-readable" in {
      d.toString mustEqual("3.00h")
    }
  }

  "a duration of three days" should {
    val d = Duration.days(3)

    "is in days" in {
      d.value must beCloseTo(3.0, precision)
      d.unit mustEqual(TimeUnit.DAYS)
    }

    "is human-readable" in {
      d.toString mustEqual("3.00d")
    }
  }
}