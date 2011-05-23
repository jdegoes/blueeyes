package blueeyes.health.metrics

import blueeyes.concurrent.{Future, FutureDeliveryStrategySequential}
import blueeyes.util.metrics.Duration
import Duration._
import org.specs.Specification

class TimerTest extends Specification with FutureDeliveryStrategySequential {
  val precision = 5.0 // milliseconds

  "timing an event" should {
    "returns the event's value" in {
      val timer = new Timer
      timer.time { 1 + 1 } mustEqual(2)
    }

    "records the duration of the event" in {
      val timer = new Timer
      timer.time { Thread.sleep(10) }
      timer.mean.ms.time mustNotBe(0.0)
    }

    "records the duration of the event specified by future" in {
      val timer  = new Timer
      val future = new Future[Unit]()

      timer.time(future)

      Thread.sleep(10)
      future.deliver(())

      timer.mean.ms.time mustNotBe(0.0)
    }

    "records the existence of the event" in {
      val timer = new Timer
      timer.time { Thread.sleep(10) }

      timer.count mustEqual(1)
    }
  }

  "a blank timer" should {
    val timer = new Timer

    "has a max of zero" in {
      timer.max.ms.time must beCloseTo(0l, precision)
    }

    "has a min of zero" in {
      timer.min.ms.time must beCloseTo(0l, precision)
    }

    "has a mean of zero" in {
      timer.mean.ms.time must beCloseTo(0l, precision)
    }

    "has a standard deviation of zero" in {
      timer.standardDeviation.ms.time must beCloseTo(0l, precision)
    }

    "has a count of zero" in {
      timer.count mustEqual (0)
    }
  }

  "timing a series of events" should {
    val timer = new Timer
    timer ++= List(
      10.milliseconds,
      20.milliseconds,
      20.milliseconds,
      30.milliseconds,
      40.milliseconds
    )

    "calculates the maximum duration" in {
      timer.max.ms.time must beCloseTo(40.0, precision)
    }

    "calculates the minimum duration" in {
      timer.min.ms.time must beCloseTo(10.0, precision)
    }

    "calculates the mean" in {
      timer.mean.ms.time must beCloseTo(24.0, precision)
    }

    "calculates the standard deviation" in {
      timer.standardDeviation.ms.time must beCloseTo(11.4, precision)
    }

    "records the count" in {
      timer.count mustEqual (5)
    }
  }

  "timing crazy-variant values" should {
    val timer = new Timer
    timer ++= List(
      Long.MaxValue.milliseconds,
      0.milliseconds
    )

    "calculates the standard deviation without overflowing" in {
      timer.standardDeviation.ms.time must beCloseTo(6.521908912666392E12, 1E3)
    }
  }
}