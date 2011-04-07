package blueeyes.health.metrics

import blueeyes.health.time.Duration
import blueeyes.concurrent.{Future, FutureDeliveryStrategySequential}
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
      timer.mean.ms.value mustNotBe(0.0)
    }

    "records the duration of the event specified by future" in {
      val timer  = new Timer
      val future = new Future[Unit]()

      timer.time(future)

      Thread.sleep(10)
      future.deliver(())

      timer.mean.ms.value mustNotBe(0.0)
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
      timer.max.ms.value must beCloseTo(0.0, precision)
    }

    "has a min of zero" in {
      timer.min.ms.value must beCloseTo(0.0, precision)
    }

    "has a mean of zero" in {
      timer.mean.ms.value must beCloseTo(0.0, precision)
    }

    "has a standard deviation of zero" in {
      timer.standardDeviation.ms.value must beCloseTo(0.0, precision)
    }

    "has a count of zero" in {
      timer.count mustEqual (0)
    }
  }

  "timing a series of events" should {
    val timer = new Timer
    timer ++= List(
      Duration.milliseconds(10),
      Duration.milliseconds(20),
      Duration.milliseconds(20),
      Duration.milliseconds(30),
      Duration.milliseconds(40)
    )

    "calculates the maximum duration" in {
      timer.max.ms.value must beCloseTo(40.0, precision)
    }

    "calculates the minimum duration" in {
      timer.min.ms.value must beCloseTo(10.0, precision)
    }

    "calculates the mean" in {
      timer.mean.ms.value must beCloseTo(24.0, precision)
    }

    "calculates the standard deviation" in {
      timer.standardDeviation.ms.value must beCloseTo(11.4, precision)
    }

    "records the count" in {
      timer.count mustEqual (5)
    }
  }

  "timing crazy-variant values" should {
    val timer = new Timer
    timer ++= List(
      Duration.milliseconds(Long.MaxValue),
      Duration.milliseconds(0)
    )

    "calculates the standard deviation without overflowing" in {
      timer.standardDeviation.ms.value must beCloseTo(6.521908912666392E12, 1E3)
    }
  }
}