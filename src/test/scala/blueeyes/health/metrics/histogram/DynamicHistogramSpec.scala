package blueeyes.health.metrics.histogram

import org.specs2.mutable.Specification
import ValueStrategy._
import java.util.concurrent.TimeUnit

class DynamicHistogramSpec extends Specification{
  private val clock = new Clock()
  implicit private val clockF = clock.now _
  "DynamicHistogram" should{
    "build Histogram" in{
      val histogram = DynamicHistogram.empty[Long](3, 3, TimeUnit.SECONDS)

      val result    = set(histogram, 15500, 16600, 18100, 21100, 21201, 21310)

      result.histogram mustEqual(Map(15 -> 2, 18 -> 1, 21 -> 3))
    }

    "fill last missing buckets" in{
      val histogram = DynamicHistogram.empty[Long](3, 3, TimeUnit.SECONDS)

      val result    = set(histogram, 18100, 21100, 21201, 21310)
      result.histogram mustEqual(Map(15 -> 0, 18 -> 1, 21 -> 3))
    }

    "fill first missing buckets" in{
      val histogram = DynamicHistogram.empty[Long](3, 3, TimeUnit.SECONDS)

      val result    = set(histogram, 15500, 16600, 18100)
      clock.setNow(21100)
      result.histogram mustEqual(Map(15 -> 2, 18 -> 1, 21 -> 0))
    }

    "remove expired buckets" in{
      val histogram = DynamicHistogram.empty[Long](3, 3, TimeUnit.SECONDS)

      val result    = set(histogram, 12100, 15500, 16600, 18100, 21100, 21201, 21310)

      result.histogram mustEqual(Map(15 -> 2, 18 -> 1, 21 -> 3))
    }
  }


  private def set(histogram: DynamicHistogram[Long], ms: Long*) = {
    ms.foldLeft(histogram){ (histogram, now) =>
      clock.setNow(now)
      histogram += (now, 1)
    }
  }

  class Clock{
    private var _now: Long = 0

    def now() = _now

    def setNow(value: Long){_now = value}
  }
}