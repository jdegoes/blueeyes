package blueeyes.health.metrics

import blueeyes.util.metrics.Duration._
import org.specs.Specification

class TimerHistogramSpec extends Specification{
  "TimerHistogram" should{
    "create Timer Histogram" in{
      val histogram = new HistogramImpl(Map(1.2 -> 2, 1.5 -> 2, 5.9 -> 1, 12.1 -> 3)).build
      histogram.get(1).get.count mustEqual(2)
      histogram.get(1).get.min mustEqual(2.nanoseconds)

      histogram.get(4).get.count mustEqual(1)
      histogram.get(4).get.min mustEqual(1.nanoseconds)

      histogram.get(7).get.count mustEqual(0)
      histogram.get(7).get.min mustEqual(0.nanoseconds)

      histogram.get(10).get.count mustEqual(1)
      histogram.get(10).get.min mustEqual(3.nanoseconds)
    }
  }
  class HistogramImpl(val rawData: Map[Double, Long]) extends TimerHistogram
}