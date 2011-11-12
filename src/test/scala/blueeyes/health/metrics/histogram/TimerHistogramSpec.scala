package blueeyes.health.metrics.histogram

import blueeyes.util.metrics.Duration._
import org.specs2.mutable.Specification

import ValueStrategy._
import blueeyes.health.metrics.Timer

class TimerHistogramSpec extends Specification{
  "TimerHistogram" should{
    "create Timer Histogram" in{
      val histogram = new StaticHistogram[Timer](new DynamicLengthBucketsStrategy()).histogram(Map(1.2 -> 2l, 1.5 -> 2l, 5.9 -> 1l, 12.1 -> 3l).toList.sortWith((e1, e2) => (e1._1 < e2._1)))
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
}