package blueeyes.health.metrics.histogram

import org.specs2.mutable.Specification
import ValueStrategy._

class StaticHistogramSpec extends Specification{
  "builds Histogram" in{
    val histogram = new StaticHistogram[Double](new DynamicLengthBucketsStrategy()).histogram(Map(1.2 -> 2l, 1.5 -> 2l, 5.9 -> 1l, 12.1 -> 3l).toList.sortWith((e1, e2) => (e1._1 < e2._1)))
    histogram mustEqual(Map(1 -> 4, 4 -> 1, 7 -> 0, 10-> 3))
  }

  "builds Histogram with predified bucket size" in{
    val histogram = new StaticHistogram[Double](new FixedLengthBucketsStrategy(3)).histogram(Map(100.0 -> 0l, 102.0 -> 2l, 103.0 -> 3l, 104.0 -> 2l, 112.0 -> 3l, 114.0 -> 4l).toList.sortWith((e1, e2) => (e1._1 < e2._1)))
    histogram mustEqual(Map(100 -> 2, 103 -> 5, 106 -> 0, 109 -> 0, 112-> 7))
  }
}

