package blueeyes.health.metrics

import org.specs.Specification

class HistogramSpec extends Specification{
  "builds Histogram" in{
    class HistogramImpl(val rawData: Map[Double, Long]) extends SimpleHistogram

    val histogram = new HistogramImpl(Map(1.2 -> 2, 1.5 -> 2, 5.9 -> 1, 12.1 -> 3)).build
    histogram mustEqual(Map(1 -> 4, 4 -> 1, 7 -> 0, 10-> 3))
  }

  "builds Histogram with predified bucket size" in{
    class HistogramImpl(val rawData: Map[Double, Long]) extends SimpleHistogram{
      override protected def bucket(sorted: List[(Double, Long)]) = 3
    }

    val histogram = new HistogramImpl(Map(100.0 -> 0, 102.0 -> 2, 103.0 -> 3, 104.0 -> 2, 112.0 -> 3, 114.0 -> 4)).build
    histogram mustEqual(Map(100 -> 2, 103 -> 5, 106 -> 0, 109 -> 0, 112-> 7))
  }
}

