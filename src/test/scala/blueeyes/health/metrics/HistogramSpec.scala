package blueeyes.health.metrics

import org.spex.Specification

class HistogramSpec extends Specification{
  "builds Histogram" in{
    val histogram = new HistogramImpl(Map(1.2 -> 2, 1.5 -> 2, 5.9 -> 1, 12.1 -> 3)).build

    histogram mustEqual(Map(1 -> 4, 4 -> 1, 7 -> 0, 10-> 3))
  }
}

class HistogramImpl(val rawData: Map[Double, Int]) extends Histogram