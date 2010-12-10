package blueeyes.health.metrics

import org.spex.Specification

class SampleSpec extends Specification{
  private val sample = new Sample(10)
  "stores raw data" in{
    sample += 1.1
    sample += 2.2
    sample += 2.2

    sample.count                mustEqual (3)
    sample.rawData.size         mustEqual (2)
    sample.rawData.get(1.1).get mustEqual (1)
    sample.rawData.get(2.2).get mustEqual (2)
  }
  "does not add new data when size is exceeded" in{
    val sample = new Sample(1)
    sample += 1.1
    sample += 2.2

    sample.count                mustEqual (1)
  }
  "does not add create Histogram when data is not full" in{
    val sample = new Sample(2)
    sample += 1.1

    sample.histogram            mustEqual (None)
  }
  "create Histogram when data is full" in{
    val sample = new Sample(2)
    sample += 1.1
    sample += 2.2

    sample.histogram            mustNotEq (None)
  }
}