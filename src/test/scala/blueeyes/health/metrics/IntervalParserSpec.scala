package blueeyes.health.metrics

import org.specs.Specification
import IntervalLength._

class IntervalParserSpec extends Specification{
  "IntervalParser" should{
    "parse 'seconds' interval" in{
      IntervalParser.parse("30s x 10") mustEqual (interval(30.seconds, 10))
      IntervalParser.parse("3 s x 10") mustEqual (interval(3.seconds, 10))
    }
    "parse 'minutes' interval" in{
      IntervalParser.parse("30min x 10") mustEqual (interval(30.minutes, 10))
      IntervalParser.parse("3 min x 10") mustEqual (interval(3.minutes, 10))
    }
    "parse 'hours' interval" in{
      IntervalParser.parse("3h x 10") mustEqual (interval(3.hours, 10))
      IntervalParser.parse("30 h x 10") mustEqual (interval(30.hours, 10))
    }
    "parse 'eternity' interval" in{
      IntervalParser.parse("eternity") mustEqual (eternity)
    }
  }
}