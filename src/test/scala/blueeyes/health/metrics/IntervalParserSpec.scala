package blueeyes.health.metrics

import org.specs2.mutable.Specification
import org.specs2.matcher.MustThrownMatchers
import java.util.concurrent.TimeUnit

class IntervalParserSpec extends Specification with MustThrownMatchers{
  "IntervalParser" should{
    "parse 'seconds' interval" in{
      IntervalParser.parse("30s x 10") mustEqual (interval(IntervalLength(30, TimeUnit.SECONDS), 10))
      IntervalParser.parse("3 s x 10") mustEqual (interval(IntervalLength(3, TimeUnit.SECONDS), 10))
    }
    "parse 'minutes' interval" in{
      IntervalParser.parse("30min x 10") mustEqual (interval(IntervalLength(30, TimeUnit.MINUTES), 10))
      IntervalParser.parse("3 min x 10") mustEqual (interval(IntervalLength(3, TimeUnit.MINUTES), 10))
    }
    "parse 'hours' interval" in{
      IntervalParser.parse("3h x 10") mustEqual (interval(IntervalLength(3, TimeUnit.HOURS), 10))
      IntervalParser.parse("30 h x 10") mustEqual (interval(IntervalLength(30, TimeUnit.HOURS), 10))
    }
    "parse 'eternity' interval" in{
      IntervalParser.parse("eternity") mustEqual (eternity)
    }
  }
}