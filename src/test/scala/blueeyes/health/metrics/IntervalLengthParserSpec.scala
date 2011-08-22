package blueeyes.health.metrics

import org.specs.Specification
import IntervalLength._

class IntervalLengthParserSpec extends Specification{
  "IntervalLengthParser" should{
    "parse 'seconds' interval" in{
      IntervalLengthParser.parse("3seconds") must beSome(3.seconds)
      IntervalLengthParser.parse("3 seconds") must beSome(3.seconds)
    }
    "parse 'minutes' interval" in{
      IntervalLengthParser.parse("3minutes") must beSome(3.minutes)
      IntervalLengthParser.parse("3 minutes") must beSome(3.minutes)
    }
    "parse 'hours' interval" in{
      IntervalLengthParser.parse("3hours") must beSome(3.hours)
      IntervalLengthParser.parse("3 hours") must beSome(3.hours)
    }
  }
}