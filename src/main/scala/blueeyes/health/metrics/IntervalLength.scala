package blueeyes.health.metrics

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import blueeyes.util.ProductPrefixUnmangler
import java.util.concurrent.TimeUnit

case class IntervalLength(length: Int, unit: TimeUnit){
  require(unit == TimeUnit.SECONDS || unit == TimeUnit.MINUTES || unit == TimeUnit.HOURS, "Interval Length can be either SECONDS or MINUTES or HOURS.")
  require(length > 0, "Interval Length must be more then 0.")

  private def abbreviate(u: TimeUnit) = {
    u match {
      case TimeUnit.SECONDS => "s"
      case TimeUnit.MINUTES => "min"
      case TimeUnit.HOURS => "h"
    }
  }

  override def toString = "%s%s".format(length.toString, abbreviate(unit))
}

object IntervalLength {
  class ToIntervalLength(length: Int) {
    def seconds      = IntervalLength(length, TimeUnit.SECONDS)
    def minutes      = IntervalLength(length, TimeUnit.MINUTES)
    def hours        = IntervalLength(length, TimeUnit.HOURS)
  }

  implicit def toIntervalLength(length: Int): ToIntervalLength = new ToIntervalLength(length)
}

object IntervalLengthParser extends RegexParsers{
  private def parser: Parser[Option[IntervalLength]] =
  (
    digitChar <~ (" "?) <~ "seconds" ^^ {case v => IntervalLength(v, TimeUnit.SECONDS)} |
    digitChar <~ (" "?) <~ "minutes" ^^ {case v => IntervalLength(v, TimeUnit.MINUTES)} |
    digitChar <~ (" "?) <~ "hours"   ^^ {case v => IntervalLength(v, TimeUnit.HOURS)}
  )?

  def parse(inString: String) = parser(new CharSequenceReader(inString)) match {
    case Success(result, _) => result

    case Failure(msg, _) => sys.error("The IntervalLength " + inString + " has a syntax error: " + msg)

    case Error(msg, _) => sys.error("There was an error parsing \"" + inString + "\": " + msg)
  }

  private def digitChar = regex("[1-9]+".r) ^^ (v => v.toInt)
}