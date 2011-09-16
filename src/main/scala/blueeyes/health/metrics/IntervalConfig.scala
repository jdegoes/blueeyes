package blueeyes.health.metrics

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import java.util.concurrent.TimeUnit

sealed trait IntervalConfig

case object eternity extends IntervalConfig

case class interval(granularity: IntervalLength, samples: Int) extends IntervalConfig{
  require(samples > 0 && samples < 1000, "Interval samples must be less then 1000 and more then 0.")

  override def toString = granularity.toString + " x " + samples
}

object IntervalParser extends RegexParsers{
  private def parser: Parser[IntervalConfig] =
    (intervalLength <~ separator) ~ digitChar ^^ {case granularity~samples => interval(granularity, samples)} |
    "eternity" ^^^ eternity


  def parse(inString: String) = parser(new CharSequenceReader(inString)) match {
    case Success(result, _) => result

    case Failure(msg, _) => sys.error("The granularity " + inString + " has a syntax error: " + msg)

    case Error(msg, _) => sys.error("There was an error parsing \"" + inString + "\": " + msg)
  }

  private def separator = (" "?) ~ "x" ~ (" "?)

  private def intervalLength =  digitChar <~ (" "?) <~ "s" ^^ {case v => IntervalLength(v, TimeUnit.SECONDS)} |
    digitChar <~ (" "?) <~ "min" ^^ {case v => IntervalLength(v, TimeUnit.MINUTES)} |
    digitChar <~ (" "?) <~ "h"   ^^ {case v => IntervalLength(v, TimeUnit.HOURS)}

  private def digitChar = regex("[0-9]+".r) ^^ (v => v.toInt)
}