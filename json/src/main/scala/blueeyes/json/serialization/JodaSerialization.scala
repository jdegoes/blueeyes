package blueeyes.json
package serialization

import DefaultSerialization._
import Extractor._
import org.joda.time.{Instant, DateTime, DateTimeZone, Duration}

trait JodaSerializationImplicits {
  implicit val InstantExtractor = LongExtractor map { l => new Instant(l) }

  implicit val InstantDecomposer = new Decomposer[Instant] {
    def decompose(dateTime: Instant): JValue = JNum(dateTime.getMillis)
  }

  implicit val DurationExtractor = LongExtractor map { l => new Duration(l) }

  implicit val DurationDecomposer = new Decomposer[Duration] {
    def decompose(duration: Duration): JValue = JNum(duration.getMillis)
  }

  def ZonedTimeExtractor(zone: DateTimeZone): Extractor[DateTime] = new Extractor[DateTime] {
    override def validated(jvalue: JValue) = jvalue match {
      case JNum(instant)  => 
        LongExtractor.validated(jvalue) map { millis => new DateTime(millis, zone) }

      case JString(text)  => 
        tryv(new DateTime(text, zone))

      case x => invalidv("Unexpected time format: " + x + "; was anticipating integral millisecond value or text string.")
    }
  }

  implicit val DateTimeDecomposer = new Decomposer[DateTime] {
    def decompose(dateTime: DateTime): JValue = JNum(dateTime.getMillis)
  }
}

object JodaSerializationImplicits extends JodaSerializationImplicits

// vim: set ts=4 sw=4 et:
