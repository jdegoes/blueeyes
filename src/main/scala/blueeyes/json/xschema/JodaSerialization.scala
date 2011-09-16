package blueeyes.json
package xschema

import JsonAST._
import DefaultSerialization._
import org.joda.time.{Instant, DateTime, DateTimeZone}

trait JodaSerializationImplicits {
  implicit val InstantExtractor = new Extractor[Instant] {
    def extract(jvalue: JValue): Instant = new Instant(jvalue.deserialize[Long])
  }

  implicit val InstantDecomposer = new Decomposer[Instant] {
    def decompose(dateTime: Instant): JValue = JInt(dateTime.getMillis)
  }

  implicit val DateTimeExtractor = new Extractor[DateTime] {
    def extract(jvalue: JValue): DateTime = jvalue match {
      case JInt(instant)  => new DateTime(instant.longValue, DateTimeZone.UTC)
      case JDouble(d)     => new DateTime(d.toLong, DateTimeZone.UTC)
      case JString(text)  => new DateTime(text)
      case x => sys.error("Unexpected time format: " + x + "; was anticipating integral millisecond value or text string.")
    }
  }

  implicit val DateTimeDecomposer = new Decomposer[DateTime] {
    def decompose(dateTime: DateTime): JValue = JInt(dateTime.getMillis)
  }
}

object JodaSerializationImplicits extends JodaSerializationImplicits

// vim: set ts=4 sw=4 et:
