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
    def extract(jvalue: JValue): DateTime = new DateTime(jvalue.deserialize[Long], DateTimeZone.UTC)
  }

  implicit val DateTimeDecomposer = new Decomposer[DateTime] {
    def decompose(dateTime: DateTime): JValue = JInt(dateTime.getMillis)
  }
}

object JodaSerializationImplicits extends JodaSerializationImplicits

// vim: set ts=4 sw=4 et:
