package blueeyes.util

import org.joda.time.{DateTime, DateTimeZone}

trait Clock {
  def now(): DateTime
}

trait ClockSystem {
  implicit val clockSystem = new Clock {
    def now(): DateTime = new DateTime(DateTimeZone.UTC)
  }
}
object ClockSystem extends ClockSystem

trait ClockMock {
  implicit val clockMock = new Clock {
    private var _now: DateTime = new DateTime(0, DateTimeZone.UTC)
    
    def now() = _now
    
    def update(dateTime: DateTime): DateTime = { _now = dateTime; _now }
    
    def update(millis: Long): DateTime = new DateTime(millis, DateTimeZone.UTC)
  }
}
object ClockMock extends ClockMock