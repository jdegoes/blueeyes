package blueeyes.util

import blueeyes.concurrent.Future
import org.joda.time.{DateTime, DateTimeZone, Period, Instant}

trait Clock {
  /** Returns the current time.
   */
  def now(): DateTime

  def instant(): Instant

  def nanoTime(): Long
  
  /** Times how long the specified future takes to be delivered.
   */
  def time[T](f: => Future[T]): Future[(Period, T)] = {
    val start = now()
    
    f.map { result =>
      val end = now()
      
      (new Period(start, end), result)
    }
  }
  
  /** Times a block of code.
   */
  def timeBlock[T](f: => T): (Period, T) = {
    val start = now()
    
    val result = f
    
    val end = now()
    
    (new Period(start, end), result)
  }
}

object Clock {
  val System = ClockSystem.realtimeClock
}

trait ClockSystem {
  implicit val realtimeClock = new Clock {
    def now(): DateTime = new DateTime(DateTimeZone.UTC)

    def instant(): Instant = new Instant()

    def nanoTime(): Long = System.nanoTime()
  }
}
object ClockSystem extends ClockSystem

trait ClockMock {
  protected class MockClock extends Clock {
    private var _now: DateTime = new DateTime(0, DateTimeZone.UTC)
    private var _nanoTime: Long = 0
    
    def now() = _now 

    def instant() = _now.toInstant()

    def nanoTime() = _nanoTime
    
    def setNow(dateTime: DateTime): DateTime = { _now = dateTime; _now }
    
    def setNow(millis: Long): DateTime = new DateTime(millis, DateTimeZone.UTC)

    def setNanoTime(time: Long): Long = { _nanoTime = time; _nanoTime }
  }

  def newMockClock = new MockClock

  implicit val clockMock: MockClock = new MockClock
}
object ClockMock extends ClockMock
