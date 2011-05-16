package blueeyes.util

import blueeyes.concurrent.Future
import org.joda.time.{DateTime, DateTimeZone, Period}

trait Clock {
  /** Returns the current time.
   */
  def now(): DateTime

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

trait ClockSystem {
  implicit val clockSystem = new Clock {
    def now(): DateTime = new DateTime(DateTimeZone.UTC)

    def nanoTime(): Long = System.nanoTime()
  }
}
object ClockSystem extends ClockSystem

trait ClockMock {
  protected class MockClock extends Clock {
    private var _now: DateTime = new DateTime(0, DateTimeZone.UTC)
    private var _nanoTime: Long = 0
    
    def now() = _now 

    def nanoTime() = _nanoTime
    
    def now_=(dateTime: DateTime): DateTime = { _now = dateTime; _now }
    
    def now_=(millis: Long): DateTime = new DateTime(millis, DateTimeZone.UTC)

    def nanoTime_=(time: Long): Long = { _nanoTime = time; _nanoTime }
  }

  implicit val clockMock: MockClock = new MockClock
}
object ClockMock extends ClockMock