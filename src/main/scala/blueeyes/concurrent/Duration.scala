package blueeyes.concurrent

import java.util.concurrent.TimeUnit

case class Duration(time: Long, unit: TimeUnit)

object Duration {
  // So user can write: 23.milliseconds, 92.seconds, etc.
  implicit def longToDuration(value: Long) = new {
    def milliseconds = Duration(value, TimeUnit.MILLISECONDS)
    def nanoseconds  = Duration(value, TimeUnit.NANOSECONDS)
    def microseconds = Duration(value, TimeUnit.MICROSECONDS)
    def seconds      = Duration(value, TimeUnit.SECONDS)
    def minutes      = Duration(value, TimeUnit.MINUTES)
    def hours        = Duration(value, TimeUnit.HOURS)
    def days         = Duration(value, TimeUnit.DAYS)
  }
}