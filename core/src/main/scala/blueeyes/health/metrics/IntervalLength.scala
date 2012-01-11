package blueeyes.health.metrics

import java.util.concurrent.TimeUnit

case class IntervalLength(length: Int, unit: TimeUnit){
  require(unit == TimeUnit.SECONDS || unit == TimeUnit.MINUTES || unit == TimeUnit.HOURS, "Interval granularity can be either SECONDS or MINUTES or HOURS.")
  require(length > 0, "Interval granularity must be more then 0.")

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