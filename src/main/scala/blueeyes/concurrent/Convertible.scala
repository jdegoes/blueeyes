package blueeyes.concurrent

import java.util.concurrent.TimeUnit

trait Convertible[A] {
  /**
   * Returns the amount of time without units.
   */
  def time: Double

  /**
   * Returns the unit of the value.
   */
  def unit: TimeUnit

  /**
   * Convert to an arbitrary time unit.
   */
  def convert(u: TimeUnit): A

  /**
   * Returns the value in nanoseconds.
   */
  def nanoseconds = convert(TimeUnit.NANOSECONDS)
  def ns = nanoseconds

  /**
   * Returns the value in microseconds.
   */
  def microseconds = convert(TimeUnit.MICROSECONDS)
  def us = microseconds

  /**
   * Returns the value in milliseconds.
   */
  def milliseconds = convert(TimeUnit.MILLISECONDS)
  def ms = milliseconds

  /**
   * Returns the value in seconds.
   */
  def seconds = convert(TimeUnit.SECONDS)
  def s = seconds

  /**
   * Returns the value in minutes.
   */
  def minutes = convert(TimeUnit.MINUTES)
  def m = minutes

  /**
   * Returns the value in hours.
   */
  def hours = convert(TimeUnit.HOURS)
  def h = hours

  /**
   * Returns the value in days.
   */
  def days = convert(TimeUnit.DAYS)
  def d = days

  /**
   * Returns the SI abbreviate for the given unit.
   */
  protected def abbreviate(u: TimeUnit) = {
    u match {
      case TimeUnit.NANOSECONDS => "ns"
      case TimeUnit.MICROSECONDS => "us"
      case TimeUnit.MILLISECONDS => "ms"
      case TimeUnit.SECONDS => "s"
      case TimeUnit.MINUTES => "min"
      case TimeUnit.HOURS => "h"
      case TimeUnit.DAYS => "d"
    }
  }

  protected def ratio(unit: TimeUnit, u: TimeUnit) = {
    val r = unit.convert(1, u)
    if (r > 0) {
      r.toDouble
    } else {
      1.0 / u.convert(1, unit)
    }
  }

  override def toString = "%2.2f%s".format(time, abbreviate(unit))
}
