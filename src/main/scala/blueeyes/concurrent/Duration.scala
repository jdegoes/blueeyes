package blueeyes.concurrent

import java.util.concurrent.TimeUnit

case class Duration(time: Double, unit: TimeUnit){
  val length = time.toLong

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
  private def abbreviate(u: TimeUnit) = {
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

  private def ratio(unit: TimeUnit, u: TimeUnit) = {
    val r = unit.convert(1, u)
    if (r > 0) {
      r.toDouble
    } else {
      1.0 / u.convert(1, unit)
    }
  }

  /**
   * Convert to an arbitrary time unit.
   */
  def convert(u: TimeUnit) = Duration((time / ratio(unit, u)), u)

  override def toString = "%2.2f%s".format(time, abbreviate(unit))
}

object Duration {
  // So user can write: 23.milliseconds, 92.seconds, etc.
  class ToDuration[N](numeric: N)(implicit num: Numeric[N]) {
    private val value = num.toLong(numeric)
    def milliseconds = Duration(value, TimeUnit.MILLISECONDS)
    def nanoseconds  = Duration(value, TimeUnit.NANOSECONDS)
    def microseconds = Duration(value, TimeUnit.MICROSECONDS)
    def seconds      = Duration(value, TimeUnit.SECONDS)
    def minutes      = Duration(value, TimeUnit.MINUTES)
    def hours        = Duration(value, TimeUnit.HOURS)
    def days         = Duration(value, TimeUnit.DAYS)
  }

  implicit def toDuration[N](numeric: N)(implicit num: Numeric[N]): ToDuration[N] = new ToDuration(numeric)
}
