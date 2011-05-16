package blueeyes.persistence.cache

import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeUnit.NANOSECONDS

import blueeyes.util.Clock

case class ExpirableValue[V] private (_value: V, creationTimeNanos: Long)(implicit clock: Clock) {
  @volatile private var _accessTimeNanos = clock.nanoTime()

  /** The access time in nanosecons. */
  def accessTimeNanos: Long = _accessTimeNanos

  /** The access time, converted to the specified time unit. */
  def accessTime(unit: TimeUnit): Long = unit.convert(accessTimeNanos, NANOSECONDS)

  /** The creation time, converted to the specified time unit. */
  def creationTime(unit: TimeUnit): Long = unit.convert(creationTimeNanos, NANOSECONDS)

  def withValue(newValue: V): ExpirableValue[V] = copy(_value = newValue)

  /** Retrieves the value and updates the access time. Use _value to retrieve the value
   * without updating the access time.
   */
  def value = {
    _accessTimeNanos = clock.nanoTime()

    _value
  }
}

object ExpirableValue {
  /** Creates a new expirable entry given the specified creatione time and time unit. */
  def apply[V](value: V, creationTime: Long, unit: TimeUnit)(implicit clock: Clock): ExpirableValue[V] = {
    new ExpirableValue[V](value, unit.toNanos(creationTime))
  }

  /** Creates a new expirable entry using the current time as the creation time. */
  def apply[V](value: V)(implicit clock: Clock): ExpirableValue[V] = apply(value, clock.nanoTime(), NANOSECONDS)
}