package blueeyes.persistence.cache

import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeUnit.NANOSECONDS
import java.lang.System.{nanoTime}

/** An expirable entry in a cache.
 */
case class Expirable[K, V] private (key: K, _value: V, policy: ExpirationPolicy, creationTimeNanos: Long) {
  private var _accessTimeNanos = nanoTime()
  
  /** The access time in nanosecons. */
  def accessTimeNanos: Long = _accessTimeNanos
  
  /** The access time, converted to the specified time unit. */
  def accessTime(unit: TimeUnit): Long = unit.convert(accessTimeNanos, NANOSECONDS)
  
  /** The creation time, converted to the specified time unit. */
  def creationTime(unit: TimeUnit): Long = unit.convert(creationTimeNanos, NANOSECONDS)
  
  /** Retrieves the value and updates the access time. Use _value to retrieve the value
   * without updating the access time.
   */
  def value = {
    _accessTimeNanos = nanoTime()
    
    _value
  }
}

object Expirable {
  /** Creates a new expirable entry given the specified creatione time and time unit. */
  def apply[K, V](key: K, value: V, policy: ExpirationPolicy, creationTime: Long, unit: TimeUnit): Expirable[K, V] = {
    new Expirable[K, V](key, value, policy, unit.toNanos(creationTime))
  }
  
  /** Creates a new expirable entry using the current time as the creation time. */
  def apply[K, V](key: K, value: V, policy: ExpirationPolicy): Expirable[K, V] = apply(key, value, policy, nanoTime(), NANOSECONDS)
}