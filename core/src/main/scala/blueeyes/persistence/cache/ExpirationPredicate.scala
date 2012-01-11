package blueeyes.persistence.cache

import java.lang.System.nanoTime
import java.util.concurrent.TimeUnit.NANOSECONDS

/** A predicate that returns true for all expirable entries that are expired.
 */
class ExpirationPredicate[K, V] extends Function[Expirable[K, V], Boolean] {
  def apply(expirable: Expirable[K, V]) = {
    val policy = expirable.policy
    val currentTime = nanoTime()
    
    !policy.eternal &&
    (isPastTime(policy.timeToIdle(NANOSECONDS), expirable.accessTime(NANOSECONDS),   currentTime) ||
     isPastTime(policy.timeToLive(NANOSECONDS), expirable.creationTime(NANOSECONDS), currentTime))
  }
  
  private def isPastTime(policyTime: Option[Long], baseTime: Long, currentTime: Long) = policyTime match {
    case Some(policyTime) => currentTime > (policyTime + baseTime)
    
    case None => false
  }
}