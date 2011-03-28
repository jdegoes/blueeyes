package blueeyes.persistence.cache

import java.lang.ref.WeakReference

private[cache] sealed class ExpirationTask[K, V](expirable: Expirable[K, V], hasExpired: Expirable[K, V] => Boolean, evict: Expirable[K, V] => Unit) extends Runnable {
  private final val expirableRef  = new WeakReference[Expirable[K, V]](expirable)
  private final val hasExpiredRef = new WeakReference[Expirable[K, V] => Boolean](hasExpired)

  def run() = {
    val (hasExpired, expirable) = (hasExpiredRef.get, expirableRef.get)

    if (hasExpired != null && expirable != null) {
      if (hasExpired(expirable)) {
        evict(expirable)
      }
    }
  }
}