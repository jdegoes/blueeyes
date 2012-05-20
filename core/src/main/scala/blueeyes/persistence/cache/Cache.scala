package blueeyes.persistence.cache

import scala.collection.mutable.{Map, ConcurrentMap}

import java.util.concurrent.{ConcurrentMap => JConcurrentMap}

case class CacheSettings[K, V](
  expirationPolicy:         ExpirationPolicy,
  initialCapacity:          Int                     = 1000,
  evict:                    (K, V) => Unit          = (k: K, v: V) => (),
  maximumWeightedCapacity:  Int                     = 10000,
  weigh:                    V => Int                = (v: V) => 1
) 

/** Contains factory methods for creating various kinds of caches. A cache is
 * an ordinary Scala mutable map.
 */
object Cache {
  import java.util.concurrent.Executors
  import Expirable._

  lazy val ScheduledExecutor = Executors.newScheduledThreadPool(1)

  /** Creates a concurrent cache with the specified settings and concurrency level.
   */
  def concurrent[K, V](settings: CacheSettings[K, V], concurrencyLevel: Int = 10): ConcurrentMap[K, V] = {
    new ExpirableMap[K, V](newConcurrentMap(settings, concurrencyLevel), settings.expirationPolicy, expirationCheck[K, V], ScheduledExecutor, settings.evict)
  }

  def concurrentWithCheckedEviction[K, V](settings: CacheSettings[K, V], concurrencyLevel: Int = 10)(evictable: (K, V) => Boolean): ConcurrentMap[K, V] = {
    val check = (expirable: Expirable[K, V]) => evictable(expirable.key, expirable.value) && expirationCheck(expirable)
    new ExpirableMap[K, V](newConcurrentMap(settings, concurrencyLevel), settings.expirationPolicy, check, ScheduledExecutor, settings.evict)
  }

  private def newConcurrentMap[K, V](settings: CacheSettings[K, V], concurrencyLevel: Int): JConcurrentMap[K, Expirable[K, V]] = {
    import com.googlecode.concurrentlinkedhashmap.{ConcurrentLinkedHashMap, CapacityLimiter, Weigher, EvictionListener}

    new ConcurrentLinkedHashMap.Builder[K, Expirable[K, V]]().
      concurrencyLevel(concurrencyLevel).
      initialCapacity(settings.initialCapacity).
      listener(new EvictionListener[K, Expirable[K, V]]() {
        def onEviction(k: K, v: Expirable[K, V]) = settings.evict(k, v._value)
      }).maximumWeightedCapacity(settings.maximumWeightedCapacity).
      weigher(new Weigher[Expirable[K, V]] {
        def weightOf(v: Expirable[K, V]): Int = settings.weigh(v._value)
      }: Weigher[Expirable[K, V]]).build()
  }
}
