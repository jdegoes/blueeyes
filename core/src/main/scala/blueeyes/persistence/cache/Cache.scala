package blueeyes.persistence.cache

import scala.collection.mutable.{Map, ConcurrentMap}

import java.util.concurrent.{ConcurrentMap => JConcurrentMap}

/** Contains factory methods for creating various kinds of caches. A cache is
 * an ordinary Scala mutable map.
 */
object Cache {
  import java.util.concurrent.Executors

  lazy val ScheduledExecutor = Executors.newScheduledThreadPool(1)

  /** Creates a concurrent cache with the specified settings and concurrency level.
   */
  def concurrent[K, V](settings: CacheSettings[K, V], concurrencyLevel: Int = 10): ConcurrentMap[K, V] = {
    new ExpirableMap[K, V](newConcurrentMap(settings, concurrencyLevel), settings.expirationPolicy, new ExpirationPredicate(), ScheduledExecutor, settings.evict)
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