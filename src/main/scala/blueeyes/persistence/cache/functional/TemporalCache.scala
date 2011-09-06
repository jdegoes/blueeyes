package blueeyes
package persistence.cache.functional

import scalaz._
import scalaz.Scalaz._

case class TemporalCacheState[K, V](removed: Map[K, V], retained: TemporalCache[K, V])

object TemporalCacheState {
  implicit def tuple2TCS[K, V](tuple: (Map[K, V], TemporalCache[K, V])) = TemporalCacheState(tuple._1, tuple._2)
}

case class CacheValue[V](value: V, creationTime: NanoTime, accessTime: NanoTime)

case class TemporalCache[K, V] private (private val cache: Map[K, CacheValue[V]]) {
  import TemporalCacheState._
  def size = cache.size

  def truncate(baseCapacity: Int, maxCapacity: Int): TemporalCacheState[K, V] = {
    assert(maxCapacity >= baseCapacity)

    if (cache.size > maxCapacity) {
      val numTruncate = cache.size - baseCapacity
      val (removed, kept) = cache.toArray.sortBy(_._2.accessTime).splitAt(numTruncate)
      TemporalCacheState(removed.view.map(t => (t._1, t._2.value)).toMap, TemporalCache(kept.toMap))
    } else {
      TemporalCacheState(Map.empty, this)
    }
  }

  def expire(creationTime: NanoTime, accessTime: NanoTime): TemporalCacheState[K, V] = {
    val (removed, kept) = cache.partition {
      case (_, CacheValue(_, ct, at)) => ct < creationTime || at < accessTime
    }

    TemporalCacheState(removed.mapValues(_.value), TemporalCache(kept))
  }

  def expireAll = TemporalCacheState(cache.mapValues(_.value), TemporalCache.empty[K, V])

  def put(kv: (K, V), accessTime: NanoTime): TemporalCache[K, V] = {
    val (key, value) = kv
    val oldValue = cache.get(key)

    TemporalCache(
      cache + (
        key -> CacheValue(
          value, oldValue.map(_.creationTime).getOrElse(accessTime), 
          oldValue.cata(v => v.accessTime.max(accessTime), accessTime)
        )
      )
    )
  }

  def putAll(vs: Iterable[(K, V)], accessTime: NanoTime) = vs.foldLeft(this)(_.put(_, accessTime))

  def get(k: K): Option[V] = cache.get(k).map(_.value)
}

object TemporalCache {
  def empty[K, V] = TemporalCache[K,V](Map.empty)
}