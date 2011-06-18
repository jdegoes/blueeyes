package blueeyes
package persistence.cache.functional

import scalaz.Semigroup
import blueeyes.util.ClockSystem._

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

sealed trait StageIn[+K, +V]
case class PutAll[K, V](values: Iterable[(K, V)], time: Long) extends StageIn[K, V]
case object ExpireAll extends StageIn[Nothing, Nothing]
case class Expire(creationTime: Long, accessTime: Long) extends StageIn[Nothing, Nothing]

trait Stage[K, V] extends (StageIn[K, V] => StageNext[K, V]) {
  def putAll(values: Iterable[(K, V)], time: NanoTime): StageNext[K, V]

  def expire(creationTime: NanoTime, accessTime: NanoTime): StageNext[K, V]

  def expireAll: StageNext[K, V]
  
  def apply(m: StageIn[K, V]) = m match {
    case PutAll(values, time) => putAll(values, time)
    
    case Expire(creationTime, accessTime) => expire(creationTime, accessTime)

    case ExpireAll => expireAll
  }
}

object Stage {
  case class StageImpl[K, V: Semigroup](cache: TemporalCache[K, V], baseCapacity: Int, maxCapacity: Int) extends Stage[K, V] {
    lazy val semigroup = implicitly[Semigroup[V]]

    def putAll(values: Iterable[(K, V)], time: NanoTime) = split {
      def updated(k: K, v: V) = k -> cache.get(k).map(semigroup.append(_, v)).getOrElse(v)

      cache.putAll(reduceMap(values).map((updated _).tupled), time).truncate(baseCapacity, maxCapacity)
    }

    def expire(creationTime: NanoTime, accessTime: NanoTime) = {
      split(cache.expire(creationTime, accessTime))
    }

    def expireAll = split(cache.expireAll)

    override def toString = "Stage(" + cache.toString + "," + baseCapacity + "," + maxCapacity + ")"

    override def equals(that: Any) = that match {
      case that: Stage[_, _] => this.expireAll == that.expireAll
      case _ => false
    }

    override def hashCode = this.expireAll.hashCode

    private def split(state: TemporalCacheState[K, V]) = (state.removed, copy(cache = state.retained))

    private def reduceMap(m: Iterable[(K, V)]): Map[K, V] = m.foldLeft(Map.empty[K, V]) {
      case (m, (k, v)) => m + (k -> m.get(k).map(semigroup.append(_, v)).getOrElse(v))
    }
  }

  def empty[K, V: Semigroup](baseCapacity: Int, maxCapacity: Int): StageImpl[K, V] = 
    new StageImpl[K, V](TemporalCache.empty[K, V], baseCapacity, maxCapacity)

  def empty[K, V: Semigroup](capacity: Int): StageImpl[K, V] = empty(capacity - (capacity / 2), capacity + (capacity / 2))
}
