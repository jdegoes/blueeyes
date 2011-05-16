package blueeyes
package persistence.cache.functional

import scalaz.Semigroup
import blueeyes.concurrent._
import blueeyes.persistence.cache.{ExpirationPolicy}
import blueeyes.concurrent.ActorStrategy._
import blueeyes.util.ClockSystem._

import scalaz.Scalaz._

import scala.collection.immutable.TreeMap

case class TemporalCacheState[K, V](removed: Map[K, V], retained: TemporalCache[K, V])

object TemporalCacheState {
  implicit def tuple2TCS[K, V](tuple: (Map[K, V], TemporalCache[K, V])) = TemporalCacheState(tuple._1, tuple._2)
}

case class CacheValue[V](value: V, creationTime: NanoTime, accessTime: NanoTime)

case class TemporalCache[K, V](accessMap: TreeMap[NanoTime, List[K]], cache: Map[K, CacheValue[V]]) {
  import TemporalCacheState._
  def size = cache.size

  def truncate(baseCapacity: Int, maxCapacity: Int): TemporalCacheState[K, V] = {
    assert(maxCapacity > baseCapacity)

    if (cache.size > maxCapacity) split(accessMap.splitAt(cache.size - baseCapacity))
    else TemporalCacheState(Map.empty, this)
  }

  def expire(time: NanoTime): TemporalCacheState[K, V] = split(accessMap.span(_._1 < time))

  def expireAll = expire(Long.MaxValue)

  def put(kv: (K, V), time: NanoTime): TemporalCache[K, V] = {
    val (key, value) = kv

    def removeAccess(t: NanoTime): TreeMap[NanoTime, List[K]] = accessMap(t) match {
      case `key` :: Nil => accessMap - t
      case xs => accessMap + (t -> xs.filterNot(_ == key))
    }

    val withoutOld = cache.get(key).map(v => removeAccess(v.accessTime)).getOrElse(accessMap)

    TemporalCache(
      accessMap = withoutOld + (time -> (key :: withoutOld.getOrElse[List[K]](time, Nil))),
      cache = cache + (key -> CacheValue(value, creationTime = cache.get(key).map(_.creationTime).getOrElse(time), accessTime = time))
    )
  }

  def putAll(vs: Iterable[(K, V)], time: NanoTime) = vs.foldLeft(this)(_.put(_, time))

  def get(k: K): Option[V] = cache.get(k).map(_.value)

  private def split(t: (TreeMap[NanoTime, List[K]], TreeMap[NanoTime, List[K]])): TemporalCacheState[K, V] = {
    val (removedA, keptA) = t

    val keysToRemove = removedA.values.flatten.toSet

    val (removedC, keptC) = cache.partition {
      case (key, _) => keysToRemove.contains(key)
    }

    TemporalCacheState(removedC.mapValues(_.value), TemporalCache(keptA, keptC))
  }
}

object TemporalCache {
  def empty[K, V] = TemporalCache[K,V](TreeMap.empty, Map.empty)
}

sealed trait StageIn
case class PutAll[K, V](values: Iterable[(K, V)], time: Long) extends StageIn
case object FlushAll extends StageIn
case class FlushExpired(time: Long) extends StageIn

trait Stage[K, V] extends (StageIn => StageNext[K, V]) {
  def putAll(values: Iterable[(K, V)], time: NanoTime): StageNext[K, V]

  def flushExpired(time: NanoTime): StageNext[K, V]

  def flushAll: StageNext[K, V]
  
  def apply(m: StageIn) = m match {
    case PutAll(values: Iterable[(K, V)], time) => putAll(values, time)
    
    case FlushExpired(time) => flushExpired(time)

    case FlushAll => flushAll
  }
}

object Stage {
  case class StageImpl[K, V: Semigroup](cache: TemporalCache[K, V], policy: ExpirationPolicy, baseCapacity: Int, maxCapacity: Int) extends Stage[K, V] {
    lazy val semigroup = implicitly[Semigroup[V]]

    def empty = Stage.empty[K, V](policy, baseCapacity, maxCapacity)

    def putAll(values: Iterable[(K, V)], time: NanoTime) = split(cache.putAll(values, time).truncate(baseCapacity, maxCapacity))

    def flushExpired(time: NanoTime) = split(cache.expire(time))

    def flushAll = split(cache.expireAll)

    private def split(state: TemporalCacheState[K, V]): StageNext[K,V] = (state.removed, copy(cache = state.retained))
  }

  def empty[K, V: Semigroup](policy: ExpirationPolicy, baseCapacity: Int, maxCapacity: Int): StageImpl[K, V] = 
    new StageImpl[K, V](TemporalCache.empty[K, V], policy, baseCapacity, maxCapacity)

  def empty[K, V: Semigroup](policy: ExpirationPolicy, capacity: Int): StageImpl[K, V] = empty(policy, capacity - (capacity / 2), capacity + (capacity / 2))
}