package blueeyes
package persistence.cache.functional

import scalaz.Semigroup
import blueeyes.concurrent._
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
    assert(maxCapacity >= baseCapacity)
    

    if (cache.size > maxCapacity) split(accessMap.splitAt(cache.size - baseCapacity))
    else TemporalCacheState(Map.empty, this)
  }

  def expireByAccessTime(accessTime: NanoTime): TemporalCacheState[K, V] = 
    split(accessMap.span(_._1 < accessTime))

  def expireByCreationTime(creationTime: NanoTime): TemporalCacheState[K, V] = {
    val (removedC, keptC) = cache.partition(_._2.creationTime < creationTime)

    val keptA = accessMap.foldLeft(accessMap) {
      case (accessMap, (accessTime, keys)) => 
        val keptK = keys.filter(keptC.contains)
        if (keptK.isEmpty) accessMap - accessTime else accessMap + (accessTime -> keptK)
    }
        
    TemporalCacheState(
      removedC.mapValues(_.value),
      TemporalCache(keptA, keptC)
    )
  }

  def expireAll = TemporalCacheState(cache.mapValues(_.value), TemporalCache.empty[K, V])

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

    private def reduceMap(m: Iterable[(K, V)]): Map[K, V] = m.foldLeft(Map.empty[K, V]) {
      case (m, (k, v)) => m + (k -> m.get(k).map(semigroup.append(_, v)).getOrElse(v))
    }

    def putAll(values: Iterable[(K, V)], time: NanoTime) = split {
      def updated(k: K, v: V) = k -> cache.get(k).map(semigroup.append(_, v)).getOrElse(v)

      cache.putAll(reduceMap(values).map((updated _).tupled), time).truncate(baseCapacity, maxCapacity)
    }

    def expire(creationTime: NanoTime, accessTime: NanoTime) = {
      split(cache.expireByAccessTime(accessTime)) match {
        case (removed1, kept1) =>
          kept1.split(kept1.cache.expireByCreationTime(creationTime)) match {
            case (removed2, kept2) => (removed1 ++ removed2, kept2)
          }
      }
    }

    def expireAll = split(cache.expireAll)

    override def toString = "Stage(" + cache.toString + "," + baseCapacity + "," + maxCapacity + ")"

    override def equals(that: Any) = that match {
      case that: Stage[K, V] => this.expireAll == that.expireAll
      case _ => false
    }

    override def hashCode = this.expireAll.hashCode

    private def split(state: TemporalCacheState[K, V]) = (state.removed, copy(cache = state.retained))
  }

  def empty[K, V: Semigroup](baseCapacity: Int, maxCapacity: Int): StageImpl[K, V] = 
    new StageImpl[K, V](TemporalCache.empty[K, V], baseCapacity, maxCapacity)

  def empty[K, V: Semigroup](capacity: Int): StageImpl[K, V] = empty(capacity - (capacity / 2), capacity + (capacity / 2))
}
