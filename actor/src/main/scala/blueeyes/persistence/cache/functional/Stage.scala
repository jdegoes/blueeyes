package blueeyes
package persistence.cache.functional

import blueeyes.util.ClockSystem._
import blueeyes.actor._
import blueeyes.actor.Actor._

import scalaz._
import scalaz.Scalaz._

sealed trait StageIn[+K, +V]
case class PutAll[K, V](values: Iterable[(K, V)], time: Long) extends StageIn[K, V]
case object ExpireAll extends StageIn[Nothing, Nothing]
case class Expire(creationTime: Long, accessTime: Long) extends StageIn[Nothing, Nothing]

case class StageActorPimp[K, V: Semigroup](actor: StageActor[K, V]) {
  def putAll(values: Iterable[(K, V)], time: NanoTime): StageState[K, V] = actor ! PutAll(values, time)

  def expire(creationTime: NanoTime, accessTime: NanoTime): StageState[K, V] = actor ! Expire(creationTime, accessTime)

  def expireAll: StageState[K, V] = actor ! ExpireAll

  def size: Int = expireAll._1.size
}

object StageActorPimp {
  implicit def ToStageActorPimp[K, V: Semigroup](actor: StageActor[K, V]): StageActorPimp[K, V] = StageActorPimp[K, V](actor)
}

case class Stage[K, V: Semigroup](cache: TemporalCache[K, V], baseCapacity: Int, maxCapacity: Int) extends Actor[StageIn[K, V], Map[K, V]] { self =>
  def receive(in: StageIn[K, V]): StageState[K, V] = {
    in match {
      case PutAll(values, time) =>
        split {
          def updated(k: K, v: V) = k -> cache.get(k).map(semigroup.append(_, v)).getOrElse(v)

          cache.putAll(reduceMap(values).map((updated _).tupled), time).truncate(baseCapacity, maxCapacity)
        }

      case Expire(creationTime, accessTime) =>
        split(cache.expire(creationTime, accessTime))

      case ExpireAll => split(cache.expireAll)
    }
  }

  override def toString = "Stage(" + cache.toString + "," + baseCapacity + "," + maxCapacity + ")"

  override def equals(that: Any) = that match {
    case that: Function[_, _] => (this ! ExpireAll) == (that.asInstanceOf[StageActor[K, V]] ! ExpireAll)
    case _ => false
  }

  override def hashCode = (this ! ExpireAll).hashCode

  private lazy val semigroup = implicitly[Semigroup[V]]

  private def split(state: TemporalCacheState[K, V]) = (state.removed, copy(cache = state.retained))

  private def reduceMap(m: Iterable[(K, V)]): Map[K, V] = m.foldLeft(Map.empty[K, V]) {
    case (m, (k, v)) => m + (k -> m.get(k).map(semigroup.append(_, v)).getOrElse(v))
  }
}

object Stage {

  def empty[K, V: Semigroup](baseCapacity: Int, maxCapacity: Int): Stage[K, V] = 
    Stage[K, V](TemporalCache.empty[K, V], baseCapacity, maxCapacity)

  def empty[K, V: Semigroup](capacity: Int): Stage[K, V] = empty(capacity - (capacity / 2), capacity + (capacity / 2))
}
