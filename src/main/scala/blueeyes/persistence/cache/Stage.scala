package blueeyes.persistence.cache

import scalaz.Semigroup
import blueeyes.concurrent._
import blueeyes.concurrent.ActorStrategy._

import scalaz.Scalaz._

object functional {
  import scala.collection.immutable.TreeMap

  private type AccessTime = Long

  private type TemporalCacheState[K, V] = (Map[K, V], TemporalCache[K, V])

  case class TemporalCache[K, V](accessMap: TreeMap[AccessTime, List[K]], cache: Map[K, (V, AccessTime)]) {
    def size = cache.size

    def truncate(baseCapacity: Int, maxCapacity: Int): TemporalCacheState[K, V] = {
      assert(maxCapacity > baseCapacity)

      if (cache.size > maxCapacity) split(accessMap.splitAt(cache.size - baseCapacity))
      else (Map.empty, this)
    }

    def expire(time: AccessTime): TemporalCacheState[K, V] = split(accessMap.span(_._1 < time))

    def expireAll = expire(Long.MaxValue)

    def put(kv: (K, V), time: AccessTime) = {
      val (key, value) = kv

      TemporalCache(
        accessMap = {
          val withoutOld = cache.get(key).map(v => removeAccess(accessMap, v._2, key)).getOrElse(accessMap)
          
          withoutOld + (time -> (key :: withoutOld.getOrElse[List[K]](time, Nil)))
        }, 
        cache = cache + (key -> (value -> time))
      )
    }

    def putAll(vs: Iterable[(K, V)], time: AccessTime) = vs.foldLeft(this)(_.put(_, time))

    def get(k: K): Option[V] = cache.get(k).map(_._1)

    private def removeAccess(m: TreeMap[AccessTime, List[K]], t: AccessTime, k: K): TreeMap[AccessTime, List[K]] = m(t) match {
      case Nil => m - t
      case `k` :: Nil => m - t
      case xs => m + (t -> xs.filterNot(_ == k))
    }

    private def split(t: (TreeMap[AccessTime, List[K]], TreeMap[AccessTime, List[K]])): TemporalCacheState[K, V] = {
      val (removedA, keptA) = t

      val keysToRemove = removedA.values.flatten.toSet

      val (removedC, keptC) = cache.partition(t => keysToRemove.contains(t._1))

      // val (removedC, keptC) = removedA.view.values.flatten.foldLeft((Map.empty[K, V], cache)) {
      //   case ((removedC, keptC), keyToRemove) =>
      //     (removedC + (keyToRemove -> keptC(keyToRemove)._1), keptC - keyToRemove)
      // }

      (removedC.mapValues(_._1), TemporalCache(
        keptA,
        keptC
      ))
    }
  }

  object TemporalCache {
    def empty[K, V] = TemporalCache(TreeMap.empty[AccessTime, List[K]], Map.empty[K, (V, AccessTime)])
  }

  sealed trait StageIn
  case class PutAll[K, V](values: Iterable[(K, V)], time: Long) extends StageIn
  case object FlushAll extends StageIn
  case class FlushExpired(time: Long) extends StageIn

  trait Stage[K, V] extends (StageIn => (Map[K, V], Stage[K, V]))

  object Stage {
    case class StageImpl[K, V: Semigroup](cache: TemporalCache[K, V], policy: ExpirationPolicy, baseCapacity: Int, maxCapacity: Int) extends Stage[K, V] {
      lazy val semigroup = implicitly[Semigroup[V]]

      def empty = Stage.empty[K, V](policy, baseCapacity, maxCapacity)

      def apply(m: StageIn) = m match {
        case PutAll(values: Iterable[(K, V)], time) => split(cache.putAll(values, time).truncate(baseCapacity, maxCapacity))
        
        case FlushExpired(time) => split(cache.expire(time))

        case FlushAll => split(cache.expireAll)
      }

      private def split(t: (Map[K, V], TemporalCache[K, V])): (Map[K, V], Stage[K, V]) = {
        val (discarded, retained) = t

        (discarded, copy(cache = retained))
      }
    }
    def empty[K, V: Semigroup](policy: ExpirationPolicy, baseCapacity: Int, maxCapacity: Int): StageImpl[K, V] = 
      new StageImpl[K, V](TemporalCache.empty[K, V], policy, baseCapacity, maxCapacity)

    def empty[K, V: Semigroup](policy: ExpirationPolicy, capacity: Int): StageImpl[K, V] = empty(policy, capacity - (capacity / 2), capacity + (capacity / 2))
  }
}

trait Stage[K, V] extends FutureDeliveryStrategySequential {
  private case class PutAll(pairs: Iterable[(K, V)])(implicit val semigroup: Semigroup[V])

  def flush(k: K, v: V): Unit

  def expirationPolicy: ExpirationPolicy

  def maximumCapacity: Int

  class Cache[A, B](flush: (A, B) => Unit) extends scala.collection.mutable.Map[A, B] { self =>
    private val impl = new scala.collection.mutable.LinkedHashMap[A, B]

    def get(key: A): Option[B] = impl.get(key)

    def iterator: Iterator[(A, B)] = impl.iterator

    def += (kv: (A, B)): this.type = {
      impl.put(kv._1, kv._2)

      this
    }

    def -= (k: A): this.type = {
      impl.get(k) foreach { v =>
        flush(k, v)

        impl.remove(k)
      }

      this
    }

    def removeEldestEntries(n: Int): this.type = {
      keys.take(n).foreach { key =>
        self -= key
      }

      this
    }

    override def size = impl.size

    override def foreach[U](f: ((A, B)) => U): Unit = {
      val it = impl.iterator

      while (it.hasNext) {
        val entry = it.next

        f((entry._1, entry._2))
      }
    }
  }

  private val flusher = (k: K, v: ExpirableValue[V]) => flush(k, v._value)

  private val worker = new Actor{
    import scala.math._
    import java.util.concurrent.TimeUnit

    private val cache           = new Cache[K, ExpirableValue[V]](flusher)
    private var flushScheduled  = false
    private val duration        = Duration(min(expirationPolicy.timeToIdleNanos.getOrElse(2000000000l), expirationPolicy.timeToLiveNanos.getOrElse(2000000000l)) / 2, TimeUnit.NANOSECONDS)

    val putAll = lift1 { request: PutAll =>
      putToCache(request)
      removeEldestEntries
      removeExpiredEntries
      scheduleFlush
    }

    private def scheduleFlush{
      if (!flushScheduled){
        ScheduledExecutor.once(flushAllBySchedule, duration)
        flushScheduled = true
      }
    }

    private def putToCache(request: PutAll){
      cache ++= request.pairs.map { tuple =>
        val (key, value2) = tuple

        val newValue = cache.get(key).map { expirableValue1 =>
          val value1 = expirableValue1.value

          expirableValue1.withValue(request.semigroup.append(value1, value2))
        }.getOrElse(ExpirableValue(value2))

        (tuple._1, newValue)
      }
    }

    private def removeEldestEntries{
      val overflowCount = 0.max(cache.size - maximumCapacity)

      cache.removeEldestEntries(overflowCount)
    }

    private def removeExpiredEntries{
      val currentTime = System.nanoTime()

      val keysToRemove = cache.foldLeft[List[K]](Nil) { (keysToRemove, tuple) =>
        val (key, value) = tuple

        val isExpired = expirationPolicy.isExpired(value, currentTime)

        if (isExpired) key :: keysToRemove else keysToRemove
      }

      cache --= keysToRemove
    }

    val flushAllBySchedule = lift { () =>
      cache.clear()
      flushScheduled = false
    }

    val flushAll = lift { () => cache.clear() }
  }

  def += (k: K, v: V)(implicit sg: Semigroup[V]): Future[Unit] = put(k, v)

  def += (tuple: (K, V))(implicit sg: Semigroup[V]): Future[Unit] = put(tuple._1, tuple._2)

  def put(k: K, v: V)(implicit sg: Semigroup[V]): Future[Unit] = putAll((k, v) :: Nil)

  def putAll(pairs: Iterable[(K, V)])(implicit sg: Semigroup[V]): Future[Unit] = (worker putAll(PutAll(pairs))).toUnit

  def flushAll(): Future[Unit] = (worker flushAll()).toUnit

}

object Stage {
  def apply[K, V](policy: ExpirationPolicy, capacity: Int, evict: (K, V) => Unit) = new Stage[K, V] {
    def expirationPolicy = policy

    def maximumCapacity = capacity

    def flush(k: K, v: V): Unit = evict(k, v)
  }
}