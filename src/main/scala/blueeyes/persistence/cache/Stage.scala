package blueeyes
package persistence.cache

import scalaz.Semigroup
import blueeyes.concurrent._
import blueeyes.concurrent.ActorStrategy._
import blueeyes.util.ClockSystem._

trait Stage[K, V] extends FutureDeliveryStrategySequential {
  private case class PutAll(pairs: Iterable[(K, V)])(implicit val semigroup: Semigroup[V])

  def flush(k: K, v: V): Unit

  def expirationPolicy: ExpirationPolicy

  def maximumCapacity: Int

  private class Cache extends scala.collection.mutable.Map[K, ExpirableValue[V]] { self =>
    private val impl = new scala.collection.mutable.LinkedHashMap[K, ExpirableValue[V]]

    def get(key: K): Option[ExpirableValue[V]] = impl.get(key)

    def iterator: Iterator[(K, ExpirableValue[V])] = impl.iterator

    def += (kv: (K, ExpirableValue[V])): this.type = {
      impl.remove(kv._1)
      impl.put(kv._1, kv._2)

      this
    }

    def -= (k: K): this.type = {
      impl.get(k) foreach { v =>
        flush(k, v.value) 
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

    override def foreach[U](f: ((K, ExpirableValue[V])) => U): Unit = impl.foreach(f)
  }

  private val worker = new Actor {
    import scala.math._
    import java.util.concurrent.TimeUnit

    private val cache           = new Cache
    private var flushScheduled  = false
    private val duration        = Duration(min(expirationPolicy.timeToIdleNanos.getOrElse(2000000000l), expirationPolicy.timeToLiveNanos.getOrElse(2000000000l)) / 2, TimeUnit.NANOSECONDS)

    val putAll = lift1 { request: PutAll =>
      putToCache(request)
      removeEldestEntries
      scheduleFlush
    }

    private def scheduleFlush: Unit = if (!flushScheduled) {
      ScheduledExecutor.once(flushAllBySchedule, duration)
      flushScheduled = true
    }

    private def putToCache(request: PutAll) {
      cache ++= request.pairs.map {
        case (key, value) => (
          key,
          cache.get(key).map{ current => current.withValue(request.semigroup.append(current.value, value)) }.getOrElse(ExpirableValue(value))
        )
      }
    }

    private def removeEldestEntries {
      cache.removeEldestEntries(0.max(cache.size - maximumCapacity))
    }

    val flushAllBySchedule = lift { () =>
      val currentTime = System.nanoTime()

      val keysToRemove = cache.foldLeft[List[K]](Nil) { 
        case (keysToRemove, (key, value)) =>
          if(expirationPolicy.isExpired(value, currentTime)) key :: keysToRemove 
          else keysToRemove
      }

      cache --= keysToRemove

      flushScheduled = false

      if (cache.size > 0) scheduleFlush
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
