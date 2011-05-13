package blueeyes.persistence.cache

import scalaz.Semigroup
import blueeyes.concurrent._
import blueeyes.concurrent.ActorStrategy._

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