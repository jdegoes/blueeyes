package blueeyes
package persistence.cache

import scalaz.Semigroup
import blueeyes.util.metrics.Duration
import blueeyes.concurrent.{Future, ScheduledExecutor, FutureDeliveryStrategy}
import blueeyes.concurrent.ActorStrategy._
import blueeyes.concurrent.FutureImplicits._
import blueeyes.util.ClockSystem._

import scalaz.Scalaz._
import akka.actor.{Actor, ActorRef, Scheduler}


abstract class Stage[K, V](implicit futureDeliveryStrategy: FutureDeliveryStrategy) {

  private sealed trait StageIn
  private case class PutAll(pairs: Iterable[(K, V)])(implicit val semigroup: Semigroup[V]) extends StageIn
  private object     FlushAll extends StageIn
  private object     FlushAllBySchedule extends StageIn

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

  private val actor: ActorRef = Actor.actorOf(new Actor{
    import scala.math._
    import java.util.concurrent.TimeUnit

    private val cache           = new Cache
    private var flushScheduled  = false
    private val duration        = Duration(min(expirationPolicy.timeToIdleNanos.getOrElse(2000000000l), expirationPolicy.timeToLiveNanos.getOrElse(2000000000l)) / 2, TimeUnit.NANOSECONDS)

    def receive = {
      case request: PutAll =>
        putToCache(request)
        removeEldestEntries
        scheduleFlush

      case FlushAllBySchedule =>
        val currentTime = System.nanoTime()

        val keysToRemove = cache.foldLeft[List[K]](Nil) {
          case (keysToRemove, (key, value)) =>
            if(expirationPolicy.isExpired(value, currentTime)) key :: keysToRemove
            else keysToRemove
        }

        cache --= keysToRemove

        flushScheduled = false

        if (cache.size > 0) scheduleFlush
      case FlushAll =>
        cache.clear()
        self.reply(())
    }

    private def scheduleFlush: Unit = if (!flushScheduled) {
       Scheduler.scheduleOnce(actor, FlushAllBySchedule, duration.time.toLong, duration.unit)
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
  })
  actor.start

  def += (k: K, v: V)(implicit sg: Semigroup[V]) = put(k, v)

  def += (tuple: (K, V))(implicit sg: Semigroup[V]) = put(tuple._1, tuple._2)

  def put(k: K, v: V)(implicit sg: Semigroup[V]) = actor ! PutAll((k, v) :: Nil)

  def putAll(pairs: Iterable[(K, V)])(implicit sg: Semigroup[V]) = actor ! PutAll(pairs)

  def flushAll(): Future[Unit] = actor.!!![Unit](FlushAll)

  def stop(): Future[Unit] = {
    val stopFuture = new Future[Unit]()
    val flushFuture = flushAll()

    def stopActorAndFinish = {
      actor.stop()
      stopFuture.deliver(())
    }

    stopFuture.ifCanceled{v => stopActorAndFinish}
    stopFuture.deliver{v: Unit => stopActorAndFinish}

    stopFuture
  }
}

object Stage {
  def apply[K, V](policy: ExpirationPolicy, capacity: Int, evict: (K, V) => Unit)(implicit deliveryStrategy: FutureDeliveryStrategy): Stage[K, V] = new Stage[K, V]()(deliveryStrategy) {
    def expirationPolicy = policy

    def maximumCapacity = capacity

    def flush(k: K, v: V): Unit = evict(k, v)
  }
}
