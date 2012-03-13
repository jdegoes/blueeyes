package blueeyes.persistence.cache

import blueeyes.bkka.Stop
import blueeyes.bkka.ActorRefStop
import akka.actor.{Actor, ActorRef, Props, Scheduler, PoisonPill, ActorKilledException, ActorSystem}
import akka.pattern.ask
import akka.dispatch.Future
import akka.dispatch.Promise
import akka.util.Timeout
import akka.util.Duration

import blueeyes.health.HealthMonitor
import blueeyes.util.ClockSystem._

import scala.collection.JavaConversions._
import scalaz.Semigroup

import com.weiglewilczek.slf4s.Logging

abstract class Stage[K, V](monitor: HealthMonitor = HealthMonitor.Noop) {
  private sealed trait StageIn
  private case class PutAll(pairs: Iterable[(K, V)], semigroup: Semigroup[V]) extends StageIn
  private object     FlushAll extends StageIn
  private object     FlushAllBySchedule extends StageIn

  def flush(k: K, v: V): Unit

  def expirationPolicy: ExpirationPolicy

  def maximumCapacity: Int

  private val actorSystem = ActorSystem("blueeyes_stage")

  private class Cache extends scala.collection.mutable.Map[K, ExpirableValue[V]] { self =>
    private val impl = new javolution.util.FastMap[K, ExpirableValue[V]]

    def get(key: K): Option[ExpirableValue[V]] = Option(impl.get(key))

    def iterator: Iterator[(K, ExpirableValue[V])] = impl.iterator

    def += (kv: (K, ExpirableValue[V])): this.type = {
      impl.remove(kv._1)
      impl.put(kv._1, kv._2)

      this
    }

    def -= (k: K): this.type = {
      Option(impl.get(k)) foreach { v =>
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

  private class StageActor extends Actor with Logging {
    import scala.math._
    import java.util.concurrent.TimeUnit

    private val cache           = new Cache
    private var flushScheduled  = false
    private val duration        = Duration(min(expirationPolicy.timeToIdleNanos.getOrElse(2000000000l), expirationPolicy.timeToLiveNanos.getOrElse(2000000000l)) / 2, TimeUnit.NANOSECONDS)

    def receive = {
      case PutAll(pairs, semigroup) =>
        putToCache(pairs, semigroup)
        removeEldestEntries
        scheduleFlush

      case FlushAllBySchedule =>
        val currentTime = System.nanoTime()

        val keysToRemove = cache.foldLeft[List[K]](Nil) {
          case (keysToRemove, (key, value)) =>
            if(expirationPolicy.isExpired(value, currentTime)) key :: keysToRemove
            else keysToRemove
        }

        //monitor.sample("flush_size")(keysToRemove.size)
        cache --= keysToRemove

        flushScheduled = false

        if (cache.size > 0) scheduleFlush

      case FlushAll =>
        val cacheSize = cache.size
        //monitor.sample("cache_size")(cacheSize)
        //monitor.sample("actor_queue_size")( actor.dispatcher.mailboxSize(actor))
        logger.trace("FlushAll start (%d entries)".format(cacheSize))
        cache --= cache.keys
        logger.trace("FlushAll complete")
        sender ! cacheSize
    }

    private def scheduleFlush: Unit = if (!flushScheduled) {
      actorSystem.scheduler.scheduleOnce(duration) {
        actor ! FlushAllBySchedule
      }

      flushScheduled = true
    }

    private def putToCache(pairs:  Iterable[(K, V)], semigroup: Semigroup[V]) {
      var putSize = 0
      for ((key, value) <- pairs) {
        putSize += 1
        cache.put(key, cache.get(key).map(current => current.withValue(semigroup.append(current.value, value))).getOrElse(ExpirableValue(value)))
      }

      //monitor.sample("put_size")(putSize)
    }

    private def removeEldestEntries {
      cache.removeEldestEntries(0.max(cache.size - maximumCapacity))
    }
  }

  private val actor: ActorRef = actorSystem.actorOf(Props(new StageActor))

  def += (k: K, v: V)(implicit sg: Semigroup[V]) = put(k, v)

  def += (tuple: (K, V))(implicit sg: Semigroup[V]) = put(tuple._1, tuple._2)

  def put(k: K, v: V)(implicit sg: Semigroup[V]) = actor ! PutAll((k, v) :: Nil, sg)

  def putAll(pairs: Iterable[(K, V)])(implicit sg: Semigroup[V]) = actor ! PutAll(pairs, sg)

  def flushAll(implicit timeout: Timeout): Future[Int] = (actor ? FlushAll).mapTo[Int]

  /** TODO: use an iteratee such that the state of the stage is not exposed. */
  private var stopFuture: Future[Unit] = _
  def stop(timeout: Timeout) = synchronized {
    if (stopFuture == null) {
      stopFuture = flushAll(timeout).flatMap(_ => ActorRefStop(actorSystem, timeout).stop(actor))
    }
    stopFuture
  }
}

object Stage {
  def apply[K, V](policy: ExpirationPolicy, capacity: Int, evict: (K, V) => Unit): Stage[K, V] = apply(policy, capacity)(evict)

  def apply[K, V](policy: ExpirationPolicy, capacity: Int, monitor: HealthMonitor = HealthMonitor.Noop)(evict: (K, V) => Unit): Stage[K, V] = new Stage[K, V](monitor) {
    def expirationPolicy = policy

    def maximumCapacity = capacity

    def flush(k: K, v: V): Unit = evict(k, v)
  }

  implicit def stop[K, V](implicit timeout: Timeout): Stop[Stage[K, V]] = new Stop[Stage[K, V]] {
    def stop(s: Stage[K, V]) = s.stop(timeout)
  }
}
