package blueeyes.persistence.cache

import scala.collection.mutable.ConcurrentMap

import java.lang.ref.WeakReference
import java.util.Set
import java.util.concurrent.{ConcurrentHashMap => JConcurrentHashMap, ConcurrentMap => JConcurrentMap, ScheduledExecutorService, TimeUnit}
import java.util.concurrent.TimeUnit.NANOSECONDS

/** A cache is a concurrent-safe key/value store where entries expire both
 * when the cache reaches a user-defined maximum size, and after user-defined
 * time to idle, time to live. Clients can listen to eviction to perform some
 * action (e.g. flushing to disk).
 * <p>
 * This cache is based on concurrent linked hash map (Benjamin Manes).
 */
class Cache[K, V](
  val delegate: JConcurrentMap[K, Expirable[K, V]], 
  val defaultPolicy: ExpirationPolicy, 
  val hasExpired: Expirable[K, V] => Boolean,
  val es: ScheduledExecutorService,
  val onEviction: (K, V) => Unit) extends ConcurrentMap[K, V] {

  import Cache.ExpirationTask

  override def size: Int = delegate.size

  override def clear() = delegate.clear()

  def containsKey(key: K): Boolean = {
    val expirable: Expirable[K, V] = delegate.get(key)
    if (expirable == null) false
    else {
      if (hasExpired(expirable)) {
        handleExpiration(expirable)
      
        false
      }
      else true
    }
  }

  def containsValue(value: V): Boolean = {
    val iterator = delegate.values.iterator
    
    while (iterator.hasNext) { 
      val expirable: Expirable[K, V] = iterator.next()
      
      if (expirable._value == value) {
        if (hasExpired(expirable)) {
          handleExpiration(expirable)
        }
        else {
          return true
        }
      }
    }
    
    false
  }

  def get(key: K): Option[V] = {
    val expirable: Expirable[K, V] = delegate.get(key)
      
    if (expirable == null) None
    else if (hasExpired(expirable)) { handleExpiration(expirable); None }
    else {
      scheduleTimeToIdle(expirable)
    
      Some(expirable.value)
    }
  }

  def putIfAbsent(key: K, value: V): Option[V] = {
    putIfAbsent(key, value, defaultPolicy)
  }

  def putIfAbsent(key: K, value: V, policy: ExpirationPolicy): Option[V] = {
    val expirable = Expirable[K, V](key, value, policy)
    val old       = delegate.putIfAbsent(key, expirable)
    
    if (old == null) {
      scheduleTimeToLive(expirable)
      scheduleTimeToIdle(expirable)
      None
    }
    else if (hasExpired(old)) {
      handleExpiration(old)
      putIfAbsent(key, value, policy)
    }
    else {
      scheduleTimeToIdle(expirable)
      
      Some(old.value)
    }
  }

  override def put(key: K, value: V): Option[V] = put(key, value, defaultPolicy)

  def put(key: K, value: V, policy: ExpirationPolicy): Option[V] = {
    val expirable: Expirable[K, V] = Expirable[K, V](key, value, policy)
    val old: Expirable[K, V] = delegate.put(key, expirable)
    
    scheduleTimeToLive(expirable)
    scheduleTimeToIdle(expirable)
    
    if (old == null) None
    else if (hasExpired(old)) { handleExpirationAfterRemoval(expirable); None }
    else Some(old.value)
  }
  
  def += (kv: (K, V)): Cache.this.type = {
    put(kv._1, kv._2)
    
    this
  }

  def -= (key: K): Cache.this.type = {
    remove(key)
    
    this
  }

  override def remove(key: K): Option[V] = {
    val old: Expirable[K, V] = delegate.remove(key)
    
    if (old == null) None
    else if (hasExpired(old)) { handleExpirationAfterRemoval(old); None }
    else Some(old.value)
  }

  def remove(key: K, value: V): Boolean = {
    val old: Expirable[K, V] = delegate.get(key)
    if (old == null) false
    else if (hasExpired(old)) { handleExpiration(old); false }
    else if (old.value == value) delegate.remove(key, old)
    else false
  }

  def replace(key: K, value: V): Option[V] = replace(key, value, defaultPolicy)

  def replace(key: K, value: V, policy: ExpirationPolicy): Option[V] = {
    val old: Expirable[K, V] = delegate.get(key)
    
    if (old == null) None
    else if (hasExpired(old)) { handleExpiration(old); None }
    else {
      val expirable = Expirable[K, V](key, value, policy)
      
      delegate.replace(key, expirable) match {
        case null => None
        
        case old =>
          scheduleTimeToLive(expirable)
          scheduleTimeToIdle(expirable)
          Some(old.value)
      }
    }
  }

  def replace(key: K, oldValue: V, newValue: V): Boolean = replace(key, oldValue, newValue, defaultPolicy)

  def replace(key: K, oldValue: V, newValue: V, policy: ExpirationPolicy): Boolean = {
    val old: Expirable[K, V] = delegate.get(key)
    
    if (old == null) false
    else if (hasExpired(old)) { handleExpiration(old); false }
    else if (!oldValue.equals(old._value)) false
    else {
      val expirable = Expirable[K, V](key, newValue, policy)
      
      if (delegate.replace(key, old, expirable)) {
        scheduleTimeToLive(expirable)
        scheduleTimeToIdle(expirable)
        true
      }
      else {
        false
      }
    }
  }

  private def scheduleTimeToIdle(expirable: Expirable[K, V]) = {
    expirable.policy.timeToIdle(NANOSECONDS) match {
      case None =>
      
      case Some(timeToIdle) =>
        es.schedule(new ExpirationTask[K, V](this, expirable), timeToIdle, NANOSECONDS)
    }
  }

  private def scheduleTimeToLive(expirable: Expirable[K, V]) = {
    expirable.policy.timeToLive(NANOSECONDS) match {
      case None =>
      
      case Some(timeToLive) =>
        es.schedule(new ExpirationTask[K, V](this, expirable), timeToLive, NANOSECONDS)
    }
  }

  private def handleExpiration(expirable: Expirable[K, V]) = {
    if (delegate.remove(expirable.key, expirable)) {
      handleExpirationAfterRemoval(expirable)
    }
  }

  private def handleExpirationAfterRemoval(expirable: Expirable[K, V]) = {
    onEviction(expirable.key, expirable._value)
  }
  
  def iterator: Iterator[(K, V)] = {
    val javaIterator = delegate.entrySet.iterator
    
    new Iterator[(K, V)] {
      def hasNext: Boolean = javaIterator.hasNext
      
      def next = { val n = javaIterator.next; (n.getKey, n.getValue._value)}
    }
  }
}

case class CacheSettings[K, V](
  expirationPolicy:         ExpirationPolicy,
  concurrencyLevel:         Int                     = 10,
  initialCapacity:          Int                     = 1000,
  onEviction:               (K, V) => Unit          = (k: K, v: V) => (),
  maximumWeightedCapacity:  Int                     = 10000,
  weigher:                  V => Int                = (v: V) => 1
)

object Cache {
  import java.util.concurrent.Executors
  
  lazy val ScheduledExecutor = Executors.newScheduledThreadPool(1)
  
  private[cache] sealed class ExpirationTask[K, V](map: Cache[K, V], expirable: Expirable[K, V]) extends Runnable {
    private final val expirableRef  = new WeakReference[Expirable[K, V]](expirable)
    private final val mapRef        = new WeakReference[Cache[K, V]](map)

    def run() = {
      (mapRef.get, expirableRef.get) match {
        case (map: Cache[K, V], expirable: Expirable[K, V]) =>
          if (map.hasExpired(expirable)) {
            map.handleExpiration(expirable)
          }
      }
    }
  }
  
  def apply[K, V](policy: ExpirationPolicy): Cache[K, V] = apply(CacheSettings[K, V](expirationPolicy = policy))
  
  def apply[K, V](settings: CacheSettings[K, V]): Cache[K, V] = {
    import com.googlecode.concurrentlinkedhashmap.{ConcurrentLinkedHashMap, CapacityLimiter, Weigher, EvictionListener}
    
    lazy val cache: Cache[K, V] = new Cache[K, V](
      new ConcurrentLinkedHashMap.Builder[K, Expirable[K, V]]().
      concurrencyLevel(settings.concurrencyLevel).
      initialCapacity(settings.initialCapacity).
      listener(new EvictionListener[K, Expirable[K, V]]() {
        def onEviction(k: K, v: Expirable[K, V]) = settings.onEviction(k, v._value)
      }).maximumWeightedCapacity(settings.maximumWeightedCapacity).
      weigher(new Weigher[Expirable[K, V]] {
        def weightOf(v: Expirable[K, V]): Int = settings.weigher(v._value)
      }: Weigher[Expirable[K, V]]).build(),
      settings.expirationPolicy,
      new ExpirationPredicate[K, V](),
      ScheduledExecutor,
      settings.onEviction
    )
    
    cache
  }
}