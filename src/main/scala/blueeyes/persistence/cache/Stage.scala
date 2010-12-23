package blueeyes.persistence.cache

import scala.collection.mutable.Map
import scala.actors.Actor

import blueeyes.util.Future

/** A stage is a particular kind of cache that is used for staging IO updates.
 * Many kinds of IO updates can be combined (e.g. instead of writing a single
 * log line to a file, you can collect ten lines and write them all at once).
 * This has the capacity to greatly improve performance when IO is the primary
 * bottleneck.
 * <p>
 * Built on a cache, stage supports standards eviction, settings such as time 
 * to idle, time to live, and maximum weighted capacity.
 */
class Stage[K, V](settings: CacheSettings[K, V], coalesce: (K, V, V) => V) extends Map[K, V] {
  private type This = this.type
  
  private sealed trait Request
  private sealed trait Response
  
  private case class  Get(k: K) extends Request
  private case class  Add(k: K, v: V) extends Request
  private case class  Remove(k: K) extends Request
  private case object GetAll extends Request
  private case object Stop extends Request
  
  private case class  Got(v: Option[V]) extends Response
  private case class  Added(old: Option[V]) extends Response
  private case class  Removed(v: Option[V]) extends Response
  private case class  GotAll(list: List[(K, V)]) extends Response
  private case object Stopped extends Response
  
  private val actor = new Actor {
    val accumulator: Map[K, V] = Cache.nonConcurrent(settings)
    
    var running: Boolean = false
    
    override def start = {
      running = true
      
      super.start
    }
    
    def act = {
      while (running) {
        try receive {
          case Get(k) =>
            Got(accumulator.remove(k))

          case Add(k, v1) =>
            Added(
              accumulator.put(k, 
                accumulator.get(k) match {
                  case None     => v1
                  case Some(v2) => coalesce(k, v1, v2)
                }
              )
            )

          case Remove(k) =>
            Removed(accumulator.remove(k))
            
          case Stop =>
            accumulator.foreach { entry =>
              settings.evict(entry._1, entry._2)
            }
            
            running = false
            
            Stopped
            
          case GetAll =>
            GotAll(accumulator.toList)
        }
        catch { case e => e.printStackTrace }
      }
    }
  }
  
  actor.start
  
  def get(key: K): Option[V] = actor !? Get(key) match {
    case Got(v) => v
  }
  
  /** Asynchronously retrieves the value for the specified key.
   */
  def getLater(key: K): Future[Option[V]] = {
    val future = new Future[Option[V]]
    
    (actor !! Get(key)) foreach { value =>
      value match {
        case Got(v) => future.deliver(v)
      }
    }
    
    future
  }
  
  def iterator: Iterator[(K, V)] = actor !? Stop match {
    case GotAll(all) => all.iterator
  }
  
  /** Removes an entry from the stage. Note: this does not call the evicter on 
   * the entry.
   */
  def -= (key: K): This = {
    actor !? Remove(key) match {
      case Removed(_) => 
    }
    
    this
  }
  
  def += (kv: (K, V)): This = {
    actor !? Add(kv._1, kv._2) match {
      case Added(_) => 
    }
    
    this
  }
  
  /** Starts the stage. This function is called automatically when the stage 
   * is created.
   */
  def start = actor.start
  
  /** Stops the stage and evicts all entries.
   */
  def stop = actor !? Stop match {
    case Stopped =>
  }
}