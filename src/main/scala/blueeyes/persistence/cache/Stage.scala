package blueeyes.persistence.cache

import scala.collection.mutable.Map
import scala.actors.Actor

import blueeyes.concurrent.Future

/** A stage is a particular kind of cache that is used for staging IO updates.
 * Many kinds of IO updates can be combined (e.g. instead of writing a single
 * log line to a file, you can collect ten lines and write them all at once).
 * This has the capacity to greatly improve performance when IO is a limiting
 * factor.
 * <p>
 * Built on a cache, stage supports standards eviction, settings such as time
 * to idle, time to live, and maximum weighted capacity.
 * <p>
 * Stopping a stage evicts all entries from the stage. As part of shutdown, in
 * order to avoid data loss, every stage should be stopped.
 */
trait Stage[K, V] extends Map[K, V] {
  def settings: CacheSettings[K, V]

  def coalesce: (K, V, V) => V

  private type This = this.type

  private sealed trait Request
  private sealed trait Response

  private case class  Get(k: K) extends Request
  private case class  Add(k: K, v: V) extends Request
  private case class  Remove(k: K) extends Request
  private case object GetAll extends Request
  private case object Stop extends Request

  private case class  Got(v: Option[V]) extends Response
  private case class  GotAll(list: List[(K, V)]) extends Response
  private case object Stopped extends Response

  private lazy val actor: Actor = new Actor { self =>
    lazy val accumulator: Map[K, V] = Cache.concurrent(settings)

    def act = {
      loop {
        try receive {
          case Get(k) =>
            reply(Got(accumulator.get(k)))

          case Add(k, v2) =>
            accumulator.put(k,
              accumulator.get(k) match {
                case None     => v2
                case Some(v1) => coalesce(k, v1, v2)
              }
            )

          case Remove(k) =>
            accumulator.remove(k)

          case Stop =>
            accumulator.foreach { entry =>
              settings.evict(entry._1, entry._2)
            }

            accumulator.clear()

            reply(Stopped)

            exit()

          case GetAll =>
            reply(GotAll(accumulator.toList))
        }
        catch { case e => e.printStackTrace }
      }
    }
  }

  def get(key: K): Option[V] = actor !? Get(key) match {
    case Got(v) => v
  }

  /** Asynchronously retrieves the value for the specified key.
   */
  def getLater(key: K): Future[Option[V]] = (actor !! (Get(key), {case Got(v) => v}))

  def iterator: Iterator[(K, V)] = actor !? GetAll match {
    case GotAll(all) => all.iterator
  }

  /** Removes an entry from the stage. Note: this does not call the evicter on
   * the entry.
   */
  def -= (key: K): This = {
    actor ! Remove(key)

    this
  }

  def += (kv: (K, V)): This = {
    actor ! Add(kv._1, kv._2)

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

object Stage {
  def apply[K, V](settings_ : CacheSettings[K, V], coalesce_ : (K, V, V) => V) = new Stage[K, V] {
    def settings = settings_

    def coalesce = coalesce_

    start
  }
}