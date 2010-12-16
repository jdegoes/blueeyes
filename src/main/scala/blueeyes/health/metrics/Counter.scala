package blueeyes.health.metrics

import java.util.concurrent.atomic.AtomicLong
import blueeyes.json.JsonAST.{JValue, JInt}

/**
 * A thread-safe counter which can go up or down from its initial value.
 *
 * @author coda
 */
class Counter(initial: Long) extends Statistic[Long, Long]{
  protected val value = new AtomicLong(initial)

  /**
   * Creates a new counter with an initial value of zero.
   */
  def this() = this(0)

  /**
   * Increments the counter by an arbitrary amount.
   */
  def +=(delta: Long): this.type = {
    value.getAndAdd(delta)

    this
  }

  /**
   * Returns the current count.
   */
  def count = value.get

  def details = count

  def toJValue: JValue =  JInt(count)
}