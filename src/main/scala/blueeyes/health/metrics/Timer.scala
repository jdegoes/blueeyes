package blueeyes.health.metrics

import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.TimeUnit
import java.lang.Double.{doubleToLongBits, longBitsToDouble}
import scala.math.sqrt
import blueeyes.concurrent.{Future, Duration}
import Duration._
import blueeyes.json.JsonAST._
import blueeyes.util.ClockSystem

/**
 * A class which tracks the amount of time it takes to perform a particular
 * action and calculates various statistics about the distribution of durations.
 *
 * @author coda
 */
class Timer extends Statistic[Duration, Tuple5[Long, Duration, Duration, Duration, Duration]] with ClockSystem{
  private val count_ = new AtomicLong(0)
  private val min_ = new AtomicLong(Long.MaxValue)
  private val max_ = new AtomicLong(Long.MinValue)
  private val sum_ = new AtomicLong(0)
  private val varianceM = new AtomicLong(-1)
  private val varianceS = new AtomicLong(0)

  /**
   * Record the amount of time it takes to execute the given function.
   *
   * @return the result of T
   */
  def time[T](f: => T): T = {
    val startTime = clockSystem.nanoTime
    val t = f
    this += (clockSystem.nanoTime - startTime).nanoseconds
    return t
  }

  def time[T](f: Future[T]): Future[T] = {
    val startTime = clockSystem.nanoTime
    f.deliverTo(v => {
      this += (clockSystem.nanoTime - startTime).nanoseconds
    })
  }

  /**
   * Returns the number of measurements taken.
   */
  def count = count_.get


  def details = (count, max, min, mean, standardDeviation)

  /**
   * Returns the sum time recorded.
   */
  def total = safeNS(sum_.get)

  /**
   * Returns the greatest amount of time recorded.
   */
  def max = safeNS(max_.get)

  /**
   * Returns the least amount of time recorded.
   */
  def min = safeNS(min_.get)

  /**
   * Returns the arthimetic mean of the recorded durations.
   */
  def mean = safeNS(sum_.get / count.toDouble)

  /**
   * Returns the standard deviation of the recorded durations.
   */
  def standardDeviation = safeNS(sqrt(variance))

  /**
   * Adds a timing in nanoseconds.
   */
  def +=(ns: Long): this.type = {
    if (ns >= 0) {
      count_.incrementAndGet
      setMax(ns)
      setMin(ns)
      sum_.getAndAdd(ns)
      updateVariance(ns)
    }
    this
  }

  /**
   * Adds a duration recorded elsewhere.
   */
  def +=(duration: Duration): this.type = {
    this += duration.ns.time.toLong

    this
  }

  def toJValue: JValue = JObject(JField("perSecond", JDouble((count / total.convert(TimeUnit.SECONDS).time))) :: JField("minimumTime", JDouble(min.convert(TimeUnit.MILLISECONDS).time)) :: JField("maximumTime", JDouble(max.convert(TimeUnit.MILLISECONDS).time)) :: JField("averageTime", JDouble(mean.convert(TimeUnit.MILLISECONDS).time)) :: JField("standardDeviation", JDouble(standardDeviation.convert(TimeUnit.MILLISECONDS).time)) :: Nil)

  private def updateVariance(ns: Long) {
    // initialize varianceM to the first reading if it's still blank
    if (!varianceM.compareAndSet(-1, doubleToLongBits(ns))) {
      var updated = false
      while (!updated) {
        val oldMCas = varianceM.get
        val oldM = longBitsToDouble(oldMCas)
        val newM = oldM + ((ns - oldM) / count)

        val oldSCas = varianceS.get
        val oldS = longBitsToDouble(oldSCas)
        val newS = oldS + ((ns - oldM) * (ns - newM))

        updated = varianceM.compareAndSet(oldMCas, doubleToLongBits(newM)) &&
                  varianceS.compareAndSet(oldSCas, doubleToLongBits(newS))
      }
    }
  }

  private def variance = if (count > 1) {
    longBitsToDouble(varianceS.get) / (count - 1)
  } else {
    0.0
  }

  private def ratio(unit: TimeUnit) = TimeUnit.NANOSECONDS.convert(1, unit).toDouble

  private def setMax(duration: Long) {
    while (max_.get < duration) {
      max_.compareAndSet(max_.get, duration)
    }
  }

  private def setMin(duration: Long) {
    while (min_.get > duration) {
      min_.compareAndSet(min_.get, duration)
    }
  }

  private def safeNS(f: => Double) = {
    if (count > 0) {
      f.nanoseconds
    } else {
      0.nanoseconds
    }
  }
}
