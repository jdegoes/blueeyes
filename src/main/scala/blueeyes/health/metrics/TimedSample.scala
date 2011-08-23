package blueeyes.health.metrics

import collection.mutable.ConcurrentMap
import java.util.concurrent.atomic.AtomicLong
import collection.JavaConversions._
import blueeyes.health.ConcurrentMaps._
import blueeyes.json.JsonAST._
import java.util.concurrent.ConcurrentHashMap

private[metrics] trait TimedSampleReport extends  Statistic[Long, Map[Long, Double]]{
  def toJValue = {
    val (intervalLength, intervalCount) = intervalConfig match{
      case interval(length, count)  => (length.toString, count)
      case eternity => ("eternity", 1)
    }
    val histogramJValue = details.toList.sortWith((e1, e2) => (e1._1 > e2._1)).map(kv => JField(kv._1.toString, JDouble(kv._2 / intervalLengthInSeconds)))
    JObject(JField("interval", JObject(JField("length", JString(intervalLength)) :: JField("count", JInt(intervalCount)) :: Nil)) :: JField("perSecond", JObject(histogramJValue)) :: Nil)
  }
  protected def intervalLengthInSeconds: Long
  protected def intervalConfig: IntervalConfig
}

class TimedSample(val intervalConfig: interval)(implicit clock: () => Long) extends Histogram with Statistic[Long, Map[Long, Double]] with TimedSampleReport{
  private val _rawData : ConcurrentMap[Double, AtomicLong] = new ConcurrentHashMap[Double, AtomicLong]

  protected val intervalLengthInSeconds = intervalConfig.length.unit.toSeconds(intervalConfig.length.length)
  private val intervalLengthInMillis    = intervalConfig.length.unit.toMillis(intervalConfig.length.length)
  private val intervalFullLength        = intervalLengthInMillis * intervalConfig.count

  def rawData = _rawData.toMap.mapValues(_.get.intValue)

  def +=(elem: Long): this.type = {
    container(clock().toDouble).addAndGet(elem)
    removeExpired()
    this
  }

  def count = _rawData.size

  def details: Map[Long, Double] = {
    removeExpired()
    container((clock() - intervalFullLength).toDouble).addAndGet(0)
    build
  }

  override protected def bucket(sorted: List[(Double, Long)]) = intervalLengthInMillis.toInt

  private def container(key: Double) = createIfAbsent(key, _rawData, newAtomicLong _)
  private def newAtomicLong = new AtomicLong(0)

  private def removeExpired(){
    val sortedKeys =_rawData.keys.toList.sorted
    val last       = clock() - intervalFullLength
    val expired    = sortedKeys.takeWhile(_ <= last)
    expired.foreach(_rawData.remove(_))
  }
}

class EternityTimedSample(implicit clock: () => Long) extends Statistic[Long, Map[Long, Double]] with TimedSampleReport{
  private val startTime = clock()
  private val _count    = new AtomicLong(0)

  def +=(element: Long) = {
    _count.getAndAdd(element)
    this
  }

  def count = _count.get

  def details = Map[Long, Double](startTime -> count)

  protected def intervalLengthInSeconds = (clock() - startTime) / 1000

  protected def intervalConfig = eternity
}

object TimedSample{
  def apply(intervalConfig: IntervalConfig)(implicit clock: () => Long): Statistic[Long, Map[Long, Double]] = intervalConfig match{
    case e: interval => new TimedSample(e)
    case eternity    => new EternityTimedSample()
  }
}