package blueeyes.health.metrics

import collection.mutable.ConcurrentMap
import java.util.concurrent.atomic.AtomicLong
import collection.JavaConversions._
import blueeyes.health.ConcurrentMaps._
import java.util.concurrent.ConcurrentHashMap

abstract class TimedSample[V](val config: interval)(implicit clock: () => Long) extends Histogram[V] with Statistic[Long, Map[Long, V]]{
  private val _rawData : ConcurrentMap[Double, AtomicLong] = new ConcurrentHashMap[Double, AtomicLong]

  val intervalLengthInSeconds           = config.granularity.unit.toSeconds(config.granularity.length)
  private val intervalLengthInMillis    = config.granularity.unit.toMillis(config.granularity.length)
  private val intervalFullLength        = intervalLengthInMillis * config.samples
  @volatile private var removeTime      = 0l

  updateRemoveTime()

  def rawData = _rawData.toMap.mapValues(_.get.longValue)

  def +=(elem: Long): this.type = {
    +=(container(clock().toDouble), elem)
    if (removeTime < clock()) removeExpired()

    this
  }

  protected def +=(value: AtomicLong, elem: Long)

  def count = _rawData.toMap.values.foldLeft(0l){(result, element) => result + element.get}

  def details: Map[Long, V] = {
    removeExpired()
    container((clock() - intervalFullLength).toDouble).addAndGet(0)
    build
  }

  override protected def bucket(sorted: List[(Double, Long)]) = intervalLengthInMillis.toInt

  private def container(key: Double) = createIfAbsent(key, _rawData, newAtomicLong _)
  private def newAtomicLong = new AtomicLong(0)

  private def removeExpired(){
    updateRemoveTime()

    val sortedKeys =_rawData.keys.toList.sorted
    val last       = clock() - intervalFullLength
    val expired    = sortedKeys.takeWhile(_ <= last)
    expired.foreach(_rawData.remove(_))
  }

  private def updateRemoveTime(){
    removeTime = clock() + intervalFullLength
  }
}

abstract class TimedNumbersSample(config: interval)(implicit clock: () => Long) extends TimedSample[Double](config) with SimpleHistogram{
  protected def +=(value: AtomicLong, elem: Long) {value.addAndGet(elem)}
}

abstract class EternityTimedNumbersSample(implicit clock: () => Long) extends Statistic[Long, Map[Long, Double]]{
  private val startTime = clock()
  private val _count    = new AtomicLong(0)

  def +=(element: Long) = {
    _count.getAndAdd(element)
    this
  }

  def count = _count.get

  def details = Map[Long, Double](startTime -> count)

  def intervalLengthInSeconds = (clock() - startTime) / 1000

  def config = eternity
}