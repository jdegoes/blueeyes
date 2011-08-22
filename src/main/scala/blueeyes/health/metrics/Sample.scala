package blueeyes.health.metrics

import java.util.concurrent.atomic.AtomicLong
import scala.collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import blueeyes.health.ConcurrentMaps
import collection.mutable.ConcurrentMap
import ConcurrentMaps._
import blueeyes.json.JsonAST._

class Sample(val size: Int) extends Histogram with Statistic[Double, Option[Map[Long, Long]]]{
  private val _count = new AtomicLong(0)
  private val _rawData : ConcurrentMap[Double, AtomicLong] = new ConcurrentHashMap[Double, AtomicLong]

  private val lock = new java.util.concurrent.locks.ReentrantReadWriteLock
  private var _histogram: Option[Map[Long, Long]] = None

  def +=(elem: Double): this.type = {
    if (count < size){
      container(elem).addAndGet(1)
      _count.addAndGet(1)
    }
    this
  }

  def count = _count.get

  def rawData = _rawData.toMap.mapValues(_.get.intValue)

  def details: Option[Map[Long, Long]] = {
    writeLock{
      if (count >= size && !_histogram.isDefined){
        _histogram = Some(build)
      }
      _histogram
    }
  }

  def toJValue = {
    val histogramJValue: List[JField] = details.map(v => v.toList.map(kv => JField(kv._1.toString, JInt(kv._2)))).map(fs => JField("histogram", JObject(fs)) :: Nil).getOrElse(Nil)

    JObject(JField("count", JInt(count)) :: histogramJValue)
  }

  private def writeLock[S](f: => S): S = {
    lock.writeLock.lock()
    try {
      f
    }
    finally {
      lock.writeLock.unlock()
    }
  }

  private def container(key: Double) = createIfAbsent(key, _rawData, newAtomicLong _)
  private def newAtomicLong = new AtomicLong(0)
}