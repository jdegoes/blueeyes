package blueeyes.health.metrics

import histogram.{DynamicLengthBucketsStrategy, StaticHistogram}
import java.util.concurrent.atomic.AtomicLong
import scala.collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import blueeyes.health.ConcurrentMaps
import collection.mutable.ConcurrentMap
import ConcurrentMaps._
import blueeyes.json.JsonAST._

import histogram.ValueStrategy._

class Sample(val size: Int) extends SyncStatistic[Double, Option[Map[Long, Double]]]{
  private val histogram = new StaticHistogram[Double](new DynamicLengthBucketsStrategy())
  private val _count    = new AtomicLong(0)
  private val _rawData : ConcurrentMap[Double, AtomicLong] = new ConcurrentHashMap[Double, AtomicLong]

  private val lock = new java.util.concurrent.locks.ReentrantReadWriteLock
  private var _histogram: Option[Map[Long, Double]] = None

  def +=(elem: Double): this.type = {
    if (count < size){
      container(elem).addAndGet(1)
      _count.addAndGet(1)
    }
    this
  }

  def count = _count.get

  def rawData = _rawData.toMap.mapValues(_.get.longValue)

  def details: Option[Map[Long, Double]] = {
    writeLock{
      if (count >= size && !_histogram.isDefined){
        _histogram = Some(histogram.histogram(_rawData.toList.map(v => (v._1, v._2.get.longValue)).toList.sortWith((e1, e2) => (e1._1 < e2._1))))
      }
      _histogram
    }
  }

  def toJValue = {
    val histogramJValue: List[JField] = details.map(v => v.toList.map(kv => JField(kv._1.toString, JNum(kv._2)))).map(fs => JField("histogram", JObject(fs)) :: Nil).getOrElse(Nil)

    JObject(JField("count", JNum(count)) :: histogramJValue)
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