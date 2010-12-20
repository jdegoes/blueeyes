package blueeyes.health.metrics

import java.util.concurrent.atomic.AtomicLong
import scala.collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import blueeyes.health.ConcurrentMaps
import scala.math.floor
import scala.math.round
import scala.math.max
import collection.mutable.{HashMap, ConcurrentMap}
import ConcurrentMaps._
import blueeyes.json.JsonAST._

class Sample(val size: Int) extends Histogram with Statistic[Double, Option[Map[Int, Int]]]{
  private val _count = new AtomicLong(0)
  private val _rawData : ConcurrentMap[Double, AtomicLong] = new ConcurrentHashMap[Double, AtomicLong]

  private val lock = new java.util.concurrent.locks.ReentrantReadWriteLock
  private var _histogram: Option[Map[Int, Int]] = None

  def +=(elem: Double): this.type = {

    if (count < size){
      
      container(elem).addAndGet(1)
      val currentCount = _count.addAndGet(1)
    }

    this
  }

  def count = _count.get

  def rawData = _rawData.toMap.mapValues(_.get.intValue)

  def details: Option[Map[Int, Int]] = {
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

  private def container(key: Double) = createIfAbsent(key, _rawData, {new AtomicLong(0)})
}

trait Histogram{
  def rawData: Map[Double, Int]

  def build = {
    val sorted      = rawData.toList.sortWith((e1, e2) => (e1._1 < e2._1))
    val bucketSize  = bucket(sorted)

    val buckets = createBuckets(bucketSize, sorted)

    fillMissing(bucketSize, sorted, buckets).toMap
  }

  private def fillMissing(bucketSize: Int, sorted: List[(Double, Int)], buckets: HashMap[Int, Int]): HashMap[Int, Int] = {
    val minV          = floor(sorted.head._1).intValue
    val maxV          = floor(sorted.last._1).intValue
    var bucketNumber  = minV

    while (bucketNumber < maxV){
      val bucket = buckets.get(bucketNumber)
      
      if (!bucket.isDefined) buckets.put(bucketNumber, 0)

      bucketNumber = bucketNumber + bucketSize
    }

    buckets
  }

  private def createBuckets(bucketSize: Int, sorted: List[(Double, Int)]) = {
    val buckets     = new HashMap[Int, Int]()
    val minV        = floor(sorted.head._1).intValue

    sorted.foreach(kv => {
      val bucketNumber: Int = minV + (floor((kv._1 - sorted.head._1) / bucketSize).intValue * bucketSize)

      val bucket = buckets.get(bucketNumber)
      buckets.put(bucketNumber, bucket.getOrElse(0) + kv._2)
    })

    buckets
  }

  private def bucket(sorted: List[(Double, Int)]) = {
    val minV = sorted.head._1
    val maxV = sorted.last._1

    max(floor(minV), round((maxV - minV) / sorted.size)).intValue
  }
}