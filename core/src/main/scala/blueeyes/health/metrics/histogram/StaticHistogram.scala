package blueeyes.health.metrics.histogram


import collection.mutable.HashMap
import scala.math.floor
import scala.math.round
import scala.math.max

class StaticHistogram[V](bucketStrategy: BucketsLengthStrategy)(implicit val valueStrategy: ValueStrategy[V]){
  def histogram(sortedData: List[(Double, Long)]) = {
    val bucketSize  = bucketStrategy.length(sortedData)

    val buckets = createBuckets(bucketSize, sortedData)

    fillMissing(bucketSize, sortedData, buckets).toMap
  }

  private def fillMissing(bucketSize: Int, sorted: List[(Double, Long)], buckets: HashMap[Long, V]): HashMap[Long, V] = {
    val minV          = floor(sorted.head._1).longValue
    val maxV          = floor(sorted.last._1).longValue
    var bucketNumber  = minV

    while (bucketNumber < maxV){
      val bucket = buckets.get(bucketNumber)

      if (!bucket.isDefined) buckets.put(bucketNumber, valueStrategy.zero)

      bucketNumber = bucketNumber + bucketSize
    }

    buckets
  }

  private def createBuckets(bucketSize: Int, sorted: List[(Double, Long)]) = {
    val buckets     = new HashMap[Long, V]()
    val minV        = floor(sorted.head._1).longValue

    sorted.foreach(kv => {
      val bucketNumber: Long = minV + (floor((kv._1 - sorted.head._1) / bucketSize).longValue * bucketSize)

      val bucket      = buckets.get(bucketNumber)
      if (kv._2 != 0){
        val bucketValue = valueStrategy.plus(bucket.getOrElse(valueStrategy.zero), kv._2)
        buckets.put(bucketNumber, bucketValue)
      }
    })

    buckets
  }
}

trait BucketsLengthStrategy{
  def length(sorted: List[(Double, Long)]): Int
}

class FixedLengthBucketsStrategy(size: Int) extends BucketsLengthStrategy{
  def length(sorted: List[(Double, Long)]) = size
}

class DynamicLengthBucketsStrategy extends BucketsLengthStrategy{
  def length(sorted: List[(Double, Long)]) = {
    val minV = sorted.head._1
    val maxV = sorted.last._1

    max(floor(minV), round((maxV - minV) / sorted.size)).intValue
  }
}