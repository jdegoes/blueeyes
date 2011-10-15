package blueeyes.health.metrics.histogram

import blueeyes.persistence.cache.functional.TemporalCache
import java.util.concurrent.TimeUnit

case class DynamicHistogram[V] private (private val cache: TemporalCache[Long, V], length: Int, capacity: Int, unit: TimeUnit)(implicit valueStrategy: ValueStrategy[V], clock: () => Long) {
  def +=(timeMs: Long, stat: Long) = {
    val bucket   = bucketNumber(timeMs)
    val value    = cache.get(bucket).getOrElse(valueStrategy.zero)
    val newValue = valueStrategy.plus(value, stat)

    copy(cache = expire(bucket, cache.put((bucket, newValue), bucket)))
  }

  def histogram: Map[Long, V] = {
    val lastBucket = bucketNumber(clock())
    val newCache   = expire(lastBucket, cache)
    val value      = newCache.keys.foldLeft(Map[Long, V]()){(value, key) => value + Tuple2(key, cache.get(key).get)}

    fillMissing(lastBucket, value)
  }

  def count = histogram.values.foldLeft(0l){(result, value) => result + valueStrategy.count(value)}

  private def bucketNumber(ms: Long) = {
    val time = unit.convert(ms, TimeUnit.MILLISECONDS)
    time - (time % length)
  }

  private def fillMissing(lastBucket: Long, histogram: Map[Long, V]) = {
    val nextBucket = lastBucket + length
    val allKeys    = List.range(nextBucket - capacity * length, nextBucket, length)

    allKeys.foldLeft(histogram){(value, key) => histogram.get(key).map(v => value).getOrElse(value + Tuple2(key, valueStrategy.zero))}
  }

  private def expire(lastBucket: Long, cache: TemporalCache[Long, V]) = {
    val expireBucket = lastBucket - length * capacity + 1
    cache.expire(expireBucket, expireBucket).retained
  }
}

object DynamicHistogram {
  def empty[V](length: Int, capacity: Int, unit: TimeUnit)(implicit valueStrategy: ValueStrategy[V], clock: () => Long): DynamicHistogram[V] = DynamicHistogram[V](TemporalCache.empty[Long, V], length, capacity, unit)
}