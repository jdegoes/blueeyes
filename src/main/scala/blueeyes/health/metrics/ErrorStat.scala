package blueeyes.health.metrics

import java.util.concurrent.atomic.AtomicLong
import scala.collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import collection.mutable.ConcurrentMap
import blueeyes.health.ConcurrentMaps
import ConcurrentMaps._
import blueeyes.json.JsonAST._

class ErrorStat extends Statistic[Throwable, Map[Class[_], Long]]{
  private val _count = new AtomicLong(0)
  private val _distribution : ConcurrentMap[Class[_], AtomicLong] = new ConcurrentHashMap[Class[_], AtomicLong]

  def +=(t: Throwable): this.type = {
    _count.getAndAdd(1)

    createIfAbsent(t.getClass, _distribution, {new AtomicLong(0)}).getAndAdd(1)

    this
  }

  def count = _count.get

  def details: Map[Class[_], Long] = _distribution.toMap.mapValues(_.get)

  def toJValue: JValue = {
    val distributionJValue = details.toList.map(kv => JField(kv._1.getName, JInt(kv._2)))
    JObject(JField("errorCount", JInt(count)) :: JField("errorDistribution", JObject(distributionJValue)) :: Nil)
  }

}