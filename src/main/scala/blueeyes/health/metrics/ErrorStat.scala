package blueeyes.health.metrics

import java.util.concurrent.atomic.AtomicLong
import scala.collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import collection.mutable.ConcurrentMap

class ErrorStat{
  private val count_ = new AtomicLong(0)
  private val distribution_ : ConcurrentMap[Class[_], AtomicLong] = new ConcurrentHashMap[Class[_], AtomicLong]

  def error[T <: Throwable](t: T): T = {
    count_.getAndAdd(1)

    statObject(t.getClass).getAndAdd(1)

    t
  }

  private def statObject[V](path: Class[V]): AtomicLong = {
    distribution_.get(path).getOrElse({
      val statObject = new AtomicLong(0)
      distribution_.putIfAbsent(path, statObject).getOrElse(statObject)
    })
  }

  def count = count_.get

  def distribution = distribution_.toMap
}