package blueeyes.health

import blueeyes.json.JPath
import blueeyes.util.Future
import metrics._
import scala.collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import collection.mutable.ConcurrentMap

trait HealthMonitor{
//
  private val counters: ConcurrentMap[String, Int] = new ConcurrentHashMap[String, Int]
  private val timer   = new Timer()

//  def count(path: JPath)(c: Long) = counter(path).inc(c)
//
//  private def counter(path: JPath): Counter = {
//    counters.get(path).getOrElse({
//      val counter = new Counter()
//      counters.putIfAbsent(path, counter).getOrElse(counter)
//    })
//  }

  def time[T](path: JPath)(f: => T): T              = timer.time(f)

  def time[T](path: JPath)(f: Future[T]): Future[T] = timer.time(f)

  def error[T <: Throwable](path: JPath)(t: T): T = t
}

//trait HealthMonitor {
//  def sample[T](path: JPath)(m: Measurable[T]): T // Ints, longs, floats, and doubles, things that must be sampled & bucketed
//  def monitor[T](path: JPath)(f: Future[T]): Future[T]
//
//  def trap[T](f: => T): T
//}