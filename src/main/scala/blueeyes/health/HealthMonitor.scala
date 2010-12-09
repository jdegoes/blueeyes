package blueeyes.health

import blueeyes.json.JPath
import blueeyes.util.Future
import metrics._
import scala.collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import collection.mutable.ConcurrentMap

trait HealthMonitor{

  private val counters: ConcurrentMap[JPath, Counter] = new ConcurrentHashMap[JPath, Counter]
  private val timers:   ConcurrentMap[JPath, Timer]   = new ConcurrentHashMap[JPath, Timer]

  def count(path: JPath)(c: Long)       = counter(path).inc(c)

  def time[T](path: JPath)(f: => T): T  = timer(path).time(f)

  def futureTime[T](path: JPath)(f: Future[T]): Future[T] = timer(path).time(f)

  def error[T <: Throwable](path: JPath)(t: T): T = t

  def countStats = counters.toMap

  def timerStats = timers.toMap

  private def counter(path: JPath): Counter = statObject(path, counters,  {new Counter()})

  private def timer(path: JPath):   Timer   = statObject(path, timers,    {new Timer()})

  private def statObject[T](path: JPath, container: ConcurrentMap[JPath, T], factory: => T): T = {
    container.get(path).getOrElse({
      val statObject = factory
      container.putIfAbsent(path, statObject).getOrElse(statObject)
    })
  }
}

//trait HealthMonitor {
//  def sample[T](path: JPath)(m: Measurable[T]): T // Ints, longs, floats, and doubles, things that must be sampled & bucketed
//  def monitor[T](path: JPath)(f: Future[T]): Future[T]
//
//  def trap[T](f: => T): T
//}