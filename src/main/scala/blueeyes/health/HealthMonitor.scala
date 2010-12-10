package blueeyes.health

import blueeyes.json.JPath
import blueeyes.util.Future
import metrics._
import scala.collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import collection.mutable.ConcurrentMap

trait HealthMonitor extends ConcurrentMaps{

  private val _countsStats: ConcurrentMap[JPath, Counter]   = new ConcurrentHashMap[JPath, Counter]
  private val _timersStats: ConcurrentMap[JPath, Timer]     = new ConcurrentHashMap[JPath, Timer]
  private val _errorsStats: ConcurrentMap[JPath, ErrorStat] = new ConcurrentHashMap[JPath, ErrorStat]

  def count(path: JPath)(c: Long) = counter(path).inc(c)

  def time[T](path: JPath)(f: => T): T = timer(path).time(f)

  def futureTime[T](path: JPath)(f: Future[T]): Future[T] = timer(path).time(f)

  def error[T <: Throwable](path: JPath)(t: T): T = errorStat(path).error(t)

  def monitor[T](path: JPath)(f: Future[T]): Future[T] = {
    f.ifCanceled(_.foreach(error(path)(_)))    
    futureTime(path)(f)
  }

  def trap[T](path: JPath)(f: => T): T = {
    try {
      f
    }
    catch {
      case t: Throwable => error(path)(t)
      throw t
    }
  }
                                        
//  def sample[T](path: JPath)(m: Measurable[T]): T // Ints, longs, floats, and doubles, things that must be sampled & bucketed

  def countStats = _countsStats.toMap

  def timerStats = _timersStats.toMap

  def errorStats = _errorsStats

  private def counter(path: JPath):   Counter = createIfAbsent(path, _countsStats, {new Counter()})

  private def timer(path: JPath):     Timer   = createIfAbsent(path, _timersStats, {new Timer()})

  private def errorStat(path: JPath): ErrorStat = createIfAbsent(path, _errorsStats, {new ErrorStat()})
}