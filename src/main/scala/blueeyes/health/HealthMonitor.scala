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
  private val _sampleStats: ConcurrentMap[JPath, Sample]    = new ConcurrentHashMap[JPath, Sample]

  def count(path: JPath)(c: Long) = counterStat(path).inc(c)

  def time[T](path: JPath)(f: => T): T = timerStat(path).time(f)

  def futureTime[T](path: JPath)(f: Future[T]): Future[T] = timerStat(path).time(f)

  def error[T <: Throwable](path: JPath)(t: T): T = errorStat(path) += t

  def sample(path: JPath)(d: Double) = sampleStat(path) += d  

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

  def countStats  = _countsStats.toMap

  def timerStats  = _timersStats.toMap

  def sampleStats = _sampleStats.toMap

  def errorStats  = _errorsStats.toMap

  def sampleSize: Int

  private def toMongoField(path: JPath) = if (path.path.startsWith(".")) path.path.substring(1) else path.path

  private def sampleStat(path: JPath):  Sample    = createIfAbsent(path, _sampleStats, {new Sample(sampleSize)})

  private def counterStat(path: JPath): Counter   = createIfAbsent(path, _countsStats, {new Counter()})

  private def timerStat(path: JPath):   Timer     = createIfAbsent(path, _timersStats, {new Timer()})

  private def errorStat(path: JPath):   ErrorStat = createIfAbsent(path, _errorsStats, {new ErrorStat()})
}