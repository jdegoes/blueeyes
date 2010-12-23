package blueeyes.health

import blueeyes.json.JPath
import blueeyes.util.Future
import metrics._
import scala.collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import collection.mutable.ConcurrentMap
import ConcurrentMaps._
import blueeyes.json.JsonAST.{JValue, JField, JObject}

class HealthMonitor{

  private val _countsStats: ConcurrentMap[JPath, Counter]   = new ConcurrentHashMap[JPath, Counter]
  private val _timersStats: ConcurrentMap[JPath, Timer]     = new ConcurrentHashMap[JPath, Timer]
  private val _errorsStats: ConcurrentMap[JPath, ErrorStat] = new ConcurrentHashMap[JPath, ErrorStat]
  private val _sampleStats: ConcurrentMap[JPath, Sample]    = new ConcurrentHashMap[JPath, Sample]

  def increment(path: JPath)(c: Long): Unit = counterStat(path) += c

  def count(path: JPath)                    = increment(path)(1)

  def time[T](path: JPath)(f: => T): T      = timerStat(path).time(f)

  def timeFuture[T](path: JPath)(f: Future[T]): Future[T] = timerStat(path).time(f)

  def sample(path: JPath)(d: Double): Unit = sampleStat(path) += d

  def error[T <: Throwable](path: JPath)(t: T): T = {
    errorStat(path) += t

    t
  }

  def monitor[T](path: JPath)(f: Future[T]): Future[T] = {
    trapFuture(path)(f)
    timeFuture(path)(f)
  }

  def trapFuture[T](path: JPath)(f: Future[T]): Future[T] = {
    f.ifCanceled(_.foreach(error(path)(_)))
    f
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

  def toJValue: JValue = {
    val statistics = List[Map[JPath, Statistic[_, _]]](timerStats, errorStats, sampleStats, countStats)
    statistics.foldLeft(JObject(Nil)) {(result, element) => result.merge(composeStatistics(element)).asInstanceOf[JObject]}
  }

  def composeStatistics[T](stat: Map[JPath, Statistic[_, _]]) = {
    val jObjects = stat.toList.map(kv => jvalueToJObject(kv._1, kv._2.toJValue))
    jObjects.foldLeft(JObject(Nil)){(result, element) => result.merge(element).asInstanceOf[JObject]}
  }

  private def jvalueToJObject(path: JPath, value: JValue): JObject = {
    val elements = normalizePath(path).split("\\.").reverse
    elements.tail.foldLeft(JObject(JField(elements.head, value) :: Nil)){(result, element) => JObject(JField(element, result) :: Nil)}
  }

  private val sampleSize = 1000

  private def normalizePath(path: JPath) = if (path.path.startsWith(".")) path.path.substring(1) else path.path

  private def sampleStat(path: JPath):  Sample    = createIfAbsent(path, _sampleStats, {new Sample(sampleSize)})

  private def counterStat(path: JPath): Counter   = createIfAbsent(path, _countsStats, {new Counter()})

  private def timerStat(path: JPath):   Timer     = createIfAbsent(path, _timersStats, {new Timer()})

  private def errorStat(path: JPath):   ErrorStat = createIfAbsent(path, _errorsStats, {new ErrorStat()})
}

//trait HealthMonitorsImplicits{
//  implicit def serializableHealthMonitorsSugar(monitors: List[HealthMonitorImpl]) = new {
//    def toJValue = {
//      val services = monitors.foldLeft(JObject(Nil)){(result, element) => result.merge(element.toJValue).asInstanceOf[JObject]}
//      JObject(JField("services", services) :: Nil)
//    }
//  }
//}
