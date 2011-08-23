package blueeyes.health

import blueeyes.json.JPath
import blueeyes.concurrent.Future
import metrics._
import scala.collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import collection.mutable.ConcurrentMap
import ConcurrentMaps._
import blueeyes.json.JsonAST.{JValue, JField, JObject}

class HealthMonitor(timedConfig: Map[JPath, IntervalConfig] = Map(), defaultTimedConfig: IntervalConfig = eternity){

  private val _countsStats:   ConcurrentMap[JPath, Counter]   = new ConcurrentHashMap[JPath, Counter]
  private val _timersStats:   ConcurrentMap[JPath, Timer]     = new ConcurrentHashMap[JPath, Timer]
  private val _errorsStats:   ConcurrentMap[JPath, ErrorStat] = new ConcurrentHashMap[JPath, ErrorStat]
  private val _sampleStats:   ConcurrentMap[JPath, Sample]    = new ConcurrentHashMap[JPath, Sample]
  private val _timedSampleStats:   ConcurrentMap[JPath, Statistic[Long, Map[Long, Double]]]    = new ConcurrentHashMap[JPath, Statistic[Long, Map[Long, Double]]]
  private val _exportedStats: ConcurrentMap[JPath, ExportedStatistic[_]] = new ConcurrentHashMap[JPath, ExportedStatistic[_]]

  def overage(path: JPath)() { timedSampleStat(path) += 1 }

  def increment(path: JPath)(c: Long) { counterStat(path) += c }

  def count(path: JPath)              { increment(path)(1) }

  def time[T](path: JPath)(f: => T): T      = timerStat(path).time(f)

  def trackTime[T](path: JPath)(ns: Long)   = timerStat(path).+=(ns)

  def timeFuture[T](path: JPath)(f: Future[T]): Future[T] = timerStat(path).time(f)

  def sample(path: JPath)(d: Double) { sampleStat(path) += d }

  def export[T](path: JPath, value: => T)(implicit converter: T => JValue) =
    createIfAbsent(path, _exportedStats, () => new ExportedStatistic[T](value))

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

  def countStats    = _countsStats.toMap

  def exportedStats = _exportedStats.toMap

  def timerStats    = _timersStats.toMap

  def sampleStats   = _sampleStats.toMap

  def timedSampleStats   = _timedSampleStats.toMap

  def errorStats    = _errorsStats.toMap

  def toJValue: JValue = {
    val statistics = List[Map[JPath, Statistic[_, _]]](timerStats, errorStats, sampleStats, countStats, exportedStats, timedSampleStats)
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

  private implicit def clock() = System.currentTimeMillis()
  private def timedSampleStat(path: JPath):  Statistic[Long, Map[Long, Double]] = createIfAbsent[JPath, Statistic[Long, Map[Long, Double]]](path, _timedSampleStats, newTimedSample(path) _)
  private def newTimedSample(path: JPath)(): Statistic[Long, Map[Long, Double]] = TimedSample(timedConfig.get(path).getOrElse(defaultTimedConfig))

  private def sampleStat(path: JPath):  Sample    = createIfAbsent[JPath, Sample](path, _sampleStats, newSample _)
  private def newSample(): Sample = new Sample(sampleSize)

  private def counterStat(path: JPath): Counter   = createIfAbsent(path, _countsStats, newCounter _)
  private def newCounter() = new Counter()

  private def timerStat(path: JPath):   Timer     = createIfAbsent(path, _timersStats, newTimer _)
  private def newTimer() = new Timer()

  private def errorStat(path: JPath):   ErrorStat = createIfAbsent(path, _errorsStats, newErrorStat _)
  private def newErrorStat() = new ErrorStat()
}