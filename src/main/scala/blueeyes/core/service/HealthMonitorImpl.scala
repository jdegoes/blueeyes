package blueeyes.core.service

import blueeyes.health.HealthMonitor
import net.lag.configgy.ConfigMap
import blueeyes.json.JsonAST._
import blueeyes.json.JPath
import blueeyes.health.metrics._

private[service] case class HealthMonitorImpl(config: ConfigMap, serviceName: String, serviceVersion: Int) extends HealthMonitor{
  def sampleSize = config.getInt("sampleSize", 1000)
}

private[service] trait HealthMonitorImplicits extends StatisticComposer{
  implicit def serializableHealthMonitorSugar(monitor: HealthMonitorImpl) = new {
    def toJValue = {
      val healthJObject = JObject(JField("helth", statisticsJObject) :: Nil)

      JObject(JField(monitor.serviceName, JObject(JField("v" + monitor.serviceVersion, healthJObject) :: Nil)) :: Nil)
    }
    private def statisticsJObject = {
      val statistics    = List(composeStatistics(monitor.timerStats, composeTimer _), composeStatistics(monitor.errorStats, composeErrorStat _), composeStatistics(monitor.sampleStats, composeSample _), composeStatistics(monitor.countStats, composeCounter _))

      statistics.foldLeft(JObject(Nil)){(result, element) => result.merge(element).asInstanceOf[JObject]}
    }
  }
}

private[service] trait HealthMonitorsImplicits extends HealthMonitorImplicits{
  implicit def serializableHealthMonitorsSugar(monitors: List[HealthMonitorImpl]) = new {
    def toJValue = {
      val services = monitors.foldLeft(JObject(Nil)){(result, element) => result.merge(element.toJValue).asInstanceOf[JObject]}
      JObject(JField("services", services) :: Nil)
    }
  }
}

private[service] trait StatisticComposer{

  def composeStatistics[T](stat: Map[JPath, T], f: (T) => JValue) = {
    val jObjects = stat.toList.map(kv => jvalueToJObject(kv._1, f(kv._2)))
    jObjects.foldLeft(JObject(Nil)){(result, element) => result.merge(element).asInstanceOf[JObject]}
  }

  def composeCounter(counter: Counter): JValue = JInt(counter.count)

  def composeSample(sample: Sample): JValue = {
    val histogramJValue: List[JField] = sample.histogram.map(v => v.toList.map(kv => JField(kv._1.toString, JInt(kv._2)))).map(fs => JField("histogram", JObject(fs)) :: Nil).getOrElse(Nil)

    JObject(JField("count", JInt(sample.count)) :: histogramJValue)
  }

  def composeErrorStat(errorStat: ErrorStat): JValue = {
    val distributionJValue = errorStat.distribution.toList.map(kv => JField(kv._1.getName, JInt(kv._2)))
    JObject(JField("errorCount", JInt(errorStat.count)) :: JField("errorDistribution", JObject(distributionJValue)) :: Nil)
  }

  def composeTimer(timer: Timer): JValue =
    JObject(JField("minimumTime", JDouble(timer.min.value)) :: JField("maximumTime", JDouble(timer.max.value)) :: JField("averageTime", JDouble(timer.mean.value)) :: JField("standardDeviation", JDouble(timer.standardDeviation.value)) :: Nil)

  private def normalizePath(path: JPath) = if (path.path.startsWith(".")) path.path.substring(1) else path.path

  private def jvalueToJObject(path: JPath, value: JValue): JObject = {
    val elements = normalizePath(path).split("\\.").reverse
    elements.tail.foldLeft(JObject(JField(elements.head, value) :: Nil)){(result, element) => JObject(JField(element, result) :: Nil)}
  }
}
