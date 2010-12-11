package blueeyes.core.service

import blueeyes.health.HealthMonitor
import net.lag.configgy.ConfigMap
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.json.JsonAST._
import blueeyes.json.JPath
import blueeyes.health.metrics._

private[service] class ServiceHealthMonitor(val config: ConfigMap, val serviceName: String, val serviceVersion: Int) extends HealthMonitor with SerializableHealthMonitor{
  def sampleSize = config.getInt("sampleSize", 1000)
}

private[service] trait SerializableHealthMonitor extends StatisticComposer{ self: HealthMonitor =>
  def toJValue: JObject = {
    val healthJObject = JObject(JField("helth", statisticsJObject) :: Nil)
    
    JObject(JField(serviceName, JObject(JField("v" + serviceVersion, healthJObject) :: Nil)) :: Nil)
  }

  def statisticsJObject = {
    val statistics    = List(composeStatistics(timerStats, composeTimer _), composeStatistics(errorStats, composeErrorStat _), composeStatistics(sampleStats, composeSample _), composeStatistics(countStats, composeCounter _))

    statistics.foldLeft(JObject(Nil)){(result, element) => result.merge(element).asInstanceOf[JObject]}
  }

  def serviceName: String

  def serviceVersion: Int
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

import blueeyes.core.http.MimeTypes._
class HealthService(monitors: List[ServiceHealthMonitor]) extends BlueEyesServiceBuilder{
  val healthService = service("health", "1.0.0") {
    context => {
      request {
        path("/blueeyes/health") {
          produce(application/json) {
            get {
              request: HttpRequest[Array[Byte]] => {
                val servicesJObject = monitors.foldLeft(JObject(Nil)){(result, element) => result.merge(element.toJValue).asInstanceOf[JObject]}
                HttpResponse[JValue](content=Some(JObject(JField("services", servicesJObject) :: Nil)))
              }
            }
          }
        }
      }
    }
  }
}