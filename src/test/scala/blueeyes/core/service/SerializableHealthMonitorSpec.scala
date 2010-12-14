package blueeyes.core.service

import org.specs.Specification
import blueeyes.json.JsonAST._
import blueeyes.json.JPathImplicits._
import net.lag.configgy.Configgy

class HealthMonitorImplSpec extends Specification with HealthMonitorImplicits with HealthMonitorsImplicits{
  private lazy val monitor     = {
    Configgy.configureFromString("")

    val montitorImpl = new HealthMonitorImpl(Configgy.config.configMap("service"), "foo", 1)
    montitorImpl.count("requestCount")(2)
    montitorImpl.count("requestCount")(3)

    montitorImpl
  }

  private val monitorJson = JObject(List(JField("foo", JObject(List(JField("v1", JObject(List(JField("helth", JObject(List(JField("requestCount",JInt(5)))))))))))))

  "HealthMonitor toJvalue: creates JValue" in{
    monitor.toJValue mustEqual(monitorJson)
  }
  "HealthMonitors toJvalue: creates JValue" in{
    List(monitor).toJValue mustEqual(JObject(JField("services", monitorJson) :: Nil))
  }
}