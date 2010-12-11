package blueeyes.core.service

import org.specs.Specification
import blueeyes.health.HealthMonitor
import blueeyes.json.JsonAST._
import blueeyes.json.JPathImplicits._

class SerializableHealthMonitorSpec extends Specification{

  "creates JValue" in{
    val monitor = new HealthMonitorImpl()

    monitor.count("requestCount")(2)
    monitor.count("requestCount")(3)

    monitor.toJValue mustEqual(JObject(List(JField("foo", JObject(List(JField("v1", JObject(List(JField("helth", JObject(List(JField("requestCount",JInt(5))))))))))))))
  }

  class HealthMonitorImpl extends HealthMonitor with SerializableHealthMonitor{
    def serviceVersion = 1

    def serviceName = "foo"

    def sampleSize = 0
  }
}