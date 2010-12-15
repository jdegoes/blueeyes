package blueeyes.core.service

import org.specs.Specification
import blueeyes.json.JsonAST._
import blueeyes.json.JPathImplicits._
import blueeyes.core.http.HttpStatusCodes._
import net.lag.configgy.Configgy
import test.BlueEyesServiceSpecification
import blueeyes.core.data.BijectionsByteArray._
import blueeyes.core.http.HttpStatus

class HeatlhMonitorServiceSpec extends BlueEyesServiceSpecification[Array[Byte]] with HealthMonitorsImplicits{
  private lazy val monitor     = {
    Configgy.configureFromString("")

    val montitorImpl = new HealthMonitorImpl(Configgy.config.configMap("service"), "foo", 1)
    montitorImpl.count("requestCount")(2)
    montitorImpl.count("requestCount")(3)

    montitorImpl
  }

  private val healthService = new HeatlhMonitorService(monitor :: Nil)

  val service = healthService.healthService

  "HeatlhMonitorService should return helth statistic" in {
    path("/blueeyes/health"){
      get{
        responseStatus  mustEqual(HttpStatus(OK))
        responseContent.map(ByteArrayToJValue(_)) must beSome(List(monitor).toJValue)
        ()
      }
    }
  }
}