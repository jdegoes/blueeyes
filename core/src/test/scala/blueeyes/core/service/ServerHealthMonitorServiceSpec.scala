package blueeyes.core.service

import test.BlueEyesServiceSpecification
import blueeyes.bkka._
import blueeyes.json._
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.test.HttpRequestMatchers 
import DefaultBijections._

class ServerHealthMonitorServiceSpec extends BlueEyesServiceSpecification with ServerHealthMonitorService with HttpRequestMatchers with TestAkkaDefaults {
  val healthMonitorQueryTimeout = akka.util.Timeout(10000)
  val executionContext = defaultFutureDispatch

   "Server Health Monitor Service" should{
    "get server health" in {
      client.get[JValue]("/serverhealth/v1/blueeyes/server/health") must succeedWithContent {
        (content: JValue) => {
          (content \ "runtime" must_!=(JUndefined)) and
          (content \ "memory" must_!=(JUndefined)) and
          (content \ "threads" must_!=(JUndefined)) and
          (content \ "operatingSystem" must_!=(JUndefined)) and
          (content \ "server" \ "hostName" must_!=(JUndefined)) and
          (content \ "server" \ "port" must_!=(JUndefined)) and
          (content \ "server" \ "sslPort" must_!=(JUndefined))
        }
      }
    }
  }
}
