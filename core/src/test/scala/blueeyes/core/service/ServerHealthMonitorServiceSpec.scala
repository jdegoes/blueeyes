package blueeyes.core.service

import test.BlueEyesServiceSpecification
import blueeyes.json.JsonAST._
import blueeyes.core.http.HttpStatus
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.test.HttpRequestMatchers 

class ServerHealthMonitorServiceSpec extends BlueEyesServiceSpecification with ServerHealthMonitorService with HttpRequestMatchers {
  val healthMonitorQueryTimeout = akka.util.Timeout(10000)

   "Server Health Monitor Service" should{
    "get server health" in {
      service.get[JValue]("/blueeyes/server/health") must succeedWithContent {
        (content: JValue) => {
          (content \ "runtime" must_!=(JNothing)) and
          (content \ "memory" must_!=(JNothing)) and
          (content \ "threads" must_!=(JNothing)) and
          (content \ "operatingSystem" must_!=(JNothing)) and
          (content \ "server" \ "hostName" must_!=(JNothing)) and
          (content \ "server" \ "port" must_!=(JNothing)) and
          (content \ "server" \ "sslPort" must_!=(JNothing))
        }
      }
    }
  }
}
