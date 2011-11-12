package blueeyes.core.service

import test.BlueEyesServiceSpecification
import blueeyes.json.JsonAST._
import blueeyes.core.http.HttpStatus
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.MimeTypes._

class ServerHealthMonitorServiceSpec extends BlueEyesServiceSpecification with ServerHealthMonitorService{
   "Server Health Monitor Service" should{
    "get server health" in {
      val f = service.get[JValue]("/blueeyes/server/health")
      f.value must eventually(beSome)

      val response = f.value.get

      response.status  mustEqual(HttpStatus(OK))
      val content = response.content.get

      content \ "runtime" must_!=(JNothing)
      content \ "memory" must_!=(JNothing)
      content \ "threads" must_!=(JNothing)
      content \ "operatingSystem" must_!=(JNothing)
      content \ "server" \ "hostName" must_!=(JNothing)
      content \ "server" \ "port" must_!=(JNothing)
      content \ "server" \ "sslPort" must_!=(JNothing)
    }
  }
}
