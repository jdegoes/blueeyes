package blueeyes.core.service

import test.BlueEyesServiceSpecification
import blueeyes.json.JsonAST._
import blueeyes.core.http.{HttpResponse, HttpStatus}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.MimeTypes._

class ServerHealthMonitorServiceSpec extends BlueEyesServiceSpecification[Array[Byte]] with ServerHealthMonitorService{
  path$("/blueeyes/server/health"){
    contentType$[JValue, Array[Byte], Unit](application/json){
      get$ { response: HttpResponse[JValue] =>
        response.status  mustEqual(HttpStatus(OK))
        val content = response.content.get

        content \ "runtime" must notEq(JNothing)
        content \ "memory" must notEq(JNothing)
        content \ "threads" must notEq(JNothing)
        content \ "operatingSystem" must notEq(JNothing)
        content \ "server" \ "hostName" must notEq(JNothing)
        content \ "server" \ "port" must notEq(JNothing)
        content \ "server" \ "sslPort" must notEq(JNothing)
      }
    }
  } should "get server health"
}
