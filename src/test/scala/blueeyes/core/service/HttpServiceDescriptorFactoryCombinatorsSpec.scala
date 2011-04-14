package blueeyes.core.service

import blueeyes.core.http.HttpStatusCodes._
import test.BlueEyesServiceSpecification
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.core.data.{ChunkReader, BijectionsChunkReaderJson, BijectionsChunkReaderString}
import blueeyes.json.JsonAST.{JValue, JInt, JNothing, JString}
import blueeyes.core.http.MimeTypes._
import blueeyes.concurrent.Future
import java.io.File

class HttpServiceDescriptorFactoryCombinatorsSpec extends BlueEyesServiceSpecification[ChunkReader] with HeatlhMonitorService with BijectionsChunkReaderJson{
  import BijectionsChunkReaderString._
  override def configuration = """
    services {
      foo {
        v1 {
          serviceRootUrl = "/foo/v1"
        }
      }
      email {
        v1 {
          requestLog {
            fields = "cs-method cs-uri"
            roll = "never"
            file = "%s"
          }
        }
      }
    }
  """.format(System.getProperty("java.io.tmpdir") + File.separator + "w3log.log")

  implicit val httpClient: HttpClient[ChunkReader] = new HttpClient[ChunkReader] {
    def apply(r: HttpRequest[ChunkReader]): Future[HttpResponse[ChunkReader]] = {
      Future(HttpResponse[ChunkReader](content = Some(r.path match {
        case "/foo/v1/proxy"  => StringToChunkReader("it works!")
        
        case _ => StringToChunkReader("it does not work!")
      })))
    }
    def isDefinedAt(x: HttpRequest[ChunkReader]) = true
  }

  doAfterSpec {
    findLogFile foreach { _.delete }
  }

  private def findLogFile = {
    new File(System.getProperty("java.io.tmpdir")).listFiles filter{ file => file.getName.startsWith("w3log") && file.getName.endsWith(".log") } headOption
  }

  "service" should {
    "support health monitor service" in {
      val f = service.get("/foo")
      f.value must eventually(beSomething)
      f.value.get.content must beNone
      f.value.get.status  mustEqual(HttpStatus(OK))
    }

    "support health monitor statistics" in {
      val f = service.contentType[JValue](application/json).get("/blueeyes/services/email/v1/health")
      f.value must eventually(beSomething)

      val response = f.value.get
      response.status  mustEqual(HttpStatus(OK))

      val content  = response.content.get

      content \ "requests" \ "GET" \ "count" mustEqual(JInt(1))
      content \ "requests" \ "GET" \ "timing" mustNotEq(JNothing)

      content \ "service" \ "name"    mustEqual(JString("email"))
      content \ "service" \ "version" mustEqual(JString("1.2.3"))
      content \ "uptimeSeconds"       mustNotEq(JNothing)
    }

    "add service locator" in {
      val f = service.get("/proxy")
      f.value must eventually(beSomething)

      val response = f.value.get
      response.status  mustEqual(HttpStatus(OK))
      response.content.map(v => ChunkReaderToString(v)) must beSome("it works!")
    }
  }

  specifyExample("RequestLogging: Creates logRequest") in{
    findLogFile mustNot be (None)
  }
}

trait HeatlhMonitorService extends BlueEyesServiceBuilder with HttpServiceDescriptorFactoryCombinators with BijectionsChunkReaderJson{
  implicit def httpClient: HttpClient[ChunkReader]
  
  val emailService = service ("email", "1.2.3") {
    requestLogging{
      logging { log =>
        healthMonitor { monitor =>
          serviceLocator { locator: ServiceLocator[ChunkReader] =>
            context => {
              request {
                path("/foo") {
                  get  { request: HttpRequest[ChunkReader] => Future(HttpResponse[ChunkReader]()) }
                } ~
                path("/proxy") {
                  get { request: HttpRequest[ChunkReader] =>
                    locator("foo", "1.02.32") { client =>
                      client(request)
                    }.flatten
                  }
                } ~
                remainingPath{ path =>
                  get{
                    request: HttpRequest[ChunkReader] => HttpResponse[ChunkReader]()
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
