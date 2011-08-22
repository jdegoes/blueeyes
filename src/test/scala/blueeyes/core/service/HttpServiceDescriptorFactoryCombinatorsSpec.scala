package blueeyes.core.service

import blueeyes.core.http.HttpStatusCodes._
import test.BlueEyesServiceSpecification
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.data.{ByteChunk, BijectionsChunkJson, BijectionsChunkString}
import blueeyes.json.JsonAST.{JValue, JInt, JNothing, JString}
import blueeyes.core.http.MimeTypes._
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import java.io.File
import blueeyes.core.http.{HttpMethods, HttpRequest, HttpResponse, HttpStatus}
import blueeyes.health.metrics.{interval, eternity}
import blueeyes.health.metrics.IntervalLength._
import blueeyes.json.Printer

class HttpServiceDescriptorFactoryCombinatorsSpec extends BlueEyesServiceSpecification with HeatlhMonitorService with BijectionsChunkJson{
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
            fields  = "cs-method cs-uri"
            roll    = "never"
            file    = "%s"
            enabled = true
          }
          healthMonitor{
            overage{
              interval{
                length = "1 seconds"
                count  = 12
              }
            }
          }
        }
      }
    }
  """.format(System.getProperty("java.io.tmpdir") + File.separator + "w3log.log")

  implicit val httpClient: HttpClient[ByteChunk] = new HttpClient[ByteChunk] {
    def apply(r: HttpRequest[ByteChunk]): Future[HttpResponse[ByteChunk]] = {
      Future.sync(HttpResponse[ByteChunk](content = Some(r.uri.path match {
        case Some("/foo/v1/proxy")  => BijectionsChunkString.StringToChunk("it works!")

        case _ => BijectionsChunkString.StringToChunk("it does not work!")
      })))
    }
    def isDefinedAt(x: HttpRequest[ByteChunk]) = true
  }

  doAfterSpec {
    findLogFile foreach { _.delete }
  }

  private def findLogFile = {
    new File(System.getProperty("java.io.tmpdir")).listFiles filter { file => file.getName.startsWith("w3log") && file.getName.endsWith(".log") } headOption
  }

  "service" should {
    "support health monitor service" in {
      val f = service.get("/foo")
      f.value must eventually(beSomething)
      f.value.get.content must beNone
      f.value.get.status  mustEqual(HttpStatus(OK))
    }

    "support health monitor statistics" in {
      val f = service.get[JValue]("/blueeyes/services/email/v1/health")
      f.value must eventually(beSomething)

      val response = f.value.get
      response.status  mustEqual(HttpStatus(OK))

      val content  = response.content.get
      println(Printer.pretty(Printer.render(content)))
      content \ "requests" \ "GET" \ "count" mustEqual(JInt(1))
      content \ "requests" \ "GET" \ "timing" mustNotEq(JNothing)
      content \ "requests" \ "GET" \ "overage" \ "perSecond"      mustNotEq(JNothing)
      content \ "requests" \ "GET" \ "overage" \ "interval" \ "length"    mustEqual(JString("1s"))
      content \ "requests" \ "GET" \ "overage" \ "interval" \ "count"      mustEqual(JInt(12))

      content \ "service" \ "name"    mustEqual(JString("email"))
      content \ "service" \ "version" mustEqual(JString("1.2.3"))
      content \ "uptimeSeconds"       mustNotEq(JNothing)
    }

    "add service locator" in {
      import BijectionsChunkString._
      val f = service.get[String]("/proxy")
      f.value must eventually(beSomething)

      val response = f.value.get
      response.status  mustEqual(HttpStatus(OK))
      response.content must beSome("it works!")
    }
  }

  specifyExample("RequestLogging: Creates logRequest") in{
    findLogFile mustNot be (None)
  }
}

trait HeatlhMonitorService extends BlueEyesServiceBuilder with HttpServiceDescriptorFactoryCombinators with BijectionsChunkJson{
  implicit def httpClient: HttpClient[ByteChunk]

  val emailService = service ("email", "1.2.3") {
    requestLogging{
      logging { log =>
        healthMonitor(eternity, (HttpMethods.POST, interval(3.seconds, 3))) { monitor =>
          serviceLocator { locator: ServiceLocator[ByteChunk] =>
            context => {
              request {
                path("/foo") {
                  get  { request: HttpRequest[ByteChunk] => HttpResponse[ByteChunk]().future }
                } ~
                path("/proxy") {
                  get { request: HttpRequest[ByteChunk] =>
                    val foo = locator("foo", "1.02.32")

                    foo(request)
                  }
                } ~
                remainingPath{ path =>
                  get{
                    request: HttpRequest[ByteChunk] => HttpResponse[ByteChunk]().future
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
