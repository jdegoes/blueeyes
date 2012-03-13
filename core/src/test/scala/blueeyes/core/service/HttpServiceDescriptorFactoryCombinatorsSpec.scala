package blueeyes.core.service

import blueeyes.core.http.HttpStatusCodes._
import test.BlueEyesServiceSpecification
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.data.{ByteChunk, BijectionsChunkJson, BijectionsChunkString}
import blueeyes.json.JsonAST._
import blueeyes.core.http.MimeTypes._
import java.io.File
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.health.metrics.{eternity}
import blueeyes.health.metrics.IntervalLength._
import org.specs2.specification.{Step, Fragments}

import akka.dispatch.Future
import akka.util.Timeout
import blueeyes.core.http.test._

class HttpServiceDescriptorFactoryCombinatorsSpec extends BlueEyesServiceSpecification with HealthMonitorService with BijectionsChunkJson with HttpRequestMatchers {
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
          healthMonitor {
            overage {
              interval {
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
      Future(HttpResponse[ByteChunk](content = Some(r.uri.path match {
        case Some("/foo/v1/proxy")  => 
          BijectionsChunkString.StringToChunk("it works!")

        case _ => BijectionsChunkString.StringToChunk("it does not work!")
      })))
    }
    def isDefinedAt(x: HttpRequest[ByteChunk]) = true
  }

  override protected def afterSpec = findLogFile foreach { _.delete }

  private def findLogFile = {
    new File(System.getProperty("java.io.tmpdir")).listFiles filter { file => file.getName.startsWith("w3log") && file.getName.endsWith(".log") } headOption
  }

  "service" should {
    "support health monitor service" in {
      service.get("/foo") must whenDelivered {
        beLike {
          case HttpResponse(status, _, None, _) => status must_== HttpStatus(OK)
        }
      }
    }

    "support health monitor statistics" in {
      service.get[JValue]("/blueeyes/services/email/v1/health") must succeedWithContent { (content: JValue) =>
        (content \ "requests" \ "GET" \ "count" \ "eternity" mustEqual(JArray(JInt(1) :: Nil))) and
        (content \ "requests" \ "GET" \ "timing" mustNotEqual(JNothing)) and
        (content \ "requests" \ "GET" \ "timing" \ "perSecond" \ "eternity" mustNotEqual(JNothing)) and
        (content \ "service" \ "name"    mustEqual(JString("email"))) and
        (content \ "service" \ "version" mustEqual(JString("1.2.3"))) and
        (content \ "uptimeSeconds"       mustNotEqual(JNothing)) 
      }
    }

    "add service locator" in {
      import BijectionsChunkString._
      service.get[String]("/proxy") must succeedWithContent((_: String) must_== "it works!")
    }

    "RequestLogging: Creates logRequest" in{
      findLogFile must_!=(None)
    }
  }
}

trait HealthMonitorService extends BlueEyesServiceBuilder with ServiceDescriptorFactoryCombinators with BijectionsChunkJson{
  implicit def httpClient: HttpClient[ByteChunk]

  val emailService = service ("email", "1.2.3") {
    requestLogging(Timeout(60000)) {
      logging { log =>
        healthMonitor(Timeout(60000), List(eternity)) { monitor =>
          serviceLocator { locator: ServiceLocator[ByteChunk] =>
            context => {
              request {
                path("/foo") {
                  get  { request: HttpRequest[ByteChunk] => Future(HttpResponse[ByteChunk]()) }
                } ~
                path("/proxy") {
                  get { request: HttpRequest[ByteChunk] =>
                    val foo = locator("foo", "1.02.32")
                    foo(request)
                  }
                } ~
                remainingPath{
                  get{
                    request: HttpRequest[ByteChunk] => { path: String =>
                      Future(HttpResponse[ByteChunk]())
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
}
