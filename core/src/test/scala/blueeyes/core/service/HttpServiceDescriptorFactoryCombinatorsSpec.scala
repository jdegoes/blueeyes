package blueeyes.core.service

import test.BlueEyesServiceSpecification

import blueeyes.BlueEyesServiceBuilder
import blueeyes.bkka._
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.test._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.core.service._
import blueeyes.health.metrics.{eternity}
import blueeyes.health.metrics.IntervalLength._
import blueeyes.json._

import akka.dispatch.Future
import akka.util.Timeout

import java.io.File
import scala.util.Random

import org.specs2.specification.{Step, Fragments}
import org.scalacheck.Gen._

import DefaultBijections._

class HttpServiceDescriptorFactoryCombinatorsSpec extends BlueEyesServiceSpecification with HealthMonitorService with HttpRequestMatchers with TestAkkaDefaults {
  val logFilePrefix = "w3log-" + identifier.sample.get
  val executionContext = defaultFutureDispatch

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
  """.format(System.getProperty("java.io.tmpdir") + File.separator + logFilePrefix + ".log")

  val httpClient: HttpClient[ByteChunk] = new HttpClient[ByteChunk] {
    def apply(r: HttpRequest[ByteChunk]): Future[HttpResponse[ByteChunk]] = {
      val responseContent = r.uri.path match {
        case Some("/foo/v1/email/v1/proxy")  => 
          DefaultBijections.stringToChunk("it works!")

        case _ => 
          DefaultBijections.stringToChunk("it does not work!")
      }

      Future(HttpResponse[ByteChunk](content = Some(responseContent)))
    }

    def isDefinedAt(x: HttpRequest[ByteChunk]) = true
  }

  override protected def afterSpec() = Step{
    findLogFile foreach { _.delete }
  }

  private def findLogFile = {
    new File(System.getProperty("java.io.tmpdir")).listFiles find { file => file.getName.startsWith(logFilePrefix) && file.getName.endsWith(".log") } 
  }

  val monitorClient = client.path("/email/v1")

  "service" should {
    "support health monitor service" in {
      monitorClient.get[ByteChunk]("/foo") must whenDelivered {
        beLike {
          case HttpResponse(HttpStatus(OK, _), _, None, _) => ok
        }
      }
    }

    "support health monitor statistics" in {
      import blueeyes.json.JParser.parse
      monitorClient.get[JValue]("/blueeyes/services/email/v1/health") must succeedWithContent { (content: JValue) =>
        (content \ "requests" \ "GET" \ "count" \ "eternity" mustEqual(parse("[1]"))) and
        (content \ "requests" \ "GET" \ "timing" mustNotEqual(JUndefined)) and
        (content \ "requests" \ "GET" \ "timing" \ "perSecond" \ "eternity" mustNotEqual(JUndefined)) and
        (content \ "service" \ "name"    mustEqual(JString("email"))) and
        (content \ "service" \ "version" mustEqual(JString("1.2.3"))) and
        (content \ "uptimeSeconds"       mustNotEqual(JUndefined)) 
      }
    }

    "add service locator" in {
      monitorClient.get[String]("/proxy") must succeedWithContent((_: String) must_== "it works!")
    }

    "RequestLogging: Creates logRequest" in{
      findLogFile must beSome[File]
    }
  }
}

trait HealthMonitorService extends BlueEyesServiceBuilder with ServiceDescriptorFactoryCombinators with TestAkkaDefaults {
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
