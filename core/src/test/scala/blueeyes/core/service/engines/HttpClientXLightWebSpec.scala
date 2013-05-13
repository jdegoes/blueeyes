package blueeyes.core.service
package engines

import blueeyes._
import blueeyes.bkka._
import blueeyes.core.http._
import blueeyes.core.http.test._
import blueeyes.core.data._
import blueeyes.core.service._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.json._
import blueeyes.util._
import DefaultBijections._

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.Await

import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat

import org.specs2.specification.{Step, Fragments}
import org.specs2.mutable.Specification
import org.specs2.time.TimeConversions._

import java.nio.ByteBuffer
import scalaz.StreamT
import scalaz.syntax.monad._
import com.weiglewilczek.slf4s.Logging

class HttpClientXLightWebSpec extends Specification with TestAkkaDefaults with HttpRequestMatchers with PortFinder {
  val duration = 2000.milliseconds
  val retries = 30
  
  sequential

  private val baseClient = new HttpClientXLightWeb
  private val client = baseClient.protocol("http").host("localhost").path("/echo/v1")

  private val configPattern = """server {
    port = %d
    sslPort = %d
  }"""

  private val (port, sslPort) = findUnusedPorts(1000, 2) match {
    case Some(p :: sp :: Nil) => (p, sp)
    case _ => throw new Exception("Failed to find 2 unused ports for testing")
  }

  override def is = args(sequential = true) ^ super.is
  override def map(fs: => Fragments) = {
    var stoppable: Option[Stoppable] = None

    def startStep = Step {
      val config = Configuration.parse(configPattern.format(port, sslPort), BlockFormat)
      val echoServer = EchoServer.server(config, defaultFutureDispatch)
      val (_, stop) = Await.result(echoServer.start.get, duration)
      stoppable = stop
    } 

    def stopStep = Step {
      stoppable foreach { Stoppable.stop(_, duration) }
    }
    
    startStep ^ fs ^ stopStep
  }

  private def httpClient = client.port(port)

  "HttpClientXLightWeb" should {
    "Support GET to invalid server should return http error" in {
      baseClient.get[String]("http://127.0.0.1:666/foo").failed must awaited(duration) {
        beLike {
          case ex: java.net.ConnectException => ok
          case ex: java.net.SocketTimeoutException => ok
        }
      }
    }

    "Support GET requests with status OK" in {
      httpClient.parameters('param1 -> "a").get[String]("") must succeedWithContent {
        (_ : String) => ok
      }
    }

    "Support GET requests with status Not Found" in {
      httpClient.get[String]("/bogus") must awaited(duration) {
        beLike {
          case HttpResponse(status, _, content, _) => status.code must_== NotFound
        }
      }
    }

    "Support GET requests with query params" in {
      httpClient.get[String]("?param1=a&param2=b") must succeedWithContent {
        be_==/("param1=a&param2=b")
      }
    }

    "Support GET requests with request params" in {
      httpClient.parameters('param1 -> "a", 'param2 -> "b").get[String]("") must succeedWithContent {
        be_==/("param1=a&param2=b")
      }
    }

    "Support POST requests with query params" in {
      httpClient.post[String]("?param1=a&param2=b")("") must succeedWithContent {
        be_==/("param1=a&param2=b")
      }
    }

    "Support POST requests with request params" in {
      httpClient.parameters('param1 -> "a", 'param2 -> "b").post[String]("")("") must succeedWithContent {
        be_==/("param1=a&param2=b")
      }
    }

    "Support POST requests with body" in {
      val expected = "Hello, world"
      httpClient.post[String]("")(expected) must succeedWithContent (be_==(expected))
    }

    "Support POST requests with body and request params" in {
      val expected = "Hello, world"
      httpClient.parameters('param1 -> "a", 'param2 -> "b").post[String]("")(expected) must succeedWithContent {
        be_==/("param1=a&param2=b" + expected)
      }
    }

    "Support PUT requests with body" in {
      val expected = "Hello, world"
      httpClient.header(`Content-Length`(100)).put[String]("")(expected) must succeedWithContent {
        be_==/(expected)
      }
    }

    "Support GET requests with header" in {
      httpClient.header("Fooblahblah" -> "washere").header("param2" -> "1").get[String]("?headers=true") must succeedWithContent {
        contain("Fooblahblah: washere") and contain("param2: 1")
      }
    }

    "Support POST requests with Content-Type: text/html & Content-Length: 100" in {
      val expected = "<html></html>"
      httpClient.headers(`Content-Type`(MimeTypes.text/html) :: `Content-Length`(100) :: Nil).post[String]("")(expected) must succeedWithContent {
        be_==(expected)
      }
    }

    "Support POST requests with large payload" in {
      val expected = Array.fill(2048*1000)(0).toList.mkString("")
      httpClient.post[String]("")(expected) must succeedWithContent {
        be_==(expected)
      }
    }

    "Support POST requests with large payload with several chunks" in {
      val expected = Array.fill[Byte](2048*100)('0')
      val chunk: ByteChunk = Right(expected :: Future(expected).liftM[StreamT])
      httpClient.post[ByteChunk]("")(chunk) must succeedWithContent {
        (data: ByteChunk) => ByteChunk.forceByteArray(data) must awaited(duration) {
          (data: Array[Byte]) => data.length must_== expected.length * 2
        }
      }
    }

   "Support HEAD requests" in {
      httpClient.head("") must awaited(duration) {
        beLike {
          case HttpResponse(status, _, _, _) =>
            (status.code must_== HttpStatusCodes.OK)
        }
      }
    }

   "Support response headers" in {
      httpClient.get("") must awaited(duration) {
        beLike {
          case HttpResponse(status, headers, _, _) =>
            (status.code must_== HttpStatusCodes.OK) and
            (headers.raw must haveKey("kludgy"))
        }
      }
    }

    "Support GET requests of 1000 requests" in {
      val total = 1000
      val duration = 1000.milliseconds
      val futures = List.fill(total) {
        httpClient.get("?test=true")
      }

      Future.sequence(futures) must awaited(duration) {
        beLike {
          case responses =>
          (responses.size must_== total) and
          forall(responses) {
            response => response.status.code must_== HttpStatusCodes.OK
          }
        }
      }
    }

    "Support GET requests with query params" in {
      httpClient.get[String]("?param1=a&param2=b") must succeedWithContent {
        be_==/("param1=a&param2=b")
      }
    }

    "Support POST requests with body with Array[Byte]" in {
      val expected = "Hello, world"
      httpClient.post("")(StringToByteArray(expected)) must succeedWithContent {
        (content: Array[Byte]) => ByteArrayToString(content) must_== expected
      }
    }

    "Support POST requests with body several chunks and transcoding" in {
      val content: ByteChunk = Right("foo".getBytes("UTF-8") :: Future("bar".getBytes("UTF-8")).liftM[StreamT])
      httpClient.post("")(content) must succeedWithContent {
        (data: ByteChunk) => ByteChunk.forceByteArray(data) must awaited(duration) {
          (bytes: Array[Byte]) => new String(bytes, "UTF-8") must_== "foobar"
        }
      }
    }

    "Support POST requests with encoded URL should be preserved" in {
      val content = "Hello, world"
      httpClient.post("?headers=true&plckForumId=Cat:Wedding%20BoardsForum:238")(content) must succeedWithContent {
        (_: String) must contain("plckForumId=Cat:Wedding BoardsForum:238")
      }
    }
  }
}

object EchoServer extends BlueEyesServer with BlueEyesServiceBuilder with HttpRequestHandlerCombinators with TestAkkaDefaults with Logging { 
  import HttpRequestHandlerImplicits._

  val executionContext = defaultFutureDispatch

  private implicit val ordering = new Ordering[Symbol] {
    def compare(l: Symbol, r: Symbol): Int = {
      l.name.compare(r.name)
    }
  }

  private def response(content: Option[String] = None) =
    HttpResponse[String](status = HttpStatus(HttpStatusCodes.OK), content = content, headers = Map("kludgy" -> "header test"))

  private def handler(request: HttpRequest[ByteChunk]): Future[HttpResponse[String]] = {
    val params = request.parameters.toList.sorted.foldLeft(List[String]()) { (l: List[String], p: Tuple2[Symbol, String]) =>
      l ++ List("%s=%s".format(p._1.name, p._2))
    }.mkString("&")

    val headers = request.parameters.get('headers).map { o =>
      request.headers.raw.toList.sorted.foldLeft(List[String]()) { (l, h) =>
        l ++ List("%s: %s".format(h._1, h._2))
      }.mkString("&")
    }.getOrElse("")

    for (bytes <- request.content.map(ByteChunk.forceByteArray).getOrElse(Promise.successful(Array.empty[Byte]))) yield {
      response(content = Some(params + new String(bytes, "UTF-8")  + headers))
    }
  }

  val echoService = service("echo", "1.0.0") { context =>
    request {
      encode[ByteChunk, Future[HttpResponse[String]], Future[HttpResponse[ByteChunk]]] {
        produce(text/html) {
          get(handler _) ~
          post(handler _) ~
          put(handler _) ~
          head(handler _)
        }
      }
    }
  }
}
