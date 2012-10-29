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

class HttpClientXLightWebSpec extends Specification with TestAkkaDefaults with HttpRequestMatchers {
  val duration = 250.milliseconds
  val retries = 30
  
  sequential

  private val httpClient = new HttpClientXLightWeb

  private val configPattern = """server {
    port = %d
    sslPort = %d
  }"""

  private var port = 8586
  private var uri = ""

  override def is = args(sequential = true) ^ super.is
  override def map(fs: => Fragments) = {
    var stoppable: Option[Stoppable] = None

    def startStep = Step {
      val config = Configuration.parse(configPattern.format(port, port + 1), BlockFormat)
      val echoServer = EchoServer.server(config, defaultFutureDispatch)
      val (_, stop) = Await.result(echoServer.start.get map { t => uri = "http://localhost:%s/echo".format(port); t }, duration)
      stoppable = stop
    } 

    def stopStep = Step {
      stoppable foreach { Stoppable.stop(_, duration) }
    }
    
    startStep ^ fs ^ stopStep
  }

  "HttpClientXLightWeb" should {
    "Support GET to invalid server should return http error" in {
      val result = httpClient.get[String]("http://127.0.0.1:666/foo").failed
      result must whenDelivered {
        haveClass[HttpException]
      }
    }

    "Support GET to invalid URI/404 should cancel Future" in {
      httpClient.get[String](uri + "/bogus").failed must whenDelivered {
        beLike { case HttpException(NotFound, _) => ok }
      }
    }

    "Support GET requests with status OK" in {
      httpClient.parameters('param1 -> "a").get[String](uri) must succeedWithContent {
        (_ : String) => ok
      }
    }

    "Support GET requests with status Not Found" in {
      httpClient.get[String](uri + "/bogus").failed must whenDelivered {
        beLike { case HttpException(NotFound, _) => ok }
      }
    }

    "Support GET requests with query params" in {
      httpClient.get[String](uri + "?param1=a&param2=b") must succeedWithContent {
        be_==/("param1=a&param2=b")
      }
    }

    "Support GET requests with request params" in {
      httpClient.parameters('param1 -> "a", 'param2 -> "b").get[String](uri) must succeedWithContent {
        be_==/("param1=a&param2=b")
      }
    }

    "Support POST requests with query params" in {
      httpClient.post[String](uri + "?param1=a&param2=b")("") must succeedWithContent {
        be_==/("param1=a&param2=b")
      }
    }

    "Support POST requests with request params" in {
      httpClient.parameters('param1 -> "a", 'param2 -> "b").post[String](uri)("") must succeedWithContent {
        be_==/("param1=a&param2=b")
      }
    }

    "Support POST requests with body" in {
      val expected = "Hello, world"
      httpClient.post[String](uri)(expected) must succeedWithContent (be_==(expected))
    }

    "Support POST requests with body and request params" in {
      val expected = "Hello, world"
      httpClient.parameters('param1 -> "a", 'param2 -> "b").post[String](uri)(expected) must succeedWithContent {
        be_==/("param1=a&param2=b" + expected)
      }
    }

    "Support PUT requests with body" in {
      val expected = "Hello, world"
      httpClient.header(`Content-Length`(100)).put[String](uri)(expected) must succeedWithContent {
        be_==/(expected)
      }
    }

    "Support GET requests with header" in {
      httpClient.header("Fooblahblah" -> "washere").header("param2" -> "1").get[String](uri + "?headers=true") must succeedWithContent {
        contain("Fooblahblah: washere") and contain("param2: 1")
      }
    }

    "Support POST requests with Content-Type: text/html & Content-Length: 100" in {
      val expected = "<html></html>"
      httpClient.headers(`Content-Type`(MimeTypes.text/html) :: `Content-Length`(100) :: Nil).post[String](uri)(expected) must succeedWithContent {
        be_==(expected)
      }
    }

    "Support POST requests with large payload" in {
      val expected = Array.fill(2048*1000)(0).toList.mkString("")
      httpClient.post[String](uri)(expected) must succeedWithContent {
        be_==(expected)
      }
    }

    "Support POST requests with large payload with several chunks" in {
      val expected = Array.fill[Byte](2048*100)('0')
      val chunk: ByteChunk = Right(ByteBuffer.wrap(expected) :: Future(ByteBuffer.wrap(expected)).liftM[StreamT])
      httpClient.post[ByteChunk](uri)(chunk) must succeedWithContent {
        (data: ByteChunk) => ByteChunk.forceByteArray(data) must whenDelivered {
          (data: Array[Byte]) => data.length must_== expected.length * 2
        }
      }
    }

   "Support HEAD requests" in {
      httpClient.head(uri) must whenDelivered {
        beLike {
          case HttpResponse(status, _, _, _) =>
            (status.code must_== HttpStatusCodes.OK)
        }
      }
    }

   "Support response headers" in {
      httpClient.get(uri) must whenDelivered {
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
        httpClient.get(uri + "?test=true")
      }

      Future.sequence(futures) must whenDelivered {
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
      httpClient.get[String](uri + "?param1=a&param2=b") must succeedWithContent {
        be_==/("param1=a&param2=b")
      }
    }

    "Support POST requests with body with Array[Byte]" in {
      val expected = "Hello, world"
      httpClient.post(uri)(StringToByteArray(expected)) must succeedWithContent {
        (content: Array[Byte]) => ByteArrayToString(content) must_== expected
      }
    }

    "Support POST requests with body several chunks and transcoding" in {
      val content: ByteChunk = Right(ByteBuffer.wrap("foo".getBytes("UTF-8")) :: Future(ByteBuffer.wrap("bar".getBytes("UTF-8"))).liftM[StreamT])
      httpClient.post(uri)(content) must succeedWithContent {
        (data: ByteChunk) => ByteChunk.forceByteArray(data) must whenDelivered {
          (bytes: Array[Byte]) => new String(bytes, "UTF-8") must_== "foobar"
        }
      }
    }

    "Support POST requests with encoded URL should be preserved" in {
      val content = "Hello, world"
      httpClient.post(uri + "?headers=true&plckForumId=Cat:Wedding%20BoardsForum:238")(content) must succeedWithContent {
        (_: String) must contain("plckForumId=Cat:Wedding BoardsForum:238")
      }
    }
  }
}

object EchoServer extends BlueEyesServer with BlueEyesServiceBuilder with HttpRequestHandlerCombinators with TestAkkaDefaults { 
  import blueeyes.core.http.MimeTypes._

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
      path("/echo") {
        produce[ByteChunk, String, ByteChunk](text/html) {
          get(handler _) ~
	        post(handler _) ~
	        put(handler _) ~
	        head(handler _)
        }
      }
    }
  }
}
