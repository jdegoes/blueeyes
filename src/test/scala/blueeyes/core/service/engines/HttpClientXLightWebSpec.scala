package blueeyes.core.service.engines

import blueeyes.core.data.BijectionsByteArray._
import blueeyes.core.data.{BijectionsString, BijectionsByteArray}
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpHeaderImplicits
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.HttpStatusCodes._
import java.util.concurrent.CountDownLatch
import net.lag.configgy.Configgy
import org.specs.Specification
import org.specs.util._
import blueeyes.concurrent.{FutureDeliveryStrategySequential, Future, FutureImplicits}
import BijectionsByteArray.ByteArrayToByteArray
import BijectionsString.StringToString

class HttpClientXLightWebSpec extends Specification with FutureImplicits with FutureDeliveryStrategySequential{
  import HttpHeaderImplicits._

  val duration = 250
  val retries = 30

  private val httpClient = new HttpClientXLightWebEnginesString { }
  private val httpClientArrayByte = new HttpClientXLightWebEnginesArrayByte { }

  private val configPattern = """server {
    port = %d
    sslPort = %d
  }"""

  shareVariables()

  private var port = 8585
  private var server: Option[NettyEngine] = None
  private var clientFacade: SampleClientFacade = _
  private var uri = ""

  "HttpClientXLightWeb" should {
    doFirst {
      var success = false
      do {
        EchoServer.echoService
        success = try {
          val doneSignal = new CountDownLatch(1)

          Configgy.configureFromString(configPattern.format(port, port + 1))

          EchoServer.start.deliverTo { _ => doneSignal.countDown()}
          true
        }
        catch {
          case e: Throwable => {
            e.printStackTrace()
            port = port + 2
            false
          }
        }
      } while(!success)

      server = Some(EchoServer)
      uri = "http://localhost:%s/echo".format(port)
    }

    "Support GET to invalid server should return http error" in {
      val f = httpClient.get("http://127.0.0.1:666/foo")
      f.error must eventually(retries, new Duration(duration))(beNone)
    }

    "Support GET to invalid URI/404 should cancel Future" in {
      val f = httpClient.get(uri + "bogus")
      f.isCanceled must eventually(retries, new Duration(duration))(beTrue)
      f.error.isInstanceOf[Option[HttpException]] must  beTrue
      f.error.get.asInstanceOf[HttpException].failure must be(NotFound)
    }
    
    "Support GET requests with status OK" in {
      val f = httpClient.get(uri)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.status.code must eventually(be(HttpStatusCodes.OK))
    }

    "Support GET requests with status Not Found" in {
      val f = httpClient.get("/bogus")
      f.value must eventually(retries, new Duration(duration))(beNone)
    }

    "Support GET requests with query params" in {
      val f = httpClient.get(uri + "?param1=a&param2=b")
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support GET requests with request params" in {
      val f = httpClient.parameters('param1 -> "a", 'param2 -> "b").get(uri)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with query params" in {
      val f = httpClient.post(uri + "?param1=a&param2=b")("")
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with request params" in {
      val f = httpClient.parameters('param1 -> "a", 'param2 -> "b").post(uri)("")
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with body" in {
      val content = "Hello, world"
      val f = httpClient.post(uri)(content)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must eventually(equalIgnoreSpace(content))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with body and request params" in {
      val content = "Hello, world"
      val f = httpClient.parameters('param1 -> "a", 'param2 -> "b").post(uri)(content)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must equalIgnoreSpace("param1=a&param2=b" + content)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support PUT requests with body" in {
      val content = "Hello, world"
      val f = httpClient.header(`Content-Length`(100)).put(uri)(content)
      f.deliverTo((res: HttpResponse[String]) => {})
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must equalIgnoreSpace(content)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support GET requests with header" in {
      val f = httpClient.header("Fooblahblah" -> "washere").header("param2" -> "1").get(uri + "?headers=true")
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must include("Fooblahblah: washere") and include("param2: 1")
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with Content-Type: text/html & Content-Length: 100" in {
      val content = "<html></html>"
      val f = httpClient.headers(`Content-Type`(text/html) :: `Content-Length`(100) :: Nil).post(uri)(content)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must beEqual(content)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with large payload" in {
      val content = Array.fill(1024*1000)(0).toList.mkString("")
      val f = httpClient.post(uri)(content)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must beEqual(content)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

   "Support HEAD requests" in {
      val f = httpClient.head(uri)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

   "Support response headers" in {
      val f = httpClient.get(uri)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.status.code must be(HttpStatusCodes.OK)
      f.value.get.headers must haveKey("kludgy")
    }

    "Support GET requests of 1000 requests" in {
      val total = 1000
      val duration = 1000
      val futures = (0 until total).map { i =>
        httpClient.get(uri + "?test=true")
      }
      val responses = futures.foldLeft(0) {
	(acc, f) => {
          f.value must eventually(retries, new Duration(duration))(beSomething)
          f.value.get.status.code must be(HttpStatusCodes.OK)
          acc + 1
	}
      }

      responses must beEqual(total)
    }

    "Support GET requests with query params with Array[Byte]" in {
      val f = httpClientArrayByte.get(uri + "?param1=a&param2=b")
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.map(ByteArrayToString(_)).get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with body with Array[Byte]" in {
      val content = "Hello, world"
      val f = httpClientArrayByte.post(uri)(StringToByteArray(content))
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.map(ByteArrayToString(_)).get.trim must eventually(equalIgnoreSpace(content))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with encoded URL should be preserved" in {
      val content = "Hello, world"
      val f = httpClientArrayByte.post(uri + "?headers=true&plckForumId=Cat:Wedding%20BoardsForum:238")(StringToByteArray(content))
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.status.code must be(HttpStatusCodes.OK)
      f.value.get.content.map(ByteArrayToString(_)).get.contains("plckForumId=Cat:Wedding BoardsForum:238") must beTrue
    }
  }
}

import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.service.{HttpService, HttpReflectiveServiceList}

object EchoServer extends EchoService with HttpReflectiveServiceList[ChunkReader] with NettyEngine{ }

trait EchoService extends BlueEyesServiceBuilder with BijectionsChunkReader{
  import blueeyes.core.http.MimeTypes._

  private implicit val ordering = new Ordering[Symbol] {
    def compare(l: Symbol, r: Symbol): Int = {
      l.name.compare(r.name)
    }
  }

  private def response(content: Option[String] = None) =
    HttpResponse[String](status = HttpStatus(HttpStatusCodes.OK), content = content, headers = Map("kludgy" -> "header test"))

  private def handler(request: HttpRequest[ChunkReader]) = {
    val params = request.parameters.toList.sorted.foldLeft(List[String]()) { (l: List[String], p: Tuple2[Symbol, String]) =>
      l ++ List("%s=%s".format(p._1.name, p._2))
    }.mkString("&")
    val headers = request.parameters.get('headers).map { o =>
      request.headers.toList.sorted.foldLeft(List[String]()) { (l, h) =>
    	l ++ List("%s: %s".format(h._1, h._2))
      }.mkString("&")
    }.getOrElse("")
    val content: String = request.content.map(v => ChunkReaderToString(v)).getOrElse("")
    val newContent = params + content + headers
    new Future[HttpResponse[String]]().deliver(response(content = Some(newContent)))
  }

  val echoService: HttpService[ChunkReader] = service("echo", "1.0.0") { context =>
    request {
      produce[ChunkReader, String, ChunkReader](text/html) {
        path("/echo") {
          get(handler) ~
	        post(handler) ~
	        put(handler) ~
	        head(handler)
        }
      }
    }
  }
}
