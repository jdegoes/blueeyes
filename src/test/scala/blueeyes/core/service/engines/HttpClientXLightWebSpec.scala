package blueeyes.core.service.engines

import blueeyes.core.data.BijectionsByteArray._
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpHeaderImplicits
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.service.HttpClientTransformerCombinators
import blueeyes.util.Future
import blueeyes.util.FutureImplicits
import java.util.concurrent.CountDownLatch
import net.lag.configgy.Configgy
import org.specs.Specification
import org.specs.util._

class HttpClientXLightWebSpec extends Specification with HttpClientTransformerCombinators with FutureImplicits{
  import HttpHeaderImplicits._

  val duration = 250
  val retries = 10
  
  private val httpClient = new HttpClientXLightWebEnginesString { }
  private val httpClientArrayByte = new HttpClientXLightWebEnginesArrayByte { }

  private val configPattern = """server {
    port = %d
    sslPort = %d
  }"""

  shareVariables()

  private var port = 8585
  private var server: Option[NettyEngineString] = None
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
      val f = path$[String, HttpResponse[String]]("http://127.0.0.1:666/foo") {
        get$[String, HttpResponse[String]] { r =>
	  println("response: " + r)
	  r
	}
      }(httpClient)
      f.error must eventually(retries, new Duration(duration))(beSomething)
    }
    
    "Support GET requests with status OK" in {
      val f = path$[String, HttpResponse[String]](uri) {
        get$[String, HttpResponse[String]] { r => r }
      }(httpClient)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.status.code must eventually(be(HttpStatusCodes.OK))
    }
    
    "Support GET requests with status Not Found" in {
      val f = path$[String, HttpResponse[String]]("/bogus") {
            get$[String, HttpResponse[String]]{ r => r }
	  }(httpClient)
      f.value must eventually(retries, new Duration(duration))(beNone)
    }

    "Support GET requests with query params" in {
      val f = path$[String, HttpResponse[String]](uri + "?param1=a&param2=b") {
        get$[String, HttpResponse[String]]{ r => r }
      }(httpClient)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support GET requests with request params" in {
      val f = path$[String, HttpResponse[String]](uri) {
        parameters$('param1 -> "a", 'param2 -> "b") {
          get$[String, HttpResponse[String]] { r => r }
        }
      }(httpClient)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with query params" in {
      val f = path$[String, HttpResponse[String]](uri + "?param1=a&param2=b") {
            post$[String, HttpResponse[String]]("") { r => r }
          }(httpClient)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with request params" in {
      val f = path$[String, HttpResponse[String]](uri) {
        parameters$('param1 -> "a", 'param2 -> "b") {
          post$[String, HttpResponse[String]]("") { r => r }
        }
      }(httpClient)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with body" in {
      val content = "Hello, world"
      val f = path$[String, HttpResponse[String]](uri) {
        post$[String, HttpResponse[String]](content) { r => r }
      }(httpClient)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must eventually(equalIgnoreSpace(content))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with body and request params" in {
      val content = "Hello, world"
      val f = path$[String, HttpResponse[String]](uri) {
        parameters$('param1 -> "a", 'param2 -> "b") {
          post$[String, HttpResponse[String]](content) { r => r }
        }
      }(httpClient)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must equalIgnoreSpace("param1=a&param2=b" + content)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support PUT requests with body" in {
      val content = "Hello, world"
      val f = path$[String, HttpResponse[String]](uri) {
        headers$(`Content-Length`(100)) {
          put$[String, HttpResponse[String]](content) { r => r }
        }
      }(httpClient)
      f.deliverTo((res: HttpResponse[String]) => {})
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must equalIgnoreSpace(content)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support GET requests with header" in {
      val f = path$[String, HttpResponse[String]](uri + "?headers=true") {
        headers$("Fooblahblah" -> "washere", "param2" -> "1") {
          get$[String, HttpResponse[String]] { r => r }
        }
      }(httpClient)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must include("Fooblahblah: washere") and include("param2: 1")
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with Content-Type: text/html & Content-Length: 100" in {
      val content = "<html></html>"
      val f = path$[String, HttpResponse[String]](uri) {
        headers$(`Content-Type`(text/html), `Content-Length`(100)) {
          post$[String, HttpResponse[String]](content) { r => r }
        }
      }(httpClient)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must beEqual(content)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with large payload" in {
      val content = Array.fill(1024*1000)(0).toList.mkString("")
      val f = path$[String, HttpResponse[String]](uri) {
        post$[String, HttpResponse[String]](content) { r => r }
      }(httpClient)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.get.trim must beEqual(content)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

   "Support HEAD requests" in {
      val f = path$[String, HttpResponse[String]](uri) {
        head$[String, HttpResponse[String]]{ r => r }
      }(httpClient)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

   "Support response headers" in {
      val f = path$[String, HttpResponse[String]](uri) {
        get$[String, HttpResponse[String]] { r => r }
      }(httpClient)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.status.code must be(HttpStatusCodes.OK)
      f.value.get.headers must haveKey("kludgy")
    }

    "Support GET requests of 1000 requests" in {
      val total = 1000
      val duration = 1000
      val futures = (0 until total).map { i =>
	path$[String, HttpResponse[String]](uri + "?test=true") {
          get$[String, HttpResponse[String]] { r => r }
        }(httpClient)
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
      val f = path$[Array[Byte], HttpResponse[Array[Byte]]](uri + "?param1=a&param2=b") {
        get$[Array[Byte], HttpResponse[Array[Byte]]]{ r => r }
      }(httpClientArrayByte)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.map(ByteArrayToString(_)).get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with body with Array[Byte]" in {
      val content = "Hello, world"
      val f = path$[Array[Byte], HttpResponse[Array[Byte]]](uri) {
        post$[Array[Byte], HttpResponse[Array[Byte]]](StringToByteArray(content)) { r => r }
      }(httpClientArrayByte)
      f.value must eventually(retries, new Duration(duration))(beSomething)
      f.value.get.content.map(ByteArrayToString(_)).get.trim must eventually(equalIgnoreSpace(content))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }
  }
}

import blueeyes.BlueEyesServiceBuilderString
import blueeyes.core.service.{HttpService, HttpReflectiveServiceList}

object EchoServer extends EchoService with HttpReflectiveServiceList[String] with NettyEngineString { }

trait EchoService extends BlueEyesServiceBuilderString {
  import blueeyes.core.http.MimeTypes._

  private implicit val ordering = new Ordering[Symbol] {
    def compare(l: Symbol, r: Symbol): Int = {
      l.name.compare(r.name)
    }
  }

  private def response(content: Option[String] = None) =
    HttpResponse[String](status = HttpStatus(HttpStatusCodes.OK), content = content, headers = Map("kludgy" -> "header test"))

  private def handler(request: HttpRequest[String]) = {
    val params = request.parameters.toList.sorted.foldLeft(List[String]()) { (l: List[String], p: Tuple2[Symbol, String]) =>
      l ++ List("%s=%s".format(p._1.name, p._2))
    }.mkString("&")
    val headers = request.parameters.get('headers).map { o =>
      request.headers.toList.sorted.foldLeft(List[String]()) { (l, h) =>
    	l ++ List("%s: %s".format(h._1, h._2))
      }.mkString("&")
    }.getOrElse("")
    val content = params + request.content.getOrElse("") + headers
    new Future[HttpResponse[String]]().deliver(response(content = Some(content)))
  }

  val echoService: HttpService[String] = service("echo", "1.0.0") { context =>
    request {
      produce(text/html) {
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
