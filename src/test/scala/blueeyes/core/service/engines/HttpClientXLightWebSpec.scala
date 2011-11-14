package blueeyes.core.service.engines

import blueeyes.core.http._
import blueeyes.core.data._
import blueeyes.core.service.HttpRequestHandlerCombinators
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.HttpStatusCodes._
import net.lag.configgy.Configgy
import org.specs2.mutable.Specification
import org.specs2.time.TimeConversions._
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future.FutureFunctor

import collection.mutable.ArrayBuilder.ofByte
import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import org.specs2.specification.{Step, Fragments}

class HttpClientXLightWebSpec extends Specification with BijectionsChunkString with ContentReader with HttpRequestHandlerCombinators {
  val duration = 250.milliseconds
  val retries = 30

  private val httpClient = new HttpClientXLightWeb

  private val configPattern = """server {
    port = %d
    sslPort = %d
  }"""

  private var port = 8585
  private var server: Option[NettyEngine] = None
  private var uri = ""

  override def is = args(sequential = true) ^ super.is
  override def map(fs: =>Fragments) = Step {
    var success = false
    do {
      EchoServer.echoService
      success = try {
        Configgy.configureFromString(configPattern.format(port, port + 1))

        EchoServer.start
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
  } ^ fs ^ Step {
    EchoServer.stop
  }


  "HttpClientXLightWeb" should {
    "Support GET to invalid server should return http error" in {
      val f = httpClient.get[String]("http://127.0.0.1:666/foo")
      f.error must eventually(retries, duration)(beNone)
    }

    "Support GET to invalid URI/404 should cancel Future" in {
      val f = httpClient.get(uri + "bogus")
      f.isCanceled must eventually(retries, duration)(beTrue)
      f.error must beLike {case Some(HttpException(NotFound, _)) => ok}
    }

    "Support GET requests with status OK" in {
      val f = httpClient.get(uri)
      f.value must eventually(retries, duration)(beSome)
      f.value.get.status.code must eventually(be(HttpStatusCodes.OK))
    }

    "Support GET requests with status Not Found" in {
      val f = httpClient.get("/bogus")
      f.value must eventually(retries, duration)(beNone)
    }

    "Support GET requests with query params" in {
      val f = httpClient.get(uri + "?param1=a&param2=b")
      f.value must eventually(retries, duration)(beSome)
      f.value.get.content.get.trim must eventually(be_==/("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support GET requests with request params" in {
      val f = httpClient.parameters('param1 -> "a", 'param2 -> "b").get(uri)
      f.value must eventually(retries, duration)(beSome)
      f.value.get.content.get.trim must eventually(be_==/("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with query params" in {
      val f = httpClient.post(uri + "?param1=a&param2=b")("")
      f.value must eventually(retries, duration)(beSome)
      f.value.get.content.get.trim must eventually(be_==/("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with request params" in {
      val f = httpClient.parameters('param1 -> "a", 'param2 -> "b").post(uri)("")
      f.value must eventually(retries, duration)(beSome)
      f.value.get.content.get.trim must eventually(be_==/("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with body" in {
      val content = "Hello, world"
      val f = httpClient.post(uri)(content)
      f.value must eventually(retries, duration)(beSome)
      f.value.get.content.get.trim must eventually(be_==/(content))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with body and request params" in {
      val content = "Hello, world"
      val f = httpClient.parameters('param1 -> "a", 'param2 -> "b").post(uri)(content)
      f.value must eventually(retries, duration)(beSome)
      f.value.get.content.get.trim must be_==/("param1=a&param2=b" + content)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support PUT requests with body" in {
      val content = "Hello, world"
      val f = httpClient.header(`Content-Length`(100)).put(uri)(content)
      f.deliverTo((res: HttpResponse[String]) => {})
      f.value must eventually(retries, duration)(beSome)
      f.value.get.content.get.trim must be_==/(content)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support GET requests with header" in {
      val f = httpClient.header("Fooblahblah" -> "washere").header("param2" -> "1").get(uri + "?headers=true")
      f.value must eventually(retries, duration)(beSome)
      f.value.get.content.get.trim must contain("Fooblahblah: washere") and contain("param2: 1")
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with Content-Type: text/html & Content-Length: 100" in {
      val content = "<html></html>"
      val f = httpClient.headers(`Content-Type`(text/html) :: `Content-Length`(100) :: Nil).post(uri)(content)
      f.value must eventually(retries, duration)(beSome)
      f.value.get.content.get.trim must_==(content)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with large payload" in {
      val content = Array.fill(2048*1000)(0).toList.mkString("")
      val f = httpClient.post(uri)(content)
      f.value must eventually(retries, duration)(beSome)
      f.value.get.content.get.trim must_==(content)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }
    "Support POST requests with large payload with several chunks" in {
      val content = Array.fill[Byte](2048*100)('0')
      val chunk   = new ByteMemoryChunk(content, () => Some(Future.sync(new ByteMemoryChunk(content))))
      val f = httpClient.post[ByteChunk](uri)(chunk)
      f.value must eventually(retries * 3, duration)(beSome)
      (new String(f.value.get.content.get.data).length) must_==(new String(content ++ content).length)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

   "Support HEAD requests" in {
      val f = httpClient.head(uri)
      f.value must eventually(retries, duration)(beSome)
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

   "Support response headers" in {
      val f = httpClient.get(uri)
      f.value must eventually(retries, duration)(beSome)
      f.value.get.status.code must be(HttpStatusCodes.OK)
      f.value.get.headers.raw must haveKey("kludgy")
    }

    "Support GET requests of 1000 requests" in {
      val total = 1000
      val duration = 1000.milliseconds
      val futures = (0 until total).map { i =>
        httpClient.get(uri + "?test=true")
      }
      val responses = futures.foldLeft(0) {
	(acc, f) => {
          f.value must eventually(retries, duration)(beSome)
          f.value.get.status.code must be(HttpStatusCodes.OK)
          acc + 1
	}
      }

      responses must_==(total)
    }

    "Support GET requests with query params" in {
      val f = httpClient.get[String](uri + "?param1=a&param2=b")
      f.value must eventually(retries, duration)(beSome)
      f.value.get.content.get.trim must eventually(be_==/("param1=a&param2=b"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with body with Array[Byte]" in {
      import BijectionsChunkByteArray._
      import BijectionsByteArray._
      val content = "Hello, world"
      val f = httpClient.post(uri)(StringToByteArray(content))
      f.value must eventually(retries, duration)(beSome)
      f.value.get.content.map(ByteArrayToString(_)).get.trim must eventually(be_==/(content))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }
    "Support POST requests with body several chunks and transcoding" in {
      import BijectionsChunkByteArray._
      import BijectionsByteArray._
      implicit val bijection = chunksToChunksArrayByte[String]

      val chunks: Chunk[String] = new MemoryChunk[String]("foo", () => Some(Future.sync[Chunk[String]](new MemoryChunk[String]("bar"))))

      val f = httpClient.post(uri)(chunks)(bijection)
      f.value must eventually(retries, duration)(beSome)
      val content = readContent(f.value.get.content.get).map(_.mkString(""))
      content.value must eventually(retries, duration)(beSome("foobar"))
      f.value.get.status.code must be(HttpStatusCodes.OK)
    }

    "Support POST requests with encoded URL should be preserved" in {
      val content = "Hello, world"
      val f = httpClient.post(uri + "?headers=true&plckForumId=Cat:Wedding%20BoardsForum:238")(content)
      f.value must eventually(retries, duration)(beSome)
      f.value.get.status.code must be(HttpStatusCodes.OK)
      f.value.get.content.get.contains("plckForumId=Cat:Wedding BoardsForum:238") must beTrue
    }
  }
}

import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.service._

object EchoServer extends EchoService with HttpReflectiveServiceList[ByteChunk] with NettyEngine{ }

trait ContentReader{
  def readContent[T](chunk: Chunk[T]): Future[List[T]] = {
    val result = new Future[List[T]]()
    readContent[T](chunk, List[T](), result)
    result
  }
  def readContent[T](chunk: Chunk[T], chunks: List[T], result: Future[List[T]]) {
    val newChunks = chunks ::: List(chunk.data)

    chunk.next match{
      case Some(x) => x.deliverTo(nextChunk => readContent(nextChunk, newChunks, result))
      case None => result.deliver(newChunks)
    }
  }
}

trait EchoService extends BlueEyesServiceBuilder with BijectionsChunkString with ContentReader{
  import blueeyes.core.http.MimeTypes._

  private implicit val ordering = new Ordering[Symbol] {
    def compare(l: Symbol, r: Symbol): Int = {
      l.name.compare(r.name)
    }
  }

  private def response(content: Option[String] = None) =
    HttpResponse[String](status = HttpStatus(HttpStatusCodes.OK), content = content, headers = Map("kludgy" -> "header test"))

  private def handler(request: HttpRequest[ByteChunk]) = {
    val params = request.parameters.toList.sorted.foldLeft(List[String]()) { (l: List[String], p: Tuple2[Symbol, String]) =>
      l ++ List("%s=%s".format(p._1.name, p._2))
    }.mkString("&")
    val headers = request.parameters.get('headers).map { o =>
      request.headers.raw.toList.sorted.foldLeft(List[String]()) { (l, h) =>
    	l ++ List("%s: %s".format(h._1, h._2))
      }.mkString("&")
    }.getOrElse("")
    val content: Future[String] = request.content.map{v =>
      readStringContent(v)
    }.getOrElse(Future.sync[String](""))
    val result = new Future[HttpResponse[String]]()
    content.deliverTo{v => result.deliver(response(content = Some(params + v + headers)))}
    content.ifCanceled(th => result.cancel(th))
    result
  }

  private def readStringContent(chunk: ByteChunk) = {
    val result = readContent(chunk)
    result.map(_.map(v => new String(v, "UTF-8")).mkString(""))
  }
  private def readContent(chunk: ByteChunk, buffer: ofByte, result: Future[String]) {
    buffer ++= chunk.data

    chunk.next match{
      case Some(x) => x.deliverTo(nextChunk => readContent(nextChunk, buffer, result))
      case None => result.deliver(new String(buffer.result, "UTF-8"))
    }
  }

  val echoService: Service[ByteChunk] = service("echo", "1.0.0") { context =>
    request {
      path("/echo") {
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
