package blueeyes.core.service.engines

import blueeyes.core.service._
import collection.mutable.ArrayBuilder.ofByte
import org.specs.Specification
import org.specs.util._
import blueeyes.concurrent.Future
import blueeyes.core.http.MimeTypes._
import blueeyes.core.service.HttpServicePimps._
import blueeyes.BlueEyesServiceBuilder
import java.util.concurrent.CountDownLatch
import blueeyes.core.http._
import blueeyes.core.data.{FileSink, FileSource, ByteMemoryChunk, ByteChunk, BijectionsByteArray, BijectionsChunkString, BijectionsIdentity}
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.http.HttpStatusCodes._
import security.BlueEyesKeyStoreFactory
import javax.net.ssl.TrustManagerFactory
import net.lag.configgy.{ConfigMap, Configgy}
import java.io.File

class HttpServerNettySpec extends Specification with BijectionsByteArray with BijectionsChunkString{

  private val configPattern = """server{
  port = %d
  sslPort = %d
}"""

  shareVariables()

  val duration = 350
  val retries = 50

  private var port = 8585
  private var server: Option[NettyEngine] = None

  "HttpServer" should {
    doFirst{
      var error: Option[Throwable] = None
      do{
        val sampleServer = new SampleServer()
        val doneSignal   = new CountDownLatch(1)

        Configgy.configureFromString(configPattern.format(port, port + 1))

        val startFuture = sampleServer.start

        startFuture.deliverTo { _ =>
          error = None
          doneSignal.countDown()
        }
        startFuture.ifCanceled{v =>
          println("Error trying to start server on ports " + port + ", " + (port + 1))
          error = v
          port  = port + 2
          doneSignal.countDown()
        }

        server = Some(sampleServer)

        doneSignal.await()
      }while(error != None)
    }

    "return empty response"in{
      val response = client.post("/empty/response")("")

      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.status.code must be (OK)
      response.value.get.content must beNone
    }
    "write file"in{
      Context.dataFile.delete

      val response = client.post("/file/write")("foo")

      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.status.code must be (OK)

      Context.dataFile.exists must be (true)
      Context.dataFile.length mustEqual("foo".length)
    }
    "read file"in{
      Context.dataFile.delete

      val postResponse = client.post("/file/write")("foo")
      postResponse.value must eventually(retries, new Duration(duration))(beSomething)

      val response = client.get("/file/read")

      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.status.code must be (OK)
      response.value.get.content must beSome("foo")
    }
    "return html by correct URI" in{
      val response =  client.get("/bar/foo/adCode.html")

      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.status.code must be (OK)
      response.value.get.content must beSome(Context.context)
    }

    "return not found error by wrong URI" in{
      val response = client.post("/foo/foo/adCode.html")("foo")

      response.isCanceled must eventually(retries, new Duration(duration)) (be (true))
      response.error.get.asInstanceOf[HttpException].failure mustEqual (NotFound)
    }
    "return Internall error when handling request crushes" in{
      val response = client.get("/error")

      response.isCanceled must eventually(retries, new Duration(duration)) (be (true))
      response.error.get.asInstanceOf[HttpException].failure mustEqual (InternalServerError)
    }
    "return Http error when handling request throws HttpException" in{
      val response = client.get("/http/error")

      response.isCanceled must eventually(retries, new Duration(duration))(be (true))
      response.error.get.asInstanceOf[HttpException].failure mustEqual (BadRequest)
    }

    "return html by correct URI with parameters" in{
      val response = client.parameters('bar -> "zar").get("/foo")

      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.status.code must be (OK)
      response.value.get.content must beSome(Context.context)
    }
    "return huge content"in{
      import BijectionsIdentity._
      val response = client.get[ByteChunk]("/huge")

      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.status.code must be (OK)
      response.value.get.content.map(v => readContent(v)) must beSome(Context.hugeContext.map(v => new String(v).mkString("")).mkString(""))
    }
    "return huge delayed content"in{
      import BijectionsIdentity._
      val response = client.get[ByteChunk]("/huge/delayed")

      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.status.code must be (OK)
      response.value.get.content.map(v => readContent(v)) must beSome(Context.hugeContext.map(v => new String(v).mkString("")).mkString(""))
    }
    "return html by correct URI by https" in{
      val response = sslClient.get("/bar/foo/adCode.html")
      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.content.get mustEqual(Context.context)
    }
    "return huge content by htpps"in{
      import BijectionsIdentity._
      val response = sslClient.get[ByteChunk]("/huge")

      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.status.code must be (OK)
      response.value.get.content.map(v => readContent(v)) must beSome(Context.hugeContext.map(v => new String(v).mkString("")).mkString(""))
    }

    doLast{
      Context.dataFile.delete
      server.foreach(_.stop)
    }
  }

  private def readContent(chunk: ByteChunk): String = {
    val result = new Future[String]()
    readContent(chunk, new ofByte(), result)

    result.value must eventually(retries, new Duration(duration)) (beSomething)
    result.value.get
  }
  private def readContent(chunk: ByteChunk, buffer: ofByte, result: Future[String]) {
    buffer ++= chunk.data

    chunk.next match{
      case Some(x) => x.deliverTo(nextChunk => readContent(nextChunk, buffer, result))
      case None => result.deliver(new String(buffer.result, "UTF-8"))
    }
  }
  private def client    = new LocalHttpsClient(server.get.config).protocol("http").host("localhost").port(port)
  private def sslClient = new LocalHttpsClient(server.get.config).protocol("https").host("localhost").port(port + 1)
}

class SampleServer extends SampleService with HttpReflectiveServiceList[ByteChunk] with NettyEngine { }

class LocalHttpsClient(config: ConfigMap) extends HttpClientXLightWeb {
  override protected def createSSLContext = {
    val keyStore            = BlueEyesKeyStoreFactory(config)
    val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm())
    trustManagerFactory.init(keyStore)

    SslContextFactory(keyStore, BlueEyesKeyStoreFactory.password, Some(trustManagerFactory.getTrustManagers))
  }
}

trait SampleService extends BlueEyesServiceBuilder with HttpRequestCombinators with BijectionsChunkString{
  import blueeyes.core.http.MimeTypes._

  private val response = HttpResponse[String](status = HttpStatus(HttpStatusCodes.OK), content = Some(Context.context))

  val sampleService: HttpService[ByteChunk] = service("sample", "1.32") { context =>
    request {
      produce(text/html) {
        path("/bar/'adId/adCode.html") {
          get { request: HttpRequest[ByteChunk] =>
            Future.sync[HttpResponse[String]](response)
          }
        } ~
        path("/foo") {
          get { request: HttpRequest[ByteChunk] =>
            Future.sync[HttpResponse[String]](response)
          }
        } ~
        path("/error") {
          get[ByteChunk, Future[HttpResponse[String]]] { request: HttpRequest[ByteChunk] =>
            throw new RuntimeException("Unexpected error (GET /error)")
          }
        } ~
        path("/http/error") {
          get[ByteChunk, Future[HttpResponse[String]]] { request: HttpRequest[ByteChunk] =>
            throw HttpException(HttpStatusCodes.BadRequest)
          }
        }
      } ~
      path("/huge"){
        get { request: HttpRequest[ByteChunk] =>
          val chunk  = new ByteMemoryChunk(Context.hugeContext.head, () => Some(Future.sync(new ByteMemoryChunk(Context.hugeContext.tail.head))))

          val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = Some(chunk))
          Future.sync[HttpResponse[ByteChunk]](response)
        }
      } ~
      path("/empty/response"){
        post { request: HttpRequest[ByteChunk] =>
          Future.sync[HttpResponse[ByteChunk]](HttpResponse[ByteChunk]())
        }
      } ~
      path("/file/write"){
        post { request: HttpRequest[ByteChunk] =>
          val future = new Future[HttpResponse[ByteChunk]]()
          request.content.foreach{value =>
            val f = FileSink(Context.dataFile, value)
            f.deliverTo(v => future.deliver(HttpResponse[ByteChunk]()))
          }
          future
        }
      } ~
      path("/file/read"){
        get { request: HttpRequest[ByteChunk] =>
          val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = FileSource(Context.dataFile))
          Future.sync[HttpResponse[ByteChunk]](response)
        }
      } ~
      path("/huge/delayed"){
        get { request: HttpRequest[ByteChunk] =>

          val nextChunkFuture = new Future[ByteChunk]()
          import scala.actors.Actor.actor
          actor {
            Thread.sleep(2000)
            nextChunkFuture.deliver(new ByteMemoryChunk(Context.hugeContext.tail.head))
          }

          val chunk  = new ByteMemoryChunk(Context.hugeContext.head, () => Some(nextChunkFuture))

          val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = Some(chunk))
          Future.sync[HttpResponse[ByteChunk]](response)
        }
      }
    }
  }
}

object Context{
  val dataFile = new File(System.getProperty("java.io.tmpdir") + File.separator + System.currentTimeMillis)

  val hugeContext = List[Array[Byte]]("first-".getBytes ++ Array.fill[Byte](2048*1000)('0'), "second-".getBytes ++ Array.fill[Byte](2048*1000)('0'))
  val context = """<html>
<head>
</head>

<body>
    <h1>Test</h1>
    <h1>Test</h1>
</body>
</html>"""
}
