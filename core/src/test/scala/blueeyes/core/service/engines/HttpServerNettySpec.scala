package blueeyes.core.service.engines

import blueeyes.core.service._
import collection.mutable.ArrayBuilder.ofByte
import org.specs2.mutable.Specification

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.Await
import akka.util._

import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http._
import blueeyes.core.data.{FileSink, FileSource, ByteMemoryChunk, ByteChunk, BijectionsByteArray, BijectionsChunkString}
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.http.HttpStatusCodes._
import security.BlueEyesKeyStoreFactory

import java.io.File
import java.util.concurrent.CountDownLatch
import javax.net.ssl.TrustManagerFactory

import net.lag.configgy.{ConfigMap, Configgy}

import blueeyes.concurrent.test.FutureMatchers
import org.specs2.specification.{Step, Fragments}
import org.specs2.time.TimeConversions._

class HttpServerNettySpec extends Specification with BijectionsByteArray with BijectionsChunkString with blueeyes.bkka.AkkaDefaults with FutureMatchers {

  private val configPattern = """server{
  port = %d
  sslPort = %d
}"""

  val duration = Duration(350, "millis")
  val retries = 50

  implicit val testTimeouts = FutureTimeouts(50, duration)

  private var port = 8585
  private var server: Option[NettyEngine] = None
  
  override def is = args(sequential = true) ^ super.is
  override def map(fs: =>Fragments) = Step {
    var error: Option[Throwable] = None
    do{
      val sampleServer = new SampleServer()
      val doneSignal   = new CountDownLatch(1)

      Configgy.configureFromString(configPattern.format(port, port + 1))

      val startFuture = sampleServer.start

      startFuture.onSuccess { case _ =>
        error = None
        doneSignal.countDown()
      }
      startFuture.onFailure { case v =>
        println("Error trying to start server on ports " + port + ", " + (port + 1))
        error = Some(v)
        port  = port + 2
        doneSignal.countDown()
      }

      server = Some(sampleServer)

      doneSignal.await()
    }while(error != None)
  } ^ fs ^ Step {
    Context.dataFile.delete
    server.foreach(_.stop)
  }
  

  "HttpServer" should {
    "return empty response"in{
      client.post("/empty/response")("") must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status.code must be (OK)) and
            (content must beNone)
        }
      }
    }

    "write file"in{
      Context.dataFile.delete

      client.post("/file/write")("foo") must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status.code must be (OK)) and
            (Context.dataFile.exists must be_==(true)) and
            (Context.dataFile.length mustEqual("foo".length))
        }
      }
    }

    "read file"in{
      Context.dataFile.delete

      akka.dispatch.Await.result(client.post("/file/write")("foo"), duration)
      client.get("/file/read") must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status.code must be (OK)) and
            (content must beSome("foo")) 
        }
      }
    }

    "return html by correct URI" in{
      client.get("/bar/foo/adCode.html") must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status.code must be (OK)) and
            (content must beSome(Context.context))
        }
      }
    }

    "return NotFound when accessing a nonexistent URI" in{
      val response = Await.result(client.post("/foo/foo/adCode.html")("foo").failed, duration)
      response must beLike { case HttpException(failure, _) => failure must_== NotFound }
    }
    "return InternalServerError when handling request crashes" in{
      val response = Await.result(client.get("/error").failed, duration)
      response must beLike { case HttpException(failure, _) => failure must_== InternalServerError }
    }
    "return Http error when handling request throws HttpException" in {
      val response = Await.result(client.get("/http/error").failed, duration)
      response must beLike { case HttpException(failure, _) => failure must_== BadRequest }
    }

    "return html by correct URI with parameters" in{
      client.parameters('bar -> "zar").get("/foo") must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status.code must be (OK)) and 
            (content must beSome(Context.context))
        }
      }
    }
    "return huge content"in{
      client.get[ByteChunk]("/huge") must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status.code must be (OK)) and
            (content.map(v => readContent(v)) must beSome(Context.hugeContext.map(v => new String(v).mkString("")).mkString("")))
        }
      }
    }
    "return huge delayed content"in{
      val content = client.get[ByteChunk]("/huge/delayed") 
      content must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status.code must be (OK)) and
            (content.map(v => readContent(v)) must beSome(Context.hugeContext.map(v => new String(v).mkString("")).mkString("")))
        }
      }
    }
    "return html by correct URI by https" in{
      sslClient.get("/bar/foo/adCode.html") must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            content.get mustEqual(Context.context)
        }
      }
    }
    "return huge content by https"in{
      sslClient.get[ByteChunk]("/huge") must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status.code must be (OK)) and
            (content.map(v => readContent(v)) must beSome(Context.hugeContext.map(v => new String(v).mkString("")).mkString(""))) 
        }
      }
    }
  }

  private def readContent(chunk: ByteChunk): String = {
    val promise = Promise[String]()
    readContent(chunk, new ofByte(), promise)
    akka.dispatch.Await.result(promise, duration)
  }

  private def readContent(chunk: ByteChunk, buffer: ofByte, promise: Promise[String]) {
    buffer ++= chunk.data

    chunk.next match{
      case Some(x) => x.onSuccess { case nextChunk => readContent(nextChunk, buffer, promise) }
      case None => promise.success(new String(buffer.result, "UTF-8"))
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

  val sampleService: Service[ByteChunk] = service("sample", "1.32") { context =>
    request {
      produce(text/html) {
        path("/bar/'adId/adCode.html") {
          get { request: HttpRequest[ByteChunk] =>
            Future[HttpResponse[String]](response)
          }
        } ~
        path("/foo") {
          get { request: HttpRequest[ByteChunk] =>
            Future[HttpResponse[String]](response)
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
          val chunk  = new ByteMemoryChunk(Context.hugeContext.head, () => Some(Future(new ByteMemoryChunk(Context.hugeContext.tail.head))))

          val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = Some(chunk))
          Future[HttpResponse[ByteChunk]](response)
        }
      } ~
      path("/empty/response"){
        post { request: HttpRequest[ByteChunk] =>
          Future[HttpResponse[ByteChunk]](HttpResponse[ByteChunk]())
        }
      } ~
      path("/file/write"){
        post { request: HttpRequest[ByteChunk] =>
          val promise = Promise[HttpResponse[ByteChunk]]()
          request.content.foreach{value =>
            val f = FileSink(Context.dataFile, value)
            f.onSuccess { case v => promise.success(HttpResponse[ByteChunk]()) } 
          }
          promise
        }
      } ~
      path("/file/read"){
        get { request: HttpRequest[ByteChunk] =>
          val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = FileSource(Context.dataFile))
          Future[HttpResponse[ByteChunk]](response)
        }
      } ~
      path("/huge/delayed"){
        get { request: HttpRequest[ByteChunk] =>

          val promise = Promise[ByteChunk]()
          import scala.actors.Actor.actor
          actor {
            Thread.sleep(2000)
            promise.success(new ByteMemoryChunk(Context.hugeContext.tail.head))
          }

          val chunk  = new ByteMemoryChunk(Context.hugeContext.head, () => Some(promise))

          val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = Some(chunk))
          Future[HttpResponse[ByteChunk]](response)
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
