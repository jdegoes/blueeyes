package blueeyes.core.service
package engines.netty

import blueeyes.BlueEyesServer
import blueeyes.bkka._
import blueeyes.core.data.{FileSink, FileSource, ByteChunk, DefaultBijections}
import blueeyes.core.http._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.http.HttpStatusCodes._
import engines.security.BlueEyesKeyStoreFactory
import engines.{TestEngineService, TestEngineServiceContext, HttpClientXLightWeb}

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext
import akka.dispatch.Await
import akka.util._

import java.io.File
import java.util.concurrent.CountDownLatch
import javax.net.ssl.TrustManagerFactory

import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat

import blueeyes.concurrent.test.FutureMatchers

import org.specs2.mutable.Specification
import org.specs2.specification.{Step, Fragments}
import org.specs2.time.TimeConversions._

import scalaz._
import scalaz.syntax.monad._
import scala.collection.mutable.ArrayBuilder.ofByte

class HttpServerNettySpec extends Specification with AkkaDefaults with FutureMatchers {
  import DefaultBijections._

  implicit val M: Monad[Future] = new FutureMonad(defaultFutureDispatch)
  private val configPattern = """server { port = %d sslPort = %d }"""

  val duration: org.specs2.time.Duration = 1000.milliseconds
  implicit val testTimeouts = FutureTimeouts(50, duration)

  private val port = 8585
  private var stop: Stoppable = null
  private var config: Configuration = null

  override def is = args(sequential = true) ^ super.is

  override def map(fs: => Fragments) = {
    def startStep = Step {
      val config = Configuration.parse(configPattern.format(port, port + 1), BlockFormat)
      val sampleServer = (new SampleServer).server(config, defaultFutureDispatch)
      config = sampleServer.config

      sampleServer.start map { startFuture =>
        Await.result(startFuture map { case (_, shutdown) => stop = shutdown }, duration)
      } getOrElse {
        sys.error("No handlers available in service being started; aborting test.")
      }
    } 
    
    def stopStep = Step {
      TestEngineServiceContext.dataFile.delete
      Stoppable.stop(stop, testTimeouts)
    }

    startStep ^ fs ^ stopStep
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
      TestEngineServiceContext.dataFile.delete

      client.post("/file/write")("foo") must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status.code must be (OK)) and
            (TestEngineServiceContext.dataFile.exists must be_==(true)) and
            (TestEngineServiceContext.dataFile.length mustEqual("foo".length))
        }
      }
    }

    "read file"in{
      TestEngineServiceContext.dataFile.delete

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
            (content must beSome(TestEngineServiceContext.context))
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
            (content must beSome(TestEngineServiceContext.context))
        }
      }
    }
    "return huge content"in{
      client.get[ByteChunk]("/huge") must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status.code must be (OK)) and
            (content.map(v => readContent(v)) must beSome(TestEngineServiceContext.hugeContext.map(v => new String(v).mkString("")).mkString("")))
        }
      }
    }
    "return huge delayed content"in{
      val content = client.get[ByteChunk]("/huge/delayed")
      content must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status.code must be (OK)) and
            (content.map(v => readContent(v)) must beSome(TestEngineServiceContext.hugeContext.map(v => new String(v).mkString("")).mkString("")))
        }
      }
    }
    "return html by correct URI by https" in{
      sslClient.get("/bar/foo/adCode.html") must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            content.get mustEqual(TestEngineServiceContext.context)
        }
      }
    }
    "return huge content by https"in{
      sslClient.get[ByteChunk]("/huge") must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status.code must be (OK)) and
            (content.map(v => readContent(v)) must beSome(TestEngineServiceContext.hugeContext.map(v => new String(v).mkString("")).mkString("")))
        }
      }
    }
  }

  private def readContent(chunk: ByteChunk): String = {
    Await.result(ByteChunk.forceByteArray(chunk).map(new String(_, "UTF-8")), duration)
  }

  private def client    = new LocalHttpsClient(config).protocol("http").host("localhost").port(port)

  private def sslClient = new LocalHttpsClient(config).protocol("https").host("localhost").port(port + 1)
}

class SampleServer extends BlueEyesServer with TestEngineService

class LocalHttpsClient(config: Configuration) extends HttpClientXLightWeb {
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
          val chunk  = Chunk(Context.hugeContext.head, Some(Future(Chunk(Context.hugeContext.tail.head))))

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
          for (value <- request.content) {
            FileSink.write(Context.dataFile, value).onSuccess { 
              case _ => promise.success(HttpResponse[ByteChunk]()) 
            } 
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

          val promise = Promise[ByteBuffer]()
          import scala.actors.Actor.actor
          actor {
            Thread.sleep(2000)
            promise.success(ByteBuffer.wrap(Context.hugeContext.tail.head))
          }

          val chunk = Right(ByteBuffer.wrap(Context.hugeContent.head) :: (promise: Future[ByteBuffer]).liftM[StreamT])

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
