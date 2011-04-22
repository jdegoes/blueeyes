package blueeyes.core.service.engines

import blueeyes.core.service._
import org.specs.Specification
import org.specs.util._
import blueeyes.concurrent.{Future, FutureDeliveryStrategySequential}
import blueeyes.core.http.MimeTypes._
import blueeyes.BlueEyesServiceBuilder
import java.util.concurrent.CountDownLatch
import blueeyes.core.http._
import blueeyes.core.data.{ByteMemoryChunk, ByteChunk, BijectionsByteArray, BijectionsIdentity, BijectionsChunkReaderString}
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.http.HttpStatusCodes._
import security.BlueEyesKeyStoreFactory
import javax.net.ssl.TrustManagerFactory
import net.lag.configgy.{ConfigMap, Configgy}

class HttpServerNettySpec extends Specification with FutureDeliveryStrategySequential{

  private val configPattern = """server{
  port = %d
  sslPort = %d
}"""

  shareVariables()

  val duration = 250
  val retries = 30

  private var port = 8585
  private var server: Option[NettyEngine] = None
  private var clientFacade: SampleClientFacade = _

  "HttpServer" should{
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

      clientFacade = new SampleClientFacade(port, port + 1, server.get.config)
    }

    "return empty response"in{
      val response = clientFacade.emptyResponse

      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.status.code must be (OK)
    }
    "return html by correct URI" in{
      val response =  clientFacade.httpRequest

      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.status.code must be (OK)
      response.value.get.content must beSome(Context.context)
    }

    "return not found error by wrong URI" in{
      val response = clientFacade.wrongHttpRequest

      response.isCanceled must eventually(retries, new Duration(duration)) (be (true))
      response.error.get.asInstanceOf[HttpException].failure mustEqual (NotFound)
    }
    "return Internall error when handling request crushes" in{
      val response = clientFacade.errorHttpRequest

      response.isCanceled must eventually(retries, new Duration(duration)) (be (true))
      response.error.get.asInstanceOf[HttpException].failure mustEqual (InternalServerError)
    }
    "return Http error when handling request throws HttpException" in{
      val response = clientFacade.httpErrorHttpRequest

      response.isCanceled must eventually(retries, new Duration(duration))(be (true))
      response.error.get.asInstanceOf[HttpException].failure mustEqual (BadRequest)
    }

    "return html by correct URI with parameters" in{
      val response = clientFacade.httpRequestWithParams

      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.status.code must be (OK)
      response.value.get.content must beSome(Context.context)
    }
    "return huge content"in{
      val response = clientFacade.hugeRequest

      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.status.code must be (OK)
      response.value.get.content must beSome(Context.hugeContext.map(v => new String(v).mkString("")).mkString(""))
    }
    "return huge delayed content"in{
      val response = clientFacade.hugeDelayedRequest

      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.status.code must be (OK)
      response.value.get.content must beSome(Context.hugeContext.map(v => new String(v).mkString("")).mkString(""))
    }
    "return html by correct URI by https" in{
      val response = clientFacade.httpsRequest
      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.content.get mustEqual(Context.context)
    }
    "return huge content by htpps"in{
      val response = clientFacade.httpsHugeRequest

      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.status.code must be (OK)
      response.value.get.content must beSome(Context.hugeContext.map(v => new String(v).mkString("")).mkString(""))
    }

    doLast{
      server.foreach(_.stop)
    }
  }
}

class SampleServer extends SampleService with HttpReflectiveServiceList[ByteChunk] with NettyEngine { }

class LocalHttpsClient(config: ConfigMap) extends HttpClientXLightWebEnginesString{
  override protected def createSSLContext = {
    val keyStore            = BlueEyesKeyStoreFactory(config)
    val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm())
    trustManagerFactory.init(keyStore)

    SslContextFactory(keyStore, BlueEyesKeyStoreFactory.password, Some(trustManagerFactory.getTrustManagers))
  }
}

class SampleClientFacade(port: Int, sslPort: Int, config: ConfigMap) extends BijectionsByteArray with BijectionsIdentity{

  def client    = new LocalHttpsClient(config).protocol("http").host("localhost").port(port)
  def sslClient = new LocalHttpsClient(config).protocol("https").host("localhost").port(sslPort)

  def httpsRequest          = sslClient.get("/bar/foo/adCode.html")

  def httpRequest           = client.get("/bar/foo/adCode.html")

  def httpRequestWithParams = client.parameters('bar -> "zar").get("/foo")

  def wrongHttpRequest      = client.post("/foo/foo/adCode.html")("foo")

  def errorHttpRequest      = client.get("/error")

  def httpErrorHttpRequest  = client.get("/http/error")

  def hugeRequest           = client.get("/huge")

  def hugeDelayedRequest    = client.get("/huge/delayed")

  def httpsHugeRequest      = sslClient.get("/huge")

  def emptyResponse         = client.post("/empty/response")("")
}

trait SampleService extends BlueEyesServiceBuilder with HttpRequestCombinators with BijectionsChunkReaderString{
  import blueeyes.core.http.MimeTypes._

  private val response = HttpResponse[String](status = HttpStatus(HttpStatusCodes.OK), content = Some(Context.context))

  val sampleService: HttpService[ByteChunk] = service("sample", "1.32") { context =>
    request {
      produce(text/html) {
        path("/bar/'adId/adCode.html") {
          get { request: HttpRequest[ByteChunk] =>
            Future.lift[HttpResponse[String]](response)
          }
        } ~
        path("/foo") {
          get { request: HttpRequest[ByteChunk] =>
            Future.lift[HttpResponse[String]](response)
          }
        } ~
        path("/error") {
          get { request: HttpRequest[ByteChunk] =>
            throw new RuntimeException("Unexecpcted Error.")
          }
        } ~
        path("/http/error") {
          get { request: HttpRequest[ByteChunk] =>
            throw HttpException(HttpStatusCodes.BadRequest)
          }
        }
      } ~
      path("/huge"){
        get { request: HttpRequest[ByteChunk] =>
          val chunk  = new ByteMemoryChunk(Context.hugeContext.head, () => Some(Future.lift(new ByteMemoryChunk(Context.hugeContext.tail.head))))

          val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = Some(chunk))
          Future.lift[HttpResponse[ByteChunk]](response)
        }
      } ~
      path("/empty/response"){
        post { request: HttpRequest[ByteChunk] =>
          Future.lift[HttpResponse[ByteChunk]](HttpResponse[ByteChunk]())
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
          Future.lift[HttpResponse[ByteChunk]](response)
        }
      }
    }
  }
}

object Context{
  val hugeContext = List[Array[Byte]]("""<html>
<head>
</head>

""".getBytes, """<body>
    <h1>Test</h1>
    <h1>Test</h1>
</body>
</html>""".getBytes)
  val context = """<html>
<head>
</head>

<body>
    <h1>Test</h1>
    <h1>Test</h1>
</body>
</html>"""
}
