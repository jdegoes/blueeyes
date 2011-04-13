package blueeyes.core.service.engines

import blueeyes.core.service._
import org.specs.Specification
import org.specs.util._
import blueeyes.concurrent.{Future, FutureDeliveryStrategySequential}
import blueeyes.core.http.MimeTypes._
import blueeyes.BlueEyesServiceBuilder
import java.util.concurrent.CountDownLatch
import blueeyes.core.data.BijectionsString
import blueeyes.core.http._
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.http.HttpStatusCodes._
import security.BlueEyesKeyStoreFactory
import javax.net.ssl.TrustManagerFactory
import net.lag.configgy.{ConfigMap, Configgy}

class HttpServerNettySpec extends Specification with FutureDeliveryStrategySequential with BijectionsString{

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
  private var client: LocalHttpsClient = _

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

      clientFacade = new SampleClientFacade(port, port + 1, new LocalHttpsClient(server.get.config))
    }

    "return html by correct URI by https" in{
      val response = clientFacade.httpsRequest
      response.value must eventually(retries, new Duration(duration))(beSomething)
      response.value.get.content.get mustEqual(Context.context)
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

    doLast{
      server.foreach(_.stop)
    }
  }
}

class SampleServer extends SampleService with HttpReflectiveServiceList[ChunkReader] with NettyEngine { }

class LocalHttpsClient(config: ConfigMap) extends HttpClientXLightWebEnginesString{
  override protected def createSSLContext = {
    val keyStore            = BlueEyesKeyStoreFactory(config)
    val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm())
    trustManagerFactory.init(keyStore)

    SslContextFactory(keyStore, BlueEyesKeyStoreFactory.password, Some(trustManagerFactory.getTrustManagers))
  }
}

class SampleClientFacade(port: Int, sslPort: Int, httpClient: HttpClient[String]) extends BijectionsString{

  def client    = httpClient.protocol("http").host("localhost").port(port)
  def sslClient = httpClient.protocol("https").host("localhost").port(sslPort)

  def httpsRequest          = sslClient.get("/bar/foo/adCode.html")

  def httpRequest           = client.get("/bar/foo/adCode.html")

  def httpRequestWithParams = client.parameters('bar -> "zar").get("/foo")

  def wrongHttpRequest      = client.post("/foo/foo/adCode.html")("foo")

  def errorHttpRequest      = client.get("/error")

  def httpErrorHttpRequest  = client.get("/http/error")
}

trait SampleService extends BlueEyesServiceBuilder with HttpRequestCombinators with BijectionsChunkReader{
  import blueeyes.core.http.MimeTypes._

  private val response = HttpResponse[String](status = HttpStatus(HttpStatusCodes.OK), content = Some(Context.context))

  val sampleService: HttpService[ChunkReader] = service("sample", "1.32") { context =>
    request {
      produce(text/html) {
        path("/bar/'adId/adCode.html") {
          get { request: HttpRequest[ChunkReader] =>
            new Future[HttpResponse[String]]().deliver(response)
          }
        } ~
        path("/foo") {
          get { request: HttpRequest[ChunkReader] =>
            new Future[HttpResponse[String]]().deliver(response)
          }
        } ~
        path("/error") {
          get { request: HttpRequest[ChunkReader] =>
            throw new RuntimeException("Unexecpcted Error.")
          }
        } ~
        path("/http/error") {
          get { request: HttpRequest[ChunkReader] =>
            throw HttpException(HttpStatusCodes.BadRequest)
          }
        }
      }
    }
  }
}

object Context{
  val context = """<html>
<head>
</head>

<body>
    <h1>Test</h1>
</body>
</html>"""
}
