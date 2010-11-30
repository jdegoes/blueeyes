package blueeyes.core.service.engines

import java.net.URL
import com.ning.http.client._
import blueeyes.core.service._
import org.specs.Specification
import blueeyes.util.Future
import blueeyes.core.http.MimeTypes._
import blueeyes.BlueEyesServiceBuilderString
import net.lag.configgy.Configgy
import java.util.concurrent.CountDownLatch
import blueeyes.core.http._
import security.BlueEyesKeyStoreFactory
import java.security.cert.X509Certificate
import org.xlightweb.{GetRequest}

import org.xlightweb.client.{HttpClient => XLHttpClient}
import javax.net.ssl.{SSLContext, X509TrustManager, TrustManagerFactory}

class HttpServerNettySpec extends Specification {

  private val configPattern = """server{
  port = %d
  sslPort = %d
}"""

  shareVariables()

  private var port = 8585
  private var server: Option[NettyEngineString] = None
  
  "HttpServer" should{
    doFirst{
      var success = false
      do{
        SampleServer.sampleService
        success = try {
          val doneSignal = new CountDownLatch(1)

          Configgy.configureFromString(configPattern.format(port, port + 1))

          SampleServer.start.deliverTo { _ => doneSignal.countDown()}
          true
        }
        catch {
          case e: Throwable => {
            e.printStackTrace()
            port = port + 2
            false
          }
        }
      }while(!success)

      server = Some(SampleServer)
    }

    "return html by correct URI by https" in{
      val keyStore            = BlueEyesKeyStoreFactory(SampleServer.config)
      val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm())
      trustManagerFactory.init(keyStore)

      val sslContext  = SslContextFactory(keyStore, BlueEyesKeyStoreFactory.password, Some(trustManagerFactory.getTrustManagers))
      val httpClient = new XLHttpClient(sslContext)
      val response = httpClient.call(new GetRequest("https://localhost:%d/bar/foo/adCode.html".format(port + 1)))

      XLightWebRequestBijections.BodyDataSourceToString(response.getBody()) mustEqual (Context.context)
    }

    "return html by correct URI" in{
      val client = new AsyncHttpClient()
      val future = client.prepareGet("http://localhost:%d/bar/foo/adCode.html".format(port)).execute();

      val response = future.get
      response.getStatusCode mustEqual (HttpStatusCodes.OK.value)
      response.getResponseBody mustEqual (Context.context)
    }

    "return not found error by wrong URI" in{
      val client = new AsyncHttpClient()
      val future = client.prepareGet("http://localhost:%d/foo/foo/adCode.html".format(port)).execute();

      val response = future.get
      response.getStatusCode mustEqual (HttpStatusCodes.NotFound.value)
    }
    "return Internall error when handling request crushes" in{
      val client = new AsyncHttpClient()
      val future = client.prepareGet("http://localhost:%d/error".format(port)).execute();

      val response = future.get
      response.getStatusCode mustEqual (HttpStatusCodes.InternalServerError.value)
    }
    "return Http error when handling request throws HttpException" in{
      val client = new AsyncHttpClient()
      val future = client.prepareGet("http://localhost:%d/http/error".format(port)).execute();

      val response = future.get
      response.getStatusCode mustEqual (HttpStatusCodes.BadRequest.value)
    }

    "return html by correct URI with parameters" in{
      val client = new AsyncHttpClient()
      val future = client.prepareGet("http://localhost:%d/foo?bar=zar".format(port)).execute();

      val response = future.get
      response.getStatusCode mustEqual (HttpStatusCodes.OK.value)
      response.getResponseBody mustEqual (Context.context)
    }

    doLast{
      server.foreach(_.stop)
    }
  }
}

object SampleServer extends SampleService with HttpReflectiveServiceList[String] with NettyEngineString { }

trait SampleService extends BlueEyesServiceBuilderString {
  import blueeyes.core.http.MimeTypes._
  
  private val response = HttpResponse[String](status = HttpStatus(HttpStatusCodes.OK), content = Some(Context.context))
  
  val sampleService: HttpService[String] = service("sample", "1.32") { context =>
    request {
      produce(text/html) {
        path("/bar/'adId/adCode.html") {
          get { request: HttpRequest[String] =>
            new Future[HttpResponse[String]]().deliver(response)
          }
        } ~ 
        path("/foo") {
          get { request: HttpRequest[String] =>
            new Future[HttpResponse[String]]().deliver(response)
          }  
        } ~
        path("/error") {
          get { request: HttpRequest[String] =>
            throw new RuntimeException("Unexecpcted Error.")
          }
        } ~
        path("/http/error") {
          get { request: HttpRequest[String] =>
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
