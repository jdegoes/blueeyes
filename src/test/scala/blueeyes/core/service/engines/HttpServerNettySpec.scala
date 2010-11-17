package blueeyes.core.service.engines

import java.net.URI
import com.ning.http.client._
import blueeyes.core.service._
import org.specs.Specification
import blueeyes.util.Future
import blueeyes.core.http.MimeTypes._
import blueeyes.BlueEyesServiceBuilderString
import net.lag.configgy.Configgy
import java.util.concurrent.CountDownLatch
import blueeyes.core.http._

class HttpServerNettySpec extends Specification {

  private val configPattern = """server{port = %d}"""

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

          Configgy.configureFromString(configPattern.format(port))

          SampleServer.start.deliverTo { _ => doneSignal.countDown()}
          true
        }
        catch {
          case e: Throwable => {
            e.printStackTrace()
            port = port + 1
            false
          }
        }
      }while(!success)

      server = Some(SampleServer)
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
          get [String]{ request: HttpRequest[String] =>
            new Future[HttpResponse[String]]().deliver(response)
          }
        } ~ 
        path("/foo") {
          get [String]{ request: HttpRequest[String] =>
            new Future[HttpResponse[String]]().deliver(response)
          }  
        } ~
        path("/error") {
          get [String]{ request: HttpRequest[String] =>
            throw new RuntimeException("Unexecpcted Error.")
          }
        } ~
        path("/http/error") {
          get [String]{ request: HttpRequest[String] =>
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
