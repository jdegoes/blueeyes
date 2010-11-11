package blueeyes.core.service.engines

import java.net.URI
import com.ning.http.client._
import blueeyes.core.service._
import org.specs.Specification
import blueeyes.util.{Future}
import blueeyes.core.data.Bijections
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpVersions, HttpRequest, HttpResponse, HttpStatus, HttpStatusCodes}
import blueeyes.BlueEyesServiceBuilder
import net.lag.configgy.Configgy
import java.util.concurrent.CountDownLatch

class HttpServerNettySpec extends Specification{

  private val configPattern = """server{port = %d}"""

  shareVariables()


  private var port = 8585
  @volatile
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

    doLast{
      server.foreach(_.stop)
    }
  }
}

object SampleServer extends HttpReflectiveServiceList[String] with SampleService with NettyEngineString { }

trait SampleService extends BlueEyesServiceBuilder[String]{
  private implicit val transcoder = new HttpStringDataTranscoder(Bijections.StringToString, text / html)

  val sampleService: HttpService[String] = service("sample", "1.32") { context =>
    startup {
    } ->
    request { state: Unit =>
      path("/bar/'adId/adCode.html") {
        get [String]{ request: HttpRequest[String] =>
          new Future[HttpResponse[String]]().deliver(HttpResponse[String](HttpStatus(HttpStatusCodes.OK), Map("Content-Type" -> "text/html"), Some(Context.context), HttpVersions.`HTTP/1.1`))
        }
      }
    } ->
    shutdown {
    }
  }
}

class Handler extends Function1[HttpRequest[String], Future[HttpResponse[String]]]{
  def apply(request: HttpRequest[String]) = new Future[HttpResponse[String]]().deliver(HttpResponse[String](HttpStatus(HttpStatusCodes.OK), Map("Content-Type" -> "text/html"), Some(Context.context), HttpVersions.`HTTP/1.1`))
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
