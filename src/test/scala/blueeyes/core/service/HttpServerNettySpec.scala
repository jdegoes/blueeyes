package blueeyes.core.service

import java.net.URI
import com.ning.http.client._
import org.specs.Specification
import blueeyes.core.service.RestPathPatternImplicits._
import blueeyes.util.{Future}
import blueeyes.core.data.{TextToTextBijection}
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpMethod, HttpVersion, HttpMethods, HttpVersions, HttpRequest, HttpResponse, HttpStatusCode, HttpStatus, HttpStatusCodes, MimeType}

class HttpServerNettySpec extends Specification{
  @volatile
  private var port = 8585
  @volatile
  private var server: Option[HttpServerNetty] = None
  "HttpServer" should{
    doFirst{
      val testServer = new HttpServerNetty(classOf[TestService] :: Nil)

      var success = false
      do{
        success = try {
          testServer.start(port)
          true
        }
        catch {
          case e: Throwable => {
            port = port + 1
            false
          }
        }
      }while(!success)

      server = Some(testServer)
    }

    "return html by correct URI" in{
      val client = new AsyncHttpClient()
      val future = client.prepareGet("http://localhost:%d/bar/foo/adCode.html".format(port)).execute();

      val response = future.get
      response.getStatusCode mustEqual (HttpStatusCodes.OK.value)
      response.getResponseBody mustEqual (Context.context)
    }

    "return html by correct URI with parameters" in{
      val client = new AsyncHttpClient()
      val future = client.prepareGet("http://localhost:%d/foo?bar=zar".format(port)).execute();

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

class TestService extends RestHierarchyBuilder[String] with HttpService[String]{
  private implicit val transcoder = new HttpStringDataTranscoder(TextToTextBijection, text / html)
  path("/bar/'adId/adCode.html"){get(new Handler())}
  path("/foo"){get(new Handler())}

  def version = 1

  def name = "test-service"
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
