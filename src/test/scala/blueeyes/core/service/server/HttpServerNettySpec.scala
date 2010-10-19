package blueeyes.core.service.server

import org.specs.Specification
import blueeyes.core.service._
import blueeyes.core.service.RestPathPatternImplicits._
import blueeyes.core.service.{HttpResponse, HttpRequest, RestHierarchyBuilder}
import blueeyes.util.{Future}
import blueeyes.core.data.Bijections

class HttpServerNettySpec extends Specification{
  @volatile
  private var server: Option[TestServer] = None
  "HttpServer" should{
    doFirst{
      val testServer = new TestServer()
      testServer.start(8585)(Bijections.textToText)

      server = Some(testServer)
    }

    doLast{
      server.foreach(_.stop)  
    }
  }
}

class TestServer extends TestService with HttpServerNetty[String]

class TestService extends RestHierarchyBuilder[String]{
  path("bar/'adId/adCode.html"){get(new Handler())}
}

class Handler extends Function2[Map[Symbol, String], HttpRequest[String], Future[HttpResponse[String]]]{
  def apply(params: Map[Symbol, String], request: HttpRequest[String]) = {
    val future = new Future[HttpResponse[String]]()
    future.deliver(HttpResponse[String](HttpStatus(HttpStatusCodes.OK), Map("Content-Type" -> "text/html"), Some(Context.context), HttpVersions.Http_1_1))

    future
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