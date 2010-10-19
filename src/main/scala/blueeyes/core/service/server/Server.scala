package blueeyes.core.service.server

import blueeyes.core.service._
import blueeyes.util.Future
import blueeyes.core.service.RestPathPatternImplicits._
import blueeyes.core.data.{Bijections}

object Server {
  def main(args: Array[String]) {
    val server = new TestServer()
    server.start(8585)(Bijections.textToText)
  }
}

class TestServer extends TestService with HttpServerNetty[String]

class TestService extends RestHierarchyBuilder[String]{
  path("bar/'adId/adCode.html"){get(new Handler())}
}

class Handler extends Function2[Map[Symbol, String], HttpRequest[String], Future[HttpResponse[String]]]{
  def apply(params: Map[Symbol, String], request: HttpRequest[String]) = {
    val future = new Future[HttpResponse[String]]()
    future.deliver(HttpResponse[String](HttpStatus(HttpStatusCodes.OK), Map("Content-Type" -> "text/html"), Some("""<html>
<head>
</head>

<body>
    <h1>Test</h1>
</body>
</html>"""), HttpVersions.Http_1_1))

    future
  }
}