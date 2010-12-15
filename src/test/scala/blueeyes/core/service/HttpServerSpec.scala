package blueeyes.core.service

import org.spex.Specification
import blueeyes.BlueEyesServiceBuilderString
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.http.MimeTypes._
import net.lag.configgy.Configgy
import blueeyes.core.http.{HttpMethods, HttpResponse, HttpRequest}
import blueeyes.util.Future

class HttpServerSpec extends Specification{

  private val server = new TestServer()

  Configgy.configureFromString("")
  server.start

  "HttpServer start: executes start up function" in{
    server.startupCalled must be (true)
  }
  "HttpServer start: sets status to Starting" in{
    server.status must be (RunningStatus.Started)
  }
  "HttpServer: adds handlers" in{
    server.isDefinedAt(HttpRequest[String](HttpMethods.GET, "/blahblah")) must be (false)
    server.isDefinedAt(HttpRequest[String](HttpMethods.GET, "/foo/bar")) must be (true)
  }
  "HttpServer: path start up value to handler" in{
    server.apply(HttpRequest[String](HttpMethods.GET, "/foo/bar")).value must beSome(HttpResponse[String](content=Some("blahblah"), headers = Map("Content-Type" -> "text/plain")))
  }

  "HttpServer stop: executes shut down function" in{
    server.stop

    server.shutdownCalled must be (true)
  }
  "HttpServer stop: sets status to Stopped" in{
    server.stop

    server.status must be (RunningStatus.Stopped)
  }  
}

class TestServer extends TestService with HttpReflectiveServiceList[String]

trait TestService extends HttpServer[String] with BlueEyesServiceBuilderString with HttpRequestCombinators{
  var startupCalled   = false
  var shutdownCalled  = false
  lazy val testService = service("test", "1.0.7") {
    context => {
      startup {
        startupCalled = true
        "blahblah"
      } ->
      request { value: String =>
        path("/foo/bar") {
          produce(text/plain) {
            get {
              request: HttpRequest[String] => Future(HttpResponse[String](content=Some(value)))
            }
          }
        }
      } ->
      shutdown { value =>
        shutdownCalled = true
      }
    }
  }
}