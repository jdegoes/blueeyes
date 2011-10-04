package blueeyes.core.service

import org.specs.Specification
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.http.MimeTypes._
import blueeyes.core.service.HttpServicePimps._
import blueeyes.core.data.{ByteChunk, BijectionsChunkString}
import net.lag.configgy.Configgy
import blueeyes.concurrent.Future
import blueeyes.core.http._

class HttpServerSpec extends Specification with BijectionsChunkString{

  private val server = new TestServer()

  Configgy.configureFromString("")
  server.start

  "HttpServer.start" should {
    "executes start up function" in{
      server.startupCalled must be (true)
    }
    "set status to Starting" in{
      server.status must be (RunningStatus.Started)
    }
  }
  
  "HttpServer.apply" should {

    "delegate to service request handler" in {
      val response = server.service(HttpRequest[ByteChunk](HttpMethods.GET, "/foo/bar"))
      response.toOption.get.value.map(response => response.copy(content=Some(ChunkToString(response.content.get)))) must beSome(HttpResponse[String](content=Some("blahblah"), headers = Map("Content-Type" -> "text/plain")))
    }
    
    "produce NotFount response when service is not defined for request" in {
      server.service(HttpRequest[ByteChunk](HttpMethods.GET, "/blahblah")).toOption.get.value must beSome(HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.NotFound)))
    }

    "gracefully handle error-producing service handler" in {
      server.service(HttpRequest[ByteChunk](HttpMethods.GET, "/foo/bar/error")).toOption.get.value.get.status.code must be(HttpStatusCodes.InternalServerError)
    }
    "gracefully handle dead-future-producing service handler" in {
      val service1 = server.service(HttpRequest[ByteChunk](HttpMethods.GET, "/foo/bar/dead"))
      service1.toOption.get.value.get.status.code must be(HttpStatusCodes.InternalServerError)
    }
  }

  "HttpServer stop" should {
    "execute shut down function" in {
      server.stop

      server.shutdownCalled must be (true)
    }
    
    "set status to Stopped" in {
      server.stop

      server.status must be (RunningStatus.Stopped)
    }
  }  
}

class TestServer extends TestService with HttpReflectiveServiceList[ByteChunk]

trait TestService extends HttpServer with BlueEyesServiceBuilder with HttpRequestCombinators with BijectionsChunkString{
  var startupCalled   = false
  var shutdownCalled  = false
  lazy val testService = service("test", "1.0.7") {
    context => {
      startup {
        startupCalled = true
        "blahblah".future
      } ->
      request { value: String =>
        path("/foo/bar") {
          produce(text/plain) {
            get {
              request: HttpRequest[ByteChunk] => Future.sync(HttpResponse[String](content=Some(value)))
            } ~
            path("/error") { 
              get[ByteChunk, Future[HttpResponse[String]]] { request: HttpRequest[ByteChunk] =>
                sys.error("He's dead, Jim.")
              }
            } ~
            path("/dead") {
              get { request: HttpRequest[ByteChunk] =>
                Future.dead[HttpResponse[String]](new RuntimeException())
              }
            }
          }
        }
      } ->
      shutdown { value =>
        Future.sync(shutdownCalled = true)
      }
    }
  }
}
