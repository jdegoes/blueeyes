package blueeyes.core.service

import blueeyes.BlueEyesServiceBuilder
import blueeyes.concurrent.test._
import blueeyes.concurrent.Future
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.data.{ByteChunk, BijectionsChunkString}
import blueeyes.core.http._

import net.lag.configgy.Configgy
import org.specs2.mutable.Specification
import org.specs2.specification.{Outside, Scope}

class HttpServerSpec extends Specification with BijectionsChunkString with FutureMatchers {
  object server extends Outside[TestServer] with Scope {
    def outside = {
      Configgy.configureFromString("")
      new TestServer() ->- { _.start }
    }
  }

  "HttpServer.start" should {
    "executes start up function" in server { 
      _.startupCalled must be_==(true)
    }
    "set status to Starting" in server { 
      _.status must be (RunningStatus.Started)
    }
  }
  
  "HttpServer.apply" should {
    "delegate to service request handler" in server { s =>
      s.service(HttpRequest[ByteChunk](HttpMethods.GET, "/foo/bar")).toOption.get.value must beLike {
        case Some(HttpResponse(HttpStatus(status, _), headers, Some(content), _)) =>
          (status must_== OK) and
          (ChunkToString(content) must_== "blahblah") and
          (headers.get("Content-Type") must beSome("text/plain"))
      }
    }
    
    "produce NotFound response when service is not defined for request" in server { s =>
      s.service(HttpRequest[ByteChunk](HttpMethods.GET, "/blahblah")).toOption.get.value must beSome(HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.NotFound)))
    }

    "gracefully handle error-producing service handler" in server { s =>
      s.service(HttpRequest[ByteChunk](HttpMethods.GET, "/foo/bar/error")).toOption.get.value.get.status.code must be(HttpStatusCodes.InternalServerError)
    }
    "gracefully handle dead-future-producing service handler" in server { s =>
      val service1 = s.service(HttpRequest[ByteChunk](HttpMethods.GET, "/foo/bar/dead"))
      service1.toOption.get.value.get.status.code must be(HttpStatusCodes.InternalServerError)
    }
  }

  "HttpServer stop" should {
    "execute shut down function" in server { s =>
      s.stop
      (s.shutdownCalled must beTrue) and
      (s.status must be (RunningStatus.Stopped))
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
