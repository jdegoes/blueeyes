package blueeyes.core.service

import akka.dispatch.{Await, Future, ExecutionContext}

import blueeyes.BlueEyesServiceBuilder
import blueeyes.bkka._
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.HttpStatusCodes._

import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat

import blueeyes.akka_testing.FutureMatchers
import org.specs2.mutable.Specification
import org.specs2.specification.{Outside, Scope}

import  com.weiglewilczek.slf4s._

object TestServer extends HttpServerModule with TestAkkaDefaults {
  import DefaultBijections._

  class TestServices extends BlueEyesServiceBuilder with HttpRequestCombinators { 
    var startupCalled   = false
    var shutdownCalled  = false
    
    val testService = service("test", "1.0.7") {
      context => {
        startup {
          startupCalled = true
          Future("blahblah")
        } ->
        request { value: String =>
          encode[ByteChunk, Future[HttpResponse[String]], Future[HttpResponse[ByteChunk]]] {
            produce(text/plain) {
              path("/foo/bar") {
                get {
                  request: HttpRequest[ByteChunk] => Future(HttpResponse[String](content=Some(value)))
                } ~
                path("/error") { 
                  get[ByteChunk, Future[HttpResponse[String]]] { request: HttpRequest[ByteChunk] =>
                    sys.error("He's dead, Jim.")
                  }
                } ~
                path("/dead") {
                  get { request: HttpRequest[ByteChunk] =>
                    akka.dispatch.Promise.failed[HttpResponse[String]](new RuntimeException())
                  }
                }
              }
            }
          }
        } ->
        shutdown { value =>
          Future(shutdownCalled = true)
        }
      }
    }
  }

  class HttpServer(rootConfig: Configuration, executor: ExecutionContext, testServices: TestServices) extends HttpServerLike(rootConfig, List(testServices.testService), executor) { enclosing =>

    import HttpRequestHandlerImplicits._
    private implicit val M = new blueeyes.bkka.FutureMonad(executor)

    class TestService(svc: AsyncHttpService[ByteChunk, ByteChunk]) extends CustomHttpService[ByteChunk, Future[HttpResponse[ByteChunk]]] {
      def metadata = None
      def service = svc.service
      def startupCalled = testServices.startupCalled
      def shutdownCalled = testServices.shutdownCalled
    }

    override def start: Option[Future[(TestService, Option[Stoppable])]] = {
      super.start.map(_ map { case (svc, shutdown) => (new TestService(svc), shutdown) })
    }
  }

  def server(config: Configuration): HttpServer = server(config, defaultFutureDispatch)
  def server(config: Configuration, executor: ExecutionContext): HttpServer = {
    new HttpServer(config, executor, new TestServices)
  }
}

class HttpServerSpec extends Specification with FutureMatchers {
  import TestServer._
  import DefaultBijections._
  import akka.util.Duration._

  object server extends Outside[(HttpServer#TestService, Option[Stoppable])] with Scope {
    def outside = {
      val config = Configuration.parse("", BlockFormat)
      Await.result(TestServer.server(config).start.get, 10 seconds)
    }
  }

  "HttpServer.start" should {
    "executes start up function" in server { 
      case (svc, _) => svc.startupCalled must be_==(true)
    }
  }
  
  "HttpServer.apply" should {
    "delegate to service request handler" in server { 
      case (s, _) =>
        import HttpHeaders._
        import MimeTypes._
        s.service(HttpRequest[ByteChunk](HttpMethods.GET, "/test/v1/foo/bar")).toOption.get must whenDelivered {
          beLike {
            case HttpResponse(HttpStatus(status, _), headers, Some(content), _) =>
              (status must_== OK) and
              (content must beLike { case Left(buf) => new String(buf.array) must_== "blahblah" }) and
              (headers.header[`Content-Type`] must beSome(`Content-Type`(MimeTypes.text/plain)))
          }
        }
    }
    
    "produce NotFound response when service is not defined for request" in server { 
      case (s, _) =>
        s.service(HttpRequest[ByteChunk](HttpMethods.GET, "/test/v1/blahblah")).toOption.get must whenDelivered {
          beLike {
            case HttpResponse(HttpStatus(HttpStatusCodes.NotFound, _), _, _, _) => ok
          }
        }
    }

    "gracefully handle error-producing service handler" in server { 
      case (s, _) =>
        s.service(HttpRequest[ByteChunk](HttpMethods.GET, "/test/v1/foo/bar/error")).toOption.get must whenDelivered {
          beLike {
            case HttpResponse(HttpStatus(HttpStatusCodes.InternalServerError, _), _, _, _) => ok
          }
        }
    }
    "gracefully handle dead-future-producing service handler" in server { 
      case (s, _) =>
        s.service(HttpRequest[ByteChunk](HttpMethods.GET, "/test/v1/foo/bar/dead")).toOption.get must whenDelivered {
          beLike {
            case HttpResponse(HttpStatus(HttpStatusCodes.InternalServerError, _), _, _, _) => ok
          }
        }
    }
  }

  "HttpServer stop" should {
    "execute shut down function" in server { 
      case (s, Some(stoppable)) =>
        Stoppable.stop(stoppable, 10 seconds)
        (s.shutdownCalled must beTrue) 
    }
  }  
}


object FailServer extends HttpServerModule 
      with BlueEyesServiceBuilder 
      with HttpRequestCombinators { enclosing =>

  import DefaultBijections._

  def testService(implicit executor: ExecutionContext) = service("test", "1.0.7") {
    context => {
      startup {
        sys.error("Error during startup should not hang.")
        Future("Hello, world, I'm Unreachable!")
      } ->
      request { state: String =>
        path("/dead") {
          get { request: HttpRequest[ByteChunk] =>
            akka.dispatch.Promise.failed[HttpResponse[ByteChunk]](new RuntimeException()): Future[HttpResponse[ByteChunk]]
          }
        }
      } ->
      shutdown { state: String =>
        Future(())
      }
    }
  }

  class HttpServer(rootConfig: Configuration, executor: ExecutionContext) extends HttpServerLike(rootConfig, List(testService(executor)), executor) 

  def server(config: Configuration, executor: ExecutionContext): HttpServer = new HttpServer(config, executor)
}

class FailServerSpec extends Specification with FutureMatchers with TestAkkaDefaults {
  import TestServer._
  import DefaultBijections._
  import akka.util.Duration._

  "FailServer.start" should {
    "not hang if an exception is thrown in the startup function" in { 
      val config = Configuration.parse("", BlockFormat)
      Await.result(FailServer.server(config, defaultFutureDispatch).start.get, 10 seconds) must throwA[RuntimeException]
    }
  }
}
