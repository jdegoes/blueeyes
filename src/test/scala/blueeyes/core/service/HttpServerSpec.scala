package blueeyes.core.service

import org.specs.Specification
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.http.MimeTypes._
import blueeyes.core.data.{Chunk, BijectionsChunkReaderString}
import net.lag.configgy.Configgy
import blueeyes.concurrent.Future
import blueeyes.core.http._

class HttpServerSpec extends Specification with BijectionsChunkReaderString{

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
    "be always defined" in {
      server.isDefinedAt(HttpRequest[Chunk](HttpMethods.GET, "/blahblah")) must be (true)
      server.isDefinedAt(HttpRequest[Chunk](HttpMethods.GET, "/foo/bar")) must be (true)
    }
    
    "delegate to service request handler" in {
      server.apply(HttpRequest[Chunk](HttpMethods.GET, "/foo/bar")).value.map(response => response.copy(content=Some(ChunkReaderToString(response.content.get)))) must beSome(HttpResponse[String](content=Some("blahblah"), headers = Map("Content-Type" -> "text/plain")))
    }
    
    "produce NotFount response when service is not defined for request" in {
      server.apply(HttpRequest[Chunk](HttpMethods.GET, "/blahblah")).value must beSome(HttpResponse[Chunk](HttpStatus(HttpStatusCodes.NotFound)))
    }

    "gracefully handle error-producing service handler" in {
      server.apply(HttpRequest[Chunk](HttpMethods.GET, "/foo/bar/error")).value.get.status.code must be(HttpStatusCodes.InternalServerError)
    }
    "gracefully handle dead-future-producing service handler" in {
      server.apply(HttpRequest[Chunk](HttpMethods.GET, "/foo/bar/dead")).value.get.status.code must be(HttpStatusCodes.InternalServerError)
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

class TestServer extends TestService with HttpReflectiveServiceList[Chunk]

trait TestService extends HttpServer[Chunk] with BlueEyesServiceBuilder with HttpRequestCombinators with BijectionsChunkReaderString{
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
              request: HttpRequest[Chunk] => Future(HttpResponse[String](content=Some(value)))
            } ~
            path("/error") { 
              get { request: HttpRequest[Chunk] =>
                error("He's dead, Jim.")
              }
            } ~
            path("/dead") {
              get { request: HttpRequest[Chunk] =>
                Future.dead[HttpResponse[String]](new RuntimeException())
              }
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


//  "write response when Future is cancelled" in {
//
//    val event  = mock[MessageEvent]
//    val nettyRequest = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "/bar/1/adCode.html")
//    val request      = fromNettyRequest(nettyRequest, remoteAddress)
//    val future       = new Future[HttpResponse[String]]()
//
//    when(event.getMessage()).thenReturn(nettyRequest, nettyRequest)
//    when(event.getRemoteAddress()).thenReturn(remoteAddress)
//    when(handler.isDefinedAt(request)).thenReturn(true)
//    when(handler.apply(request)).thenReturn(future, future)
//    when(event.getChannel()).thenReturn(channel, channel)
//    when(channel.write(Matchers.argThat(new RequestMatcher(toNettyResponse(HttpResponse[String](HttpStatus(HttpStatusCodes.InternalServerError, InternalServerError.defaultMessage))))))).thenReturn(channelFuture, channelFuture)
//
//    nettyHandler.messageReceived(context, event)
//
//    future.cancel(HttpException(InternalServerError, InternalServerError.defaultMessage))
//
//    Mockito.verify(channelFuture, times(1)).addListener(ChannelFutureListener.CLOSE)
//  }