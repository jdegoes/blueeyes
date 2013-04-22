package blueeyes.core.service
package engines.netty

import org.jboss.netty.handler.codec.http.{HttpResponse => NettyHttpResponse,
                                           DefaultHttpResponse => NettyDefaultHttpResponse,
                                           HttpResponseStatus => NettyHttpResponseStatus,
                                           HttpVersion => NettyHttpVersion}
import org.jboss.netty.handler.stream.ChunkedInput
import org.jboss.netty.channel._

import akka.dispatch._
import akka.util.Duration

import blueeyes.bkka._
import blueeyes.akka_testing.FutureMatchers
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.service._
import DefaultBijections._

import com.weiglewilczek.slf4s.Logging

import org.mockito.Mockito.{times, when}
import org.mockito.{Matchers, ArgumentMatcher}
import org.specs2.mock._
import org.specs2.mutable.Specification

import scalaz._

class HttpServiceUpstreamHandlerSpec extends Specification with Mockito with Logging with TestAkkaDefaults with FutureMatchers {
  import HttpNettyConverters._

  private val handler       = mock[AsyncHttpService[ByteChunk, ByteChunk]]
  private val context       = mock[ChannelHandlerContext]
  private val channel       = mock[Channel]
  private val channelFuture = mock[ChannelFuture]
  private val service       = mock[HttpRequest[ByteChunk] => Validation[NotServed, Future[HttpResponse[ByteChunk]]]]

  private val request       = HttpRequest[ByteChunk](HttpMethods.GET, URI("/bar/1/adCode.html"), Map[Symbol, String](), HttpHeaders.Empty, None, None, HttpVersions.`HTTP/1.0`)
  private val response      = HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.OK), Map("retry-after" -> "1"), Some(ByteChunk("12".getBytes("UTF-8"))), HttpVersions.`HTTP/1.1`)

  override def is = args(sequential = true) ^ super.is

  /* FIXME
  "write OK response service when path is match" in {
    val nettyHandler  = new HttpServiceUpstreamHandler(handler, defaultFutureDispatch)

    val event        = mock[MessageEvent]
    val future       = Promise.successful(response)
    val nettyMessage = toNettyResponse(response, true)
    val nettyContent = new StreamChunkedInput(StreamT.empty[Future, ByteBuffer], channel)

    when(event.getMessage()).thenReturn(request, request)
    when(handler.service).thenReturn(service)
    when(service.apply(request)).thenReturn(Success(future))
    when(event.getChannel()).thenReturn(channel)
    when(event.getChannel().isConnected).thenReturn(true)
    when(channel.write(Matchers.argThat(new RequestMatcher(nettyMessage)))).thenReturn(channelFuture)
    when(channel.write(Matchers.argThat(new ContentMatcher(nettyContent)))).thenReturn(channelFuture)

    nettyHandler.messageReceived(context, event)
    Thread.sleep(2000)

    there was one(channelFuture).addListener(ChannelFutureListener.CLOSE)
  }
  */

  "cancel Future when connection closed" in {
    val nettyHandler  = new HttpServiceUpstreamHandler(handler, defaultFutureDispatch)
    val event        = mock[MessageEvent]
    val stateEvent   = mock[ChannelStateEvent]
    val promise      = Promise[HttpResponse[ByteChunk]]()

    when(event.getMessage()).thenReturn(request, request)
    when(handler.service).thenReturn(service)
    when(service.apply(request)).thenReturn(Success(promise))

    nettyHandler.messageReceived(context, event)

    nettyHandler.channelDisconnected(context, stateEvent)

    Await.result(promise.failed, 10 seconds) must haveSuperclass[Throwable]
  }

  "return a bad request code when server on bad urls" in {
    import org.mockito.Matchers

    val nettyHandler  = new HttpServiceUpstreamHandler(handler, defaultFutureDispatch)
    val event        = mock[MessageEvent]
    val stateEvent   = mock[ChannelStateEvent]
    val exceptionEvent = new DefaultExceptionEvent(channel, HttpException(HttpStatusCodes.BadRequest, "bad mojo"))

    channel.isOpen() returns true
    channel.isConnected() returns true
    context.getChannel() returns channel

    nettyHandler.exceptionCaught(context, exceptionEvent)
    nettyHandler.channelDisconnected(context, stateEvent)

    val badSyntaxStatus = new NettyHttpResponseStatus(400, "The request contains bad syntax or cannot be fulfilled.")
    val expectedResponse = new NettyDefaultHttpResponse(NettyHttpVersion.HTTP_1_1, badSyntaxStatus)
    val matchBadSyntaxResponse = new RequestMatcher(expectedResponse)
    there was one(channel).write(Matchers argThat matchBadSyntaxResponse)
  }

  class RequestMatcher(matchingResponce: NettyHttpResponse) extends ArgumentMatcher[NettyHttpResponse] {
     def matches(arg: Object ): Boolean = {
       val response = arg.asInstanceOf[NettyHttpResponse]
       response != null && matchingResponce.getStatus == response.getStatus
     }
  }
  class ContentMatcher(chunkedInput: ChunkedInput) extends ArgumentMatcher[NettyHttpResponse] {
     def matches(arg: Object ): Boolean = {
       arg != null
     }
  }
}
