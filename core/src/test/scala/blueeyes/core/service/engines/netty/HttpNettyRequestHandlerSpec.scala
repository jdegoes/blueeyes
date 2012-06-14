package blueeyes.core.service.engines.netty

import org.specs2.mutable.Specification
import org.jboss.netty.handler.codec.http.{HttpResponse => NettyHttpResponse}
import org.jboss.netty.handler.stream.ChunkedInput
import org.jboss.netty.channel._

import blueeyes.bkka.AkkaDefaults
import akka.dispatch._
import akka.util.Duration

import blueeyes.concurrent.test.FutureMatchers
import blueeyes.core.http.MimeTypes._
import blueeyes.core.service._
import blueeyes.core.data.{ByteChunk, Chunk, BijectionsChunkString}
import com.weiglewilczek.slf4s.Logging
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._
import org.mockito.Mockito.{times, when}
import scalaz.Scalaz._
import blueeyes.core.service.NotServed
import org.specs2.mock._
import org.mockito.{Matchers, ArgumentMatcher}
import scalaz.{Success, Validation}

class HttpNettyRequestHandlerSpec extends Specification with HttpNettyConverters with Mockito with BijectionsChunkString with Logging with AkkaDefaults with FutureMatchers {
  private val handler       = mock[AsyncCustomHttpService[ByteChunk]]
  private val context       = mock[ChannelHandlerContext]
  private val channel       = mock[Channel]
  private val channelFuture = mock[ChannelFuture]
  private val service       = mock[HttpRequest[ByteChunk] => Validation[NotServed, Future[HttpResponse[ByteChunk]]]]

  private val request       = HttpRequest[ByteChunk](HttpMethods.GET, URI("/bar/1/adCode.html"), Map[Symbol, String](), HttpHeaders.Empty, None, None, HttpVersions.`HTTP/1.0`)
  private val response      = HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.OK), Map("retry-after" -> "1"), Some(StringToChunk("12")), HttpVersions.`HTTP/1.1`)

  override def is = args(sequential = true) ^ super.is
  "write OK response service when path is match" in {
    val nettyHandler  = new HttpNettyRequestHandler(handler, logger)

    val event        = mock[MessageEvent]
    val future       = Promise.successful(response)
    val nettyMessage = toNettyResponse(response, true)
    val nettyContent = new NettyChunkedInput(Chunk(Array[Byte]()), channel)

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

  "cancel Future when connection closed" in {
    val nettyHandler  = new HttpNettyRequestHandler(handler, logger)
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
