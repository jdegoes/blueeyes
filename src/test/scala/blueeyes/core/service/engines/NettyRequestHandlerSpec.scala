package blueeyes.core.service.engines

import org.specs.Specification
import org.specs.mock.MocksCreation
import org.jboss.netty.handler.codec.http.{HttpResponse => NettyHttpResponse}
import org.jboss.netty.handler.stream.ChunkedInput
import org.jboss.netty.channel._
import org.mockito.{Matchers, Mockito, ArgumentMatcher}
import blueeyes.concurrent.{Future, FutureDeliveryStrategySequential}
import blueeyes.core.service.RestPathPatternImplicits._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.service._
import blueeyes.core.data.{ByteChunk, MemoryChunk, BijectionsChunkReaderString}
import java.net.InetSocketAddress
import net.lag.logging.Logger
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._
import org.mockito.Mockito.{times, when}

class NettyRequestHandlerSpec extends Specification with NettyConverters with FutureDeliveryStrategySequential with MocksCreation with BijectionsChunkReaderString{
  private val handler       = mock[HttpRequestHandler[ByteChunk]]
  private val context       = mock[ChannelHandlerContext]
  private val channel       = mock[Channel]
  private val channelFuture = mock[ChannelFuture]

  private val request       = HttpRequest[ByteChunk](HttpMethods.GET, URI("/bar/1/adCode.html"), Map[Symbol, String](), HttpHeaders(), None, None, HttpVersions.`HTTP/1.0`)
  private val response      = HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.OK), Map("retry-after" -> "1"), Some(StringToChunkReader("12")), HttpVersions.`HTTP/1.1`)
  private val nettyHandler  = new NettyRequestHandler(handler, Logger.get)

  "write OK response service when path is match" in {
    val event        = mock[MessageEvent]
    val future       = new Future[HttpResponse[ByteChunk]]().deliver(response)
    val nettyMessage = toNettyResponse(response, true)
    val nettyContent = new NettyChunkedInput(new MemoryChunk(Array[Byte]()), channel)

    when(event.getMessage()).thenReturn(request, request)
    when(handler.isDefinedAt(request)).thenReturn(true)
    when(handler.apply(request)).thenReturn(future, future)
    when(event.getChannel()).thenReturn(channel, channel)
    when(event.getChannel().isConnected).thenReturn(true)
    when(channel.write(Matchers.argThat(new RequestMatcher(nettyMessage)))).thenReturn(channelFuture, channelFuture)
    when(channel.write(Matchers.argThat(new ContentMatcher(nettyContent)))).thenReturn(channelFuture, channelFuture)

    nettyHandler.messageReceived(context, event)

    Mockito.verify(channelFuture, times(1)).addListener(ChannelFutureListener.CLOSE)
  }

  "cancel Future when connection closed" in {
    val event        = mock[MessageEvent]
    val stateEvent   = mock[ChannelStateEvent]
    val future       = new Future[HttpResponse[ByteChunk]]()

    when(event.getMessage()).thenReturn(request, request)
    when(handler.isDefinedAt(request)).thenReturn(true)
    when(handler.apply(request)).thenReturn(future, future)

    nettyHandler.messageReceived(context, event)

    nettyHandler.channelDisconnected(context, stateEvent)

    future.isCanceled must be (true)
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
