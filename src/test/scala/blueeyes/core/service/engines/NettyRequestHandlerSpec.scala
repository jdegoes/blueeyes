package blueeyes.core.service.engines

import org.scalatest.mock.MockitoSugar
import org.specs.Specification
import org.jboss.netty.handler.codec.http.{HttpMethod => NettyHttpMethod, HttpVersion => NettyHttpVersion, HttpResponse => NettyHttpResponse}
import org.jboss.netty.handler.codec.http.DefaultHttpRequest
import org.jboss.netty.channel._
import org.jboss.netty.util.CharsetUtil
import org.mockito.Mockito.{when, times}
import org.mockito.{Matchers, Mockito, ArgumentMatcher}
import blueeyes.util.Future
import blueeyes.core.service.RestPathPatternImplicits._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.service._
import java.net.InetSocketAddress
import net.lag.logging.Logger
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._

class NettyRequestHandlerSpec extends Specification with MockitoSugar with NettyConverters{
  private val handler       = mock[HttpRequestHandler[String]]
  private val context       = mock[ChannelHandlerContext]
  private val channel       = mock[Channel]
  private val channelFuture = mock[ChannelFuture]

  private implicit val bijection  = NettyBijections.ChannelBufferToString

  private val response      = HttpResponse[String](HttpStatus(HttpStatusCodes.OK), Map("retry-after" -> "1"), Some("12"), HttpVersions.`HTTP/1.1`)
  private val nettyHandler  = new NettyRequestHandler(handler, Logger.get)
  private val remoteAddress = new InetSocketAddress("127.0.0.0", 8080)

  "write OK response service when path is match" in {
    val event  = mock[MessageEvent]
    val nettyRequest = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "/bar/1/adCode.html")
    val request      = fromNettyRequest(nettyRequest, remoteAddress)
    val future       = new Future[HttpResponse[String]]().deliver(response)

    when(event.getMessage()).thenReturn(nettyRequest, nettyRequest)
    when(event.getRemoteAddress()).thenReturn(remoteAddress)
    when(handler.isDefinedAt(request)).thenReturn(true)
    when(handler.apply(request)).thenReturn(future, future)
    when(event.getChannel()).thenReturn(channel, channel)
    when(channel.write(Matchers.argThat(new RequestMatcher(toNettyResponse(response))))).thenReturn(channelFuture, channelFuture)

    nettyHandler.messageReceived(context, event)

    Mockito.verify(channelFuture, times(1)).addListener(ChannelFutureListener.CLOSE)
  }

  "write Not Found response when path is not match" in {
    val event        = mock[MessageEvent]
    val nettyRequest = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "/foo/1/adCode.html")
    val request      = fromNettyRequest(nettyRequest, remoteAddress)

    when(event.getMessage()).thenReturn(nettyRequest, nettyRequest)
    when(handler.isDefinedAt(request)).thenReturn(false)
    when(event.getChannel()).thenReturn(channel, channel)
    when(channel.write( Matchers.argThat(new RequestMatcher(toNettyResponse(HttpResponse[String](HttpStatus(HttpStatusCodes.NotFound))))))).thenReturn(channelFuture, channelFuture)

    nettyHandler.messageReceived(context, event)

    Mockito.verify(channelFuture, times(1)).addListener(ChannelFutureListener.CLOSE)
  }
  "write response when Future is cancelled" in {

    val event  = mock[MessageEvent]
    val nettyRequest = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "/bar/1/adCode.html")
    val request      = fromNettyRequest(nettyRequest, remoteAddress)
    val future       = new Future[HttpResponse[String]]()

    when(event.getMessage()).thenReturn(nettyRequest, nettyRequest)
    when(event.getRemoteAddress()).thenReturn(remoteAddress)
    when(handler.isDefinedAt(request)).thenReturn(true)
    when(handler.apply(request)).thenReturn(future, future)
    when(event.getChannel()).thenReturn(channel, channel)
    when(channel.write(Matchers.argThat(new RequestMatcher(toNettyResponse(HttpResponse[String](HttpStatus(HttpStatusCodes.InternalServerError, InternalServerError.defaultMessage))))))).thenReturn(channelFuture, channelFuture)

    nettyHandler.messageReceived(context, event)

    future.cancel(HttpException(InternalServerError, InternalServerError.defaultMessage))

    Mockito.verify(channelFuture, times(1)).addListener(ChannelFutureListener.CLOSE)
  }

  "cancel Future when connection closed" in {
    val event        = mock[MessageEvent]
    val stateEvent   = mock[ChannelStateEvent]
    val nettyRequest = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "/bar/1/adCode.html")
    val request      = fromNettyRequest(nettyRequest, remoteAddress)
    val future       = new Future[HttpResponse[String]]()

    when(event.getMessage()).thenReturn(nettyRequest, nettyRequest)
    when(event.getRemoteAddress()).thenReturn(remoteAddress)
    when(handler.isDefinedAt(request)).thenReturn(true)
    when(handler.apply(request)).thenReturn(future, future)

    nettyHandler.messageReceived(context, event)

    nettyHandler.channelDisconnected(context, stateEvent)

    future.isCanceled must be (true)
  }

  class RequestMatcher(matchingResponce: NettyHttpResponse) extends ArgumentMatcher[NettyHttpResponse] {
     def matches(arg: Object ): Boolean = {
       val repsonce = arg.asInstanceOf[NettyHttpResponse]
       matchingResponce.getStatus == repsonce.getStatus && matchingResponce.getContent.toString(CharsetUtil.UTF_8) == repsonce.getContent.toString(CharsetUtil.UTF_8)
     }
  }
}
