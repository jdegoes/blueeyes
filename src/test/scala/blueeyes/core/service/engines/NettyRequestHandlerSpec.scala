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
import blueeyes.core.data.Bijections
import blueeyes.core.http.MimeTypes._
import blueeyes.core.service._
import blueeyes.core.http.{HttpVersions, HttpResponse, HttpStatus, HttpStatusCodes}
import java.net.InetSocketAddress

class NettyRequestHandlerSpec extends Specification with MockitoSugar with NettyConvertersions{
  private val handler       = mock[HttpRequestHandler[String]]
  private val context       = mock[ChannelHandlerContext]
  private val channel       = mock[Channel]
  private val channelFuture = mock[ChannelFuture]

  private implicit val bijection  = NettyBijections.ChannelBufferToString

  private val response      = HttpResponse[String](HttpStatus(HttpStatusCodes.OK), Map("retry-after" -> "1"), Some("12"), HttpVersions.`HTTP/1.1`)
  private val nettyHandler  = new NettyRequestHandler(handler)
  private val remoteAddress = new InetSocketAddress("127.0.0.0", 8080)

  "write OK responce service when path is match" in {
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

    assert(true)
  }

  "write Not Found responce service when path is not match" in {
    val event        = mock[MessageEvent]
    val nettyRequest = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "/foo/1/adCode.html")
    val request      = fromNettyRequest(nettyRequest, remoteAddress)

    when(event.getMessage()).thenReturn(nettyRequest, nettyRequest)
    when(handler.isDefinedAt(request)).thenReturn(false)
    when(event.getChannel()).thenReturn(channel, channel)
    when(channel.write( Matchers.argThat(new RequestMatcher(toNettyResponse(HttpResponse[String](HttpStatus(HttpStatusCodes.NotFound))))))).thenReturn(channelFuture, channelFuture)

    nettyHandler.messageReceived(context, event)

    Mockito.verify(channelFuture, times(1)).addListener(ChannelFutureListener.CLOSE)

    assert(true)
  }

  class RequestMatcher(matchingResponce: NettyHttpResponse) extends ArgumentMatcher[NettyHttpResponse] {
     def matches(arg: Object ): Boolean = {
       val repsonce = arg.asInstanceOf[NettyHttpResponse]
       matchingResponce.getStatus == repsonce.getStatus && matchingResponce.getContent.toString(CharsetUtil.UTF_8) == repsonce.getContent.toString(CharsetUtil.UTF_8)
     }
  }
}
