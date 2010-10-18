package blueeyes.core.service.server

import org.specs.Specification
import blueeyes.util.Future
import org.jboss.netty.handler.codec.http.{HttpMethod => NettyHttpMethod, HttpVersion => NettyHttpVersion, HttpResponse => NettyHttpResponse}
import org.scalatest.mock.MockitoSugar
import blueeyes.util.Future
import blueeyes.core.service.RestPathPatternImplicits._
import org.jboss.netty.handler.codec.http.DefaultHttpRequest
import blueeyes.core.service._
import org.jboss.netty.channel._
import org.mockito.Mockito.{when, times}
import org.jboss.netty.util.CharsetUtil
import org.mockito.{Matchers, Mockito, ArgumentMatcher}

class NettyRequestHandlerSpec extends Specification with MockitoSugar {
  private val handler       = mock[(Map[Symbol, String], HttpRequest[Int]) => Future[HttpResponse[Int]]]
  private val context       = mock[ChannelHandlerContext]
  private val channel       = mock[Channel]
  private val channelFuture = mock[ChannelFuture]

  private val response     = HttpResponse[Int](HttpStatus(HttpStatusCodes.OK), Map("retry-after" -> "1"), Some(12), HttpVersions.Http_1_0)
  private val nettyHandler = new NettyRequestHandler[Int](new TestService())(_.toInt, _.toString)

  "write OK responce service when path is match" in {
    val event  = mock[MessageEvent]
    val nettyRequest = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "bar/1/adCode.html")
    val future = new Future[HttpResponse[Int]]()
    future.deliver(response)

    when(event.getMessage()).thenReturn(nettyRequest, nettyRequest)
    when(handler.apply(Map('adId -> "1"), Converters.fromNetty(nettyRequest)(_.toInt))).thenReturn(future, future)
    when(event.getChannel()).thenReturn(channel, channel)
    when(channel.write( Matchers.argThat(new RequestMatcher(Converters.toNetty(response)(_.toString))))).thenReturn(channelFuture, channelFuture)

    nettyHandler.messageReceived(context, event)

    Mockito.verify(channelFuture, times(1)).addListener(ChannelFutureListener.CLOSE)

    assert(true)
  }

  "write Not Found responce service when path is not match" in {
    val event        = mock[MessageEvent]
    val nettyRequest = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "foo/1/adCode.html")

    when(event.getMessage()).thenReturn(nettyRequest, nettyRequest)
    when(event.getChannel()).thenReturn(channel, channel)
    when(channel.write( Matchers.argThat(new RequestMatcher(Converters.toNetty(HttpResponse(HttpStatus(HttpStatusCodes.NotFound)))(_.toString))))).thenReturn(channelFuture, channelFuture)

    nettyHandler.messageReceived(context, event)

    Mockito.verify(channelFuture, times(1)).addListener(ChannelFutureListener.CLOSE)

    assert(true)    
  }

  class TestService extends RestHierarchyBuilder[Int]{
    path("bar" / 'adId / "adCode.html"){get(handler)}
  }

  class RequestMatcher(matchingResponce: NettyHttpResponse) extends ArgumentMatcher[NettyHttpResponse] {
     def matches(arg: Object ): Boolean = {
       val repsonce = arg.asInstanceOf[NettyHttpResponse]
       matchingResponce.getStatus == repsonce.getStatus && matchingResponce.getContent.toString(CharsetUtil.UTF_8) == repsonce.getContent.toString(CharsetUtil.UTF_8)
     }
  }
}