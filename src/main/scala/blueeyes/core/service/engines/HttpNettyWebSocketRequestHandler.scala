package blueeyes.core.service.engines

import blueeyes.core.data.ByteChunk
import org.jboss.netty.handler.codec.http.websocketx.{TextWebSocketFrame, WebSocketServerHandshaker, WebSocketServerHandshakerFactory}
import blueeyes.core.service.engines.HttpNettyConverters._
import blueeyes.core.http.{HttpHeaders, HttpRequest}
import org.jboss.netty.channel._

class HttpNettyWebSocketRequestHandler extends  ChannelUpstreamHandler with ChannelDownstreamHandler with WebSocketUtils{

  def handleDownstream(ctx: ChannelHandlerContext, e: ChannelEvent) {
    e.getMessage match {
      case r: HttpRequest[ByteChunk] =>
        if (isWebSocketRequest(r)){
          val newRequest = request.copy(content = Some(ByteMemoryChunk(Array[Byte](), () => webSocketFrame)))
          Channels.fireMessageReceived(ctx, newRequest, e.getRemoteAddress)
        }
        else{
          super.messageReceived(ctx, e)
        }
    }
  }

  def handleUpstream(ctx: ChannelHandlerContext, e: ChannelEvent) {

  }
}