package blueeyes.core.service.engines

import blueeyes.core.data.ByteChunk
import org.jboss.netty.handler.codec.http.websocketx._
import org.jboss.netty.channel._
import blueeyes.core.http.HttpResponse
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.util.CharsetUtil

class HttpNettyWebSocketRequestHandler(response: HttpResponse[ByteChunk], handshaker: WebSocketServerHandshaker) extends SimpleChannelUpstreamHandler{
  override def messageReceived(ctx: ChannelHandlerContext, event: MessageEvent) = event.getMessage match{
    case frame: CloseWebSocketFrame  => handshaker.executeClosingHandshake(ctx, frame)
    case frame: PingWebSocketFrame   => ctx.getChannel.write(new PongWebSocketFrame(frame.isFinalFragment(), frame.getRsv(), frame.getBinaryData()))
    case frame: TextWebSocketFrame   =>
      ctx.getChannel.write(new TextWebSocketFrame(false, frame.getRsv(), frame.getBinaryData()))
      ctx.getChannel.write(new ContinuationWebSocketFrame(false, frame.getRsv(), ChannelBuffers.copiedBuffer("onen\n", CharsetUtil.UTF_8)))
      ctx.getChannel.write(new ContinuationWebSocketFrame(true, frame.getRsv(), ChannelBuffers.copiedBuffer("end\n", CharsetUtil.UTF_8)))
    case frame: BinaryWebSocketFrame => ctx.getChannel.write(new BinaryWebSocketFrame(frame.isFinalFragment(), frame.getRsv(), frame.getBinaryData()))
    case frame: ContinuationWebSocketFrame => ctx.getChannel.write(new ContinuationWebSocketFrame(frame.isFinalFragment(), frame.getRsv(), frame.getBinaryData()))
    case frame: PongWebSocketFrame  =>
    case _ => throw new UnsupportedOperationException("%s frame types not supported".format(event.getMessage.getClass().getName()))
  }
}