package blueeyes.core.service.engines

import blueeyes.core.service._
import blueeyes.core.data._
import net.lag.logging.Logger
import org.jboss.netty.channel.{MessageEvent, ChannelHandlerContext, SimpleChannelUpstreamHandler}

private[engines] class WebSocketServerHandler(requestHandler: AsyncCustomHttpService[ByteChunk], log: Logger) extends SimpleChannelUpstreamHandler{
  override def messageReceived(ctx: ChannelHandlerContext, event: MessageEvent) {

  }
}