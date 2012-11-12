package blueeyes.core.service.engines.netty

import org.jboss.netty.channel.group.ChannelGroup
import org.jboss.netty.channel.{ChannelStateEvent, ChannelHandlerContext, SimpleChannelUpstreamHandler}

class ChannelsTrackerUpstreamHandler(channelGroup: ChannelGroup) extends SimpleChannelUpstreamHandler {
  override def channelOpen(ctx: ChannelHandlerContext, e: ChannelStateEvent) {
    channelGroup.add(e.getChannel)
  }
}
