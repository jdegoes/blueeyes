package blueeyes.core.service.engines

import org.jboss.netty.channel.group.ChannelGroup
import blueeyes.core.service.HttpServer

class WebSocketServerProvider(server: HttpServer) extends AbstractNettyServerProvider{
  def pipelineFactory(channelGroup: ChannelGroup) = null

  def engineType = "ws"

  def enginePort = server.config.getInt("wsPort", 8889)
}

class WebSocketNettyServer(server: HttpServer) extends NettyServer(new WebSocketServerProvider(server))