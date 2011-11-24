package blueeyes.core.service.engines

import org.jboss.netty.channel.group.ChannelGroup
import blueeyes.core.service.HttpServer

class WebSocketServerProvider(server: HttpServer) extends NettyServerProvider{
  def startEngine(channelGroup: ChannelGroup) = {

  }

  def engineType = "ws"

  def enginePort = server.config.getInt("wsPort", 8888)

  def log = server.log
}