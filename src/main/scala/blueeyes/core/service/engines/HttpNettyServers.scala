package blueeyes.core.service.engines

import blueeyes.core.service.HttpServer
import org.jboss.netty.channel.group.ChannelGroup

class HttpNettyServerProvider(val server: HttpServer) extends AbstractNettyServerProvider{
  def pipelineFactory(channelGroup: ChannelGroup) = new HttpPipelineFactory("http", server.host, server.port, server.chunkSize, server, channelGroup)

  def engineType = "http"

  def enginePort = server.port
}

class HttpsNettyServerProvider(val server: HttpServer) extends AbstractNettyServerProvider{
  def pipelineFactory(channelGroup: ChannelGroup) = new HttpsPipelineFactory("https", server.host, server.sslPort, server.chunkSize, server, channelGroup, server.config)

  def engineType = "https"

  def enginePort = server.sslPort
}

class HttpNettyServer(server: HttpServer) extends NettyServer(new HttpNettyServerProvider(server))

class HttpsNettyServer(server: HttpServer) extends NettyServer(new HttpsNettyServerProvider(server))