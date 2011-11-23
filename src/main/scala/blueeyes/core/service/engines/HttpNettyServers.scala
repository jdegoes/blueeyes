package blueeyes.core.service.engines

import blueeyes.core.service.HttpServer
import org.jboss.netty.channel.group.ChannelGroup
import java.util.concurrent.Executors
import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import org.jboss.netty.channel.{ChannelStateEvent, ChannelHandlerContext, SimpleChannelUpstreamHandler, ChannelPipelineFactory}

trait AbstractHttpServerProvider extends NettyServerProvider{
  def startEngine(channelGroup: ChannelGroup) = {
    val executor  = Executors.newCachedThreadPool()
    val bootstrap = new ServerBootstrap(new NioServerSocketChannelFactory(executor, executor))
    bootstrap.setParentHandler(new SetBacklogHandler(server.config.getInt("backlog", 10000)))
    bootstrap.setPipelineFactory(pipelineFactory(channelGroup))
    val channel = bootstrap.bind(InetInterfaceLookup.socketAddres(server.config, enginePort))

    (bootstrap, channel)
  }

  def pipelineFactory(channelGroup: ChannelGroup): ChannelPipelineFactory

  def server: HttpServer

  def log = server.log
}

private[engines] class SetBacklogHandler(backlog: Int) extends SimpleChannelUpstreamHandler{
  override def channelOpen(ctx: ChannelHandlerContext, e: ChannelStateEvent) {
    e.getChannel.getConfig.setOption("backlog", backlog)
    super.channelOpen(ctx, e)
  }
}

class HttpNettyServerProvider(val server: HttpServer) extends AbstractHttpServerProvider{
  def pipelineFactory(channelGroup: ChannelGroup) = new HttpPipelineFactory("http", server.host, server.port, server.chunkSize, server, channelGroup)

  def engineType = "http"

  def enginePort = server.port
}

class HttpsNettyServerProvider(val server: HttpServer) extends AbstractHttpServerProvider{
  def pipelineFactory(channelGroup: ChannelGroup) = new HttpsPipelineFactory("https", server.host, server.sslPort, server.chunkSize, server, channelGroup, server.config)

  def engineType = "https"

  def enginePort = server.sslPort
}

class HttpNettyServer(server: HttpServer) extends NettyServer(new HttpNettyServerProvider(server))

class HttpsNettyServer(server: HttpServer) extends NettyServer(new HttpsNettyServerProvider(server))