package blueeyes.core.service.engines

import org.jboss.netty.channel.group.ChannelGroup
import blueeyes.core.service.HttpServer
import org.jboss.netty.channel.{Channels, ChannelPipeline, ChannelPipelineFactory}
import org.jboss.netty.handler.codec.http.HttpResponseEncoder
import org.jboss.netty.handler.stream.ChunkedWriteHandler
import net.lag.logging.Logger

private[engines] class WebSocketServerProvider(server: HttpServer) extends AbstractNettyServerProvider{
  def pipelineFactory(channelGroup: ChannelGroup) = null

  def engineType = "ws"

  def enginePort = server.config.getInt("wsPort", 8889)
}

private[engines] class WebSocketPipelineFactory(host: String, port: Int) extends ChannelPipelineFactory{
  def getPipeline: ChannelPipeline = {
    val pipeline = Channels.pipeline()

    pipeline.addLast("decoder",         new FullURIHttpRequestDecoder("ws", host, port, chunkSize))
    pipeline.addLast("encoder",         new HttpResponseEncoder())
    pipeline.addLast("chunkedWriter",   new ChunkedWriteHandler())
    pipeline.addLast("aggregator",      new HttpNettyChunkedRequestHandler(chunkSize))
    pipeline.addLast("channelsTracker", new ChannelsTrackerHandler(channelGroup))
    pipeline.addLast("handler",         new HttpNettyRequestHandler(requestHandler, Logger.get))

    pipeline
  }
}

class WebSocketNettyServer(server: HttpServer) extends NettyServer(new WebSocketServerProvider(server))