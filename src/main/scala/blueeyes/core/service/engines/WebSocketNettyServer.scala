package blueeyes.core.service.engines

import org.jboss.netty.channel.group.ChannelGroup
import org.jboss.netty.channel.{Channels, ChannelPipeline, ChannelPipelineFactory}
import net.lag.logging.Logger
import blueeyes.core.service._
import blueeyes.core.data._
import org.jboss.netty.handler.codec.http.{HttpChunkAggregator, HttpResponseEncoder}

private[engines] class WebSocketServerProvider(server: HttpServer) extends AbstractNettyServerProvider{
  def pipelineFactory(channelGroup: ChannelGroup) = new WebSocketPipelineFactory(server.host, server.port, server.chunkSize, server, channelGroup)

  def engineType = "ws"

  def enginePort = server.config.getInt("wsPort", 8889)
}

private[engines] class WebSocketPipelineFactory(host: String, port: Int, chunkSize: Int, requestHandler: AsyncCustomHttpService[ByteChunk], channelGroup: ChannelGroup) extends ChannelPipelineFactory{
  def getPipeline: ChannelPipeline = {
    val pipeline = Channels.pipeline()

    pipeline.addLast("decoder", new FullURIHttpRequestDecoder("ws", host, port, chunkSize))
    pipeline.addLast("aggregator", new HttpChunkAggregator(65536))
    pipeline.addLast("encoder", new HttpResponseEncoder)
    pipeline.addLast("handler", new WebSocketServerHandler(requestHandler, Logger.get))

    pipeline
  }
}

class WebSocketNettyServer(server: HttpServer) extends NettyServer(new WebSocketServerProvider(server))