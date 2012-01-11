package blueeyes.core.service.engines

import org.jboss.netty.channel.group.ChannelGroup
import blueeyes.core.service._
import blueeyes.core.data._
import org.jboss.netty.channel.{Channels, ChannelPipeline, ChannelPipelineFactory}
import org.jboss.netty.handler.codec.http.HttpResponseEncoder
import org.jboss.netty.handler.stream.ChunkedWriteHandler
import com.weiglewilczek.slf4s.Logging

private[engines] class HttpNettyServerProvider(server: HttpServer) extends AbstractNettyServerProvider{
  def pipelineFactory(channelGroup: ChannelGroup) = new HttpPipelineFactory("http", server.host, server.port, server.chunkSize, server, channelGroup)

  def engineType = "http"

  def enginePort = server.port

  def config = server.config

  def log = server.log
}

private[engines] class HttpPipelineFactory(protocol: String, host: String, port: Int, chunkSize: Int,
                                           requestHandler: AsyncCustomHttpService[ByteChunk], channelGroup: ChannelGroup) extends ChannelPipelineFactory with Logging {
  def getPipeline: ChannelPipeline = {
    val pipeline = Channels.pipeline()

    pipeline.addLast("decoder",         new FullURIHttpRequestDecoder(protocol, host, port, chunkSize))
    pipeline.addLast("encoder",         new HttpResponseEncoder())
    pipeline.addLast("chunkedWriter",   new ChunkedWriteHandler())
    pipeline.addLast("aggregator",      new HttpNettyChunkedRequestHandler(chunkSize))
    pipeline.addLast("channelsTracker", new ChannelsTrackerHandler(channelGroup))
    pipeline.addLast("handler",         new HttpNettyRequestHandler(requestHandler, logger))

    pipeline
  }
}

class HttpNettyServer(server: HttpServer) extends NettyServer(new HttpNettyServerProvider(server))
