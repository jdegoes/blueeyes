package blueeyes.core.service
package engines.netty

import blueeyes.core.data._

import akka.dispatch.ExecutionContext

import org.jboss.netty.channel.{Channels, ChannelPipeline, ChannelPipelineFactory}
import org.jboss.netty.channel.group.ChannelGroup
import org.jboss.netty.handler.codec.http.HttpResponseEncoder
import org.jboss.netty.handler.stream.ChunkedWriteHandler
import com.weiglewilczek.slf4s.Logging

private[engines] class HttpNettyServerProvider(server: HttpServerConfig, service: AsyncHttpService[ByteChunk], executionContext: ExecutionContext) extends AbstractNettyServerProvider {
  def pipelineFactory(channelGroup: ChannelGroup) = {
    new HttpPipelineFactory("http", server.host, server.port, server.chunkSize, service, channelGroup, executionContext)
  }

  def engineType = "http"

  def enginePort = server.port

  def config = server.config

  def log = server.log
}

private[engines] class HttpPipelineFactory(protocol: String, host: String, port: Int, chunkSize: Int,
                                           service: AsyncHttpService[ByteChunk], channelGroup: ChannelGroup,
                                           executionContext: ExecutionContext) extends ChannelPipelineFactory with Logging {
  def getPipeline: ChannelPipeline = {
    val pipeline = Channels.pipeline()

    pipeline.addLast("decoder",         new FullURIHttpRequestDecoder(protocol, host, port, chunkSize))
    pipeline.addLast("encoder",         new HttpResponseEncoder())
    pipeline.addLast("chunkedWriter",   new ChunkedWriteHandler())
    pipeline.addLast("aggregator",      new HttpNettyChunkedRequestHandler(chunkSize)(executionContext))
    pipeline.addLast("channelsTracker", new ChannelsTrackerUpstreamHandler(channelGroup))
    pipeline.addLast("handler",         new HttpServiceUpstreamHandler(service, executionContext))

    pipeline
  }
}
