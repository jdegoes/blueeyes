package blueeyes.core.service
package engines.netty

import blueeyes.core.data._

import akka.dispatch.ExecutionContext

import org.jboss.netty.channel.{Channels, ChannelPipeline, ChannelPipelineFactory}
import org.jboss.netty.channel.group.ChannelGroup
import org.jboss.netty.handler.codec.http.HttpResponseEncoder
import org.jboss.netty.handler.codec.http.HttpContentCompressor
import org.jboss.netty.handler.stream.ChunkedWriteHandler
import com.weiglewilczek.slf4s.Logging

import HttpServerConfig._

private[engines] class HttpNettyServerProvider(conf: HttpServerConfig, service: AsyncHttpService[ByteChunk, ByteChunk], executionContext: ExecutionContext) extends AbstractNettyServerProvider {
  def pipelineFactory(channelGroup: ChannelGroup) = {
    new HttpPipelineFactory("http", conf.host, conf.port, conf.chunkSize, conf.compressionLevel, service, channelGroup, executionContext)
  }

  def engineType = "http"

  def enginePort = conf.port

  def config = conf.config
}

private[engines] class HttpPipelineFactory(protocol: String, host: String, port: Int, chunkSize: Int, compression: Option[CompressionLevel],
                                           service: AsyncHttpService[ByteChunk, ByteChunk], channelGroup: ChannelGroup,
                                           executionContext: ExecutionContext) extends ChannelPipelineFactory with Logging {
  def getPipeline: ChannelPipeline = {
    val pipeline = Channels.pipeline()

    pipeline.addLast("decoder",         new FullURIHttpRequestDecoder(protocol, host, port, chunkSize))
    pipeline.addLast("encoder",         new HttpResponseEncoder())
    pipeline.addLast("chunkedWriter",   new ChunkedWriteHandler())
    pipeline.addLast("aggregator",      new HttpNettyChunkedRequestHandler(chunkSize)(executionContext))
    pipeline.addLast("channelsTracker", new ChannelsTrackerUpstreamHandler(channelGroup))
    pipeline.addLast("handler",         new HttpServiceUpstreamHandler(service, executionContext))
    compression foreach { c => pipeline.addFirst("compression", new HttpContentCompressor(c)) }
    
    pipeline
  }
}
