package blueeyes.core.service.engines

import org.jboss.netty.handler.stream.ChunkedWriteHandler
import net.lag.logging.Logger
import org.jboss.netty.channel._
import group.ChannelGroup

import blueeyes.core.data.ByteChunk
import blueeyes.core.service._
import security.BlueEyesKeyStoreFactory
import org.jboss.netty.handler.ssl.SslHandler
import org.jboss.netty.handler.codec.http.HttpResponseEncoder
import net.lag.configgy.ConfigMap

private[engines] class HttpPipelineFactory(val protocol: String, host: String, port: Int, chunkSize: Int,
                                           requestHandler: AsyncCustomHttpService[ByteChunk], channelGroup: ChannelGroup) extends ChannelPipelineFactory {
  def getPipeline: ChannelPipeline = {
    val pipeline = Channels.pipeline()

    pipeline.addLast("decoder",         new FullURIHttpRequestDecoder(protocol, host, port, chunkSize))
    pipeline.addLast("encoder",         new HttpResponseEncoder())
    pipeline.addLast("chunkedWriter",   new ChunkedWriteHandler())
    pipeline.addLast("aggregator",      new HttpNettyChunkedRequestHandler(chunkSize))
    pipeline.addLast("channelsTracker", new ChannelsTrackerHandler(channelGroup))
    pipeline.addLast("handler",         new HttpNettyRequestHandler(requestHandler, Logger.get))

    pipeline
  }
}

private[engines] class HttpsPipelineFactory(protocol: String, host: String, port: Int, chunkSize: Int,
                                            requestHandler: AsyncCustomHttpService[ByteChunk], channelGroup: ChannelGroup, config: ConfigMap)
  extends HttpPipelineFactory(protocol: String, host, port, chunkSize, requestHandler, channelGroup) {

    val keyStore = BlueEyesKeyStoreFactory(config)

  override def getPipeline: ChannelPipeline = {
    val pipeline = super.getPipeline()

    val engine = SslContextFactory(keyStore, BlueEyesKeyStoreFactory.password).createSSLEngine()

    engine.setUseClientMode(false);

    pipeline.addFirst("ssl", new SslHandler(engine))

    pipeline
  }
}