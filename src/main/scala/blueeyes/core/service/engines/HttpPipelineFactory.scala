package blueeyes.core.service.engines

import org.jboss.netty.handler.stream.ChunkedWriteHandler
import net.lag.logging.Logger
import org.jboss.netty.channel._
import group.ChannelGroup

import blueeyes.core.data.ByteChunk
import blueeyes.core.service._
import security.BlueEyesKeyStoreFactory
import org.jboss.netty.handler.ssl.SslHandler
import org.jboss.netty.handler.codec.http.{HttpRequestDecoder, HttpResponseEncoder}
import util.matching.Regex
import net.lag.configgy.ConfigMap

private[engines] class HttpPipelineFactory(val protocol: String, host: String, port: Int, chunkSize: Int,
                                           requestHandler: AsyncCustomHttpService[ByteChunk], channelGroup: ChannelGroup) extends ChannelPipelineFactory {
  def getPipeline: ChannelPipeline = {
    val pipeline = Channels.pipeline()

    pipeline.addLast("decoder",         new FullURIHttpRequestDecoder(protocol, host, port, chunkSize))
    pipeline.addLast("encoder",         new HttpResponseEncoder())
    pipeline.addLast("chunkedWriter",   new ChunkedWriteHandler())
    pipeline.addLast("aggregator",      new NettyChunkedRequestHandler(chunkSize))
    pipeline.addLast("channelsTracker", new ChannelsTracker(channelGroup))
    pipeline.addLast("handler",         new NettyRequestHandler(requestHandler, Logger.get))

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

private[engines] class ChannelsTracker(channelGroup: ChannelGroup) extends SimpleChannelUpstreamHandler {
  override def channelOpen(ctx: ChannelHandlerContext, e: ChannelStateEvent) {
    channelGroup.add(e.getChannel)
  }
}

private[engines] class FullURIHttpRequestDecoder(protocol: String, host: String, port: Int, chunkSize: Int) extends HttpRequestDecoder(4096, 8192, 2){
  private val baseUri = """%s://%s:%d""".format(protocol, host, port)
  private val fullUriRegexp = new Regex("""(https|http)://.+/(:\d+/)?.+""")
  override def createMessage(initialLine: Array[String]) = {
    val path = initialLine(1)
    if (!fullUriRegexp.pattern.matcher(path).matches) initialLine(1) = baseUri + (if (path.startsWith("/")) path else "/" + path)
    super.createMessage(initialLine)
  }
}