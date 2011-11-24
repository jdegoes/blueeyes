package blueeyes.core.service.engines

import org.jboss.netty.channel.group.ChannelGroup
import blueeyes.core.service._
import blueeyes.core.data._
import net.lag.configgy.ConfigMap
import security.BlueEyesKeyStoreFactory
import security.BlueEyesKeyStoreFactory._
import org.jboss.netty.channel.ChannelPipeline
import org.jboss.netty.handler.ssl.SslHandler

private[engines] class HttpsNettyServerProvider(val server: HttpServer) extends AbstractNettyServerProvider{
  def pipelineFactory(channelGroup: ChannelGroup) = new HttpsPipelineFactory("https", server.host, server.sslPort, server.chunkSize, server, channelGroup, server.config)

  def engineType = "https"

  def enginePort = server.sslPort
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

class HttpsNettyServer(server: HttpServer) extends NettyServer(new HttpsNettyServerProvider(server))