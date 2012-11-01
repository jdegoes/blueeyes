package blueeyes.core.service
package engines.netty

import blueeyes.core.data._
import engines.security._
import engines.security.BlueEyesKeyStoreFactory._

import akka.dispatch.ExecutionContext

import org.jboss.netty.channel.group.ChannelGroup
import org.jboss.netty.channel.ChannelPipeline
import org.jboss.netty.handler.ssl.SslHandler

import org.streum.configrity.Configuration

private[engines] class HttpsNettyServerProvider(server: HttpServerConfig, service: AsyncHttpService[ByteChunk], executionContext: ExecutionContext) extends AbstractNettyServerProvider {
  def pipelineFactory(channelGroup: ChannelGroup) = {
    new HttpsPipelineFactory("https", server.host, server.sslPort, server.chunkSize, service, channelGroup, server.config, executionContext)
  }

  def engineType = "https"

  def enginePort = server.sslPort

  def config = server.config

  def log = server.log
}

private[engines] class HttpsPipelineFactory(protocol: String, host: String, port: Int, chunkSize: Int,
                                            requestHandler: AsyncHttpService[ByteChunk], channelGroup: ChannelGroup, 
                                            config: Configuration, //TODO: Use of Configuration here is bogus
                                            executionContext: ExecutionContext)
    extends HttpPipelineFactory(protocol: String, host, port, chunkSize, requestHandler, channelGroup, executionContext) {

  val keyStore = BlueEyesKeyStoreFactory(config)

  override def getPipeline: ChannelPipeline = {
    val pipeline = super.getPipeline()

    val engine = SslContextFactory(keyStore, BlueEyesKeyStoreFactory.password).createSSLEngine()

    engine.setUseClientMode(false);

    pipeline.addFirst("ssl", new SslHandler(engine))

    pipeline
  }
}
