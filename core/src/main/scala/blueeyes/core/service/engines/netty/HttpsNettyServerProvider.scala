package blueeyes.core.service
package engines.netty

import blueeyes.core.data._
import engines.security._
import engines.security.BlueEyesKeyStoreFactory._

import akka.dispatch.ExecutionContext

import org.jboss.netty.channel.group.ChannelGroup
import org.jboss.netty.channel.ChannelPipeline
import org.jboss.netty.handler.ssl.SslHandler

import com.weiglewilczek.slf4s.Logger
import org.streum.configrity.Configuration

import HttpServerConfig._

private[engines] class HttpsNettyServerProvider(conf: HttpServerConfig, service: AsyncHttpService[ByteChunk, ByteChunk], executionContext: ExecutionContext) extends AbstractNettyServerProvider {
  def pipelineFactory(channelGroup: ChannelGroup) = {
    new HttpsPipelineFactory("https", conf.host, conf.sslPort, conf.chunkSize, conf.compressionLevel, service, channelGroup, conf.config, executionContext)
  }

  def engineType = "https"

  def enginePort = conf.sslPort

  def config = conf.config
}

private[engines] class HttpsPipelineFactory(protocol: String, host: String, port: Int, chunkSize: Int, compression: Option[CompressionLevel],
                                            requestHandler: AsyncHttpService[ByteChunk, ByteChunk], channelGroup: ChannelGroup, 
                                            config: Configuration, //TODO: Use of Configuration here is bogus
                                            executionContext: ExecutionContext)
    extends HttpPipelineFactory(protocol: String, host, port, chunkSize, compression, requestHandler, channelGroup, executionContext) {

  val keyStore = BlueEyesKeyStoreFactory(config)

  override def getPipeline: ChannelPipeline = {
    val pipeline = super.getPipeline()

    val engine = SslContextFactory(keyStore, BlueEyesKeyStoreFactory.password).createSSLEngine()

    engine.setUseClientMode(false);

    pipeline.addFirst("ssl", new SslHandler(engine))

    pipeline
  }
}
