package blueeyes.core.service

import org.jboss.netty.handler.codec.http.{HttpResponseEncoder, HttpRequestDecoder}
import org.jboss.netty.channel.{Channels, ChannelPipeline, ChannelPipelineFactory}
import java.net.InetSocketAddress
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.util.internal.ExecutorUtil
import java.util.concurrent.{Executor, Executors}

class HttpServerNetty(val service: Class[_ <: HttpService[_]]) {
  @volatile
  private var server: Option[ServerBootstrap] = None
  @volatile
  private var serverExecutor: Option[Executor] = None

  def start(port: Int) = {
    val executor  = Executors.newCachedThreadPool()
    val bootstrap = new ServerBootstrap(new NioServerSocketChannelFactory(executor, executor))

    bootstrap.setPipelineFactory(new HttpServerPipelineFactory(service.newInstance()))

    bootstrap.bind(new InetSocketAddress(port))

    server = Some(bootstrap)
    serverExecutor = Some(executor)
  }

  def stop = {
    serverExecutor.foreach(ExecutorUtil.terminate(_))
    server.foreach(_.releaseExternalResources)
  }
}

class HttpServerPipelineFactory(hierarchy: RestHierarchy[_]) extends ChannelPipelineFactory {
  def getPipeline(): ChannelPipeline = {
    val pipeline = Channels.pipeline()

    pipeline.addLast("decoder", new HttpRequestDecoder())
    pipeline.addLast("encoder", new HttpResponseEncoder())

    pipeline.addLast("handler", new NettyRequestHandler(hierarchy))

    pipeline
  }
}
