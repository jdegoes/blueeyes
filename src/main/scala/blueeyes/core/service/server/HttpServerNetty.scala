package blueeyes.core.service.server

import org.jboss.netty.handler.codec.http.{HttpResponseEncoder, HttpRequestDecoder}
import org.jboss.netty.channel.{Channels, ChannelPipeline, ChannelPipelineFactory}
import java.net.InetSocketAddress
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.util.internal.ExecutorUtil
import java.util.concurrent.{Executor, Executors}
import blueeyes.core.service.{RestHierarchy}
import blueeyes.core.data.{DataTranscoder, Bijection}

trait HttpServerNetty[T] {
  @volatile
  private var server: Option[ServerBootstrap] = None
  @volatile
  private var serverExecutor: Option[Executor] = None

  def start(port: Int) = {
    val executor  = Executors.newCachedThreadPool()
    val bootstrap = new ServerBootstrap(new NioServerSocketChannelFactory(executor, executor))

    bootstrap.setPipelineFactory(new HttpServerPipelineFactory(hierarchies))

    bootstrap.bind(new InetSocketAddress(port))

    server = Some(bootstrap)
    serverExecutor = Some(executor)
  }

  def stop = {
    serverExecutor.foreach(ExecutorUtil.terminate(_))
    server.foreach(_.releaseExternalResources)
  }

  def hierarchies: List[(RestHierarchy[T], DataTranscoder[String, T])]
}

class HttpServerPipelineFactory[T](hierarchies: List[(RestHierarchy[T], DataTranscoder[String, T])]) extends ChannelPipelineFactory {
  def getPipeline(): ChannelPipeline = {
    val pipeline = Channels.pipeline()

    pipeline.addLast("decoder", new HttpRequestDecoder())
    pipeline.addLast("encoder", new HttpResponseEncoder())

    pipeline.addLast("handler", new NettyRequestHandler(hierarchies))

    pipeline
  }
}
