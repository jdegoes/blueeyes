package blueeyes.core.service.server

import org.jboss.netty.handler.codec.http.{HttpResponseEncoder, HttpRequestDecoder}
import org.jboss.netty.channel.{Channels, ChannelPipeline, ChannelPipelineFactory}
import java.net.InetSocketAddress
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import org.jboss.netty.bootstrap.ServerBootstrap
import blueeyes.core.data.{Bijection}
import blueeyes.core.service.{RestHierarchyBuilder}
import org.jboss.netty.util.internal.ExecutorUtil
import java.util.concurrent.{ExecutorService, Executor, Executors}

trait HttpServerNetty[T] {self: RestHierarchyBuilder[T] =>
  @volatile
  private var server: Option[ServerBootstrap] = None
  @volatile
  private var serverExecutor: Option[Executor] = None

  def start(port: Int)(implicit bijection: Bijection[String, T]) = {
    val executor  = Executors.newCachedThreadPool()
    val bootstrap = new ServerBootstrap(new NioServerSocketChannelFactory(executor, executor))

    bootstrap.setPipelineFactory(new HttpServerPipelineFactory(this))

    bootstrap.bind(new InetSocketAddress(port))

    server = Some(bootstrap)
    serverExecutor = Some(executor)
  }

  def stop = {
    serverExecutor.foreach(ExecutorUtil.terminate(_))
    server.foreach(_.releaseExternalResources)
  }
}

class HttpServerPipelineFactory[T](builder: RestHierarchyBuilder[T])(implicit bijection: Bijection[String, T]) extends ChannelPipelineFactory {
  def getPipeline(): ChannelPipeline = {
    val pipeline = Channels.pipeline()

    pipeline.addLast("decoder", new HttpRequestDecoder())
    pipeline.addLast("encoder", new HttpResponseEncoder())

    pipeline.addLast("handler", new NettyRequestHandler(builder))

    pipeline
  }
}
