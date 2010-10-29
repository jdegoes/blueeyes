package blueeyes.core.service

import org.jboss.netty.handler.codec.http.{HttpResponseEncoder, HttpRequestDecoder}
import org.jboss.netty.channel.{Channels, ChannelPipeline, ChannelPipelineFactory}
import java.net.InetSocketAddress
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.util.internal.ExecutorUtil
import java.util.concurrent.{Executor, Executors}
import net.lag.logging.Logger

class HttpServerNetty(val servicesClasses: List[Class[_ <: HttpService[_]]]) {
  if (servicesClasses.isEmpty) error("No services are specified.")

  private val logger        = Logger.get

  @volatile
  private var server: Option[ServerBootstrap] = None
  @volatile
  private var serverExecutor: Option[Executor] = None

  def start(port: Int) = {

    val executor  = Executors.newCachedThreadPool()
    val bootstrap = new ServerBootstrap(new NioServerSocketChannelFactory(executor, executor))

    val services = servicesClasses.map(_.newInstance())

    bootstrap.setPipelineFactory(new HttpServerPipelineFactory(services))

    bootstrap.bind(new InetSocketAddress(port))

    logger.info("Services are started: " + format(services))

    server          = Some(bootstrap)
    serverExecutor  = Some(executor)
  }

  def stop = {
    serverExecutor.foreach(ExecutorUtil.terminate(_))
    server.foreach(_.releaseExternalResources)

    val services = server.map(bootstrap => bootstrap.getPipelineFactory().asInstanceOf[HttpServerPipelineFactory].services).getOrElse(Nil)
    logger.info("Services are stopped: " + format(services))
  }

  private def format(services: List[HttpService[_]]) = services.map(service => "[Name=%s; Version=%d; Class=%s]".format(service.name, service.version, service.getClass)).mkString(" ")
}

class HttpServerPipelineFactory(val services: List[HttpService[_]]) extends ChannelPipelineFactory {
  def getPipeline(): ChannelPipeline = {
    val pipeline = Channels.pipeline()

    pipeline.addLast("decoder", new HttpRequestDecoder())
    pipeline.addLast("encoder", new HttpResponseEncoder())

    pipeline.addLast("handler", new NettyRequestHandler(services))

    pipeline
  }
}
