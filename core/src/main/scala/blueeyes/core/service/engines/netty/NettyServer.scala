package blueeyes.core.service.engines.netty

import org.jboss.netty.channel.group.{ChannelGroup, DefaultChannelGroup}
import com.weiglewilczek.slf4s.Logger
import java.util.concurrent.Executors
import org.jboss.netty.bootstrap.{ServerBootstrap, Bootstrap}
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import org.jboss.netty.channel._
import blueeyes.core.service.engines.InetInterfaceLookup
import org.streum.configrity.Configuration


class NettyServer(provider: NettyServerProvider) {
  private val startStopLock = new java.util.concurrent.locks.ReentrantReadWriteLock
  private val channelGroup = new DefaultChannelGroup()

  private var server: Option[Bootstrap]  = None

  def start {
    startStopLock.writeLock.lock()
    try {
      val (bootstrap, channel) = provider.startEngine(channelGroup)

      channelGroup.add(channel)
      server = Some(bootstrap)

      provider.log.info("%s netty engine is started using port: %d".format(provider.engineType, provider.enginePort))
    } catch {
      case e: Throwable => {
        provider.log.error("Error on server startup", e)
        stop
        throw e
      }
    } finally{
      startStopLock.writeLock.unlock()
    }
  }

  def stop {
    startStopLock.writeLock.lock()
    channelGroup.close().awaitUninterruptibly()
    try {
      server.foreach(_.releaseExternalResources())
      server = None
    }
    finally{
      startStopLock.writeLock.unlock()
    }

    provider.log.info("%s Netty engine is stopped.".format(provider.engineType))
  }
}

trait NettyServerProvider {
  def engineType: String

  def enginePort: Int

  def log: Logger

  def startEngine(channelGroup: ChannelGroup): (Bootstrap, Channel)
}

trait AbstractNettyServerProvider extends NettyServerProvider {
  def startEngine(channelGroup: ChannelGroup) = {
    val executor  = Executors.newCachedThreadPool()
    val bootstrap = new ServerBootstrap(new NioServerSocketChannelFactory(executor, executor))
    bootstrap.setParentHandler(new SetBacklogHandler(config[Int]("backlog", 10000)))
    bootstrap.setPipelineFactory(pipelineFactory(channelGroup))
    val channel = bootstrap.bind(InetInterfaceLookup.socketAddres(config, enginePort))

    (bootstrap, channel)
  }

  def pipelineFactory(channelGroup: ChannelGroup): ChannelPipelineFactory

  def config: Configuration 

  private[engines] class SetBacklogHandler(backlog: Int) extends SimpleChannelUpstreamHandler {
    override def channelOpen(ctx: ChannelHandlerContext, e: ChannelStateEvent) {
      e.getChannel.getConfig.setOption("backlog", backlog)
      super.channelOpen(ctx, e)
    }
  }
}
