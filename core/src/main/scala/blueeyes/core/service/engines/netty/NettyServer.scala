package blueeyes.core.service.engines.netty

import blueeyes.core.service.engines.InetInterfaceLookup

import org.jboss.netty.channel.group.{ChannelGroup, DefaultChannelGroup}
import org.jboss.netty.bootstrap.{ServerBootstrap, Bootstrap}
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import org.jboss.netty.channel._

import java.util.concurrent.Executors

import com.weiglewilczek.slf4s.Logger
import com.weiglewilczek.slf4s.Logging

import org.streum.configrity.Configuration


class NettyServer(provider: NettyServerProvider) extends Logging {
  private val startStopLock = new java.util.concurrent.locks.ReentrantReadWriteLock
  private val channelGroup = new DefaultChannelGroup()

  private var server: Option[Bootstrap]  = None

  def start {
    startStopLock.writeLock.lock()
    try {
      val (bootstrap, channel) = provider.startEngine(channelGroup)

      channelGroup.add(channel)
      server = Some(bootstrap)

      logger.info("%s netty engine is started using port: %d".format(provider.engineType, provider.enginePort))
    } catch {
      case e: Throwable => {
        logger.error("Error on server startup", e)
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

    logger.info("%s Netty engine is stopped.".format(provider.engineType))
  }
}

trait NettyServerProvider {
  def engineType: String

  def enginePort: Int

  def startEngine(channelGroup: ChannelGroup): (Bootstrap, Channel)
}

trait AbstractNettyServerProvider extends NettyServerProvider {
  def config: Configuration 

  def startEngine(channelGroup: ChannelGroup) = {
    val executor  = Executors.newCachedThreadPool()
    val bootstrap = new ServerBootstrap(new NioServerSocketChannelFactory(executor, executor))
    bootstrap.setParentHandler(new SetBacklogHandler(config[Int]("backlog", 10000)))
    bootstrap.setPipelineFactory(pipelineFactory(channelGroup))
    val channel = bootstrap.bind(InetInterfaceLookup.socketAddres(config, enginePort))

    (bootstrap, channel)
  }

  def pipelineFactory(channelGroup: ChannelGroup): ChannelPipelineFactory

  private[engines] class SetBacklogHandler(backlog: Int) extends SimpleChannelUpstreamHandler {
    override def channelOpen(ctx: ChannelHandlerContext, e: ChannelStateEvent) {
      e.getChannel.getConfig.setOption("backlog", backlog)
      super.channelOpen(ctx, e)
    }
  }
}
