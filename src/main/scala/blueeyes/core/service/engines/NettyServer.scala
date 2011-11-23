package blueeyes.core.service.engines

import org.jboss.netty.channel.group.{ChannelGroup, DefaultChannelGroup}
import net.lag.logging.Logger
import org.jboss.netty.bootstrap.Bootstrap
import org.jboss.netty.channel.Channel

class NettyServer(provider: NettyServerProvider){
  private val startStopLock = new java.util.concurrent.locks.ReentrantReadWriteLock
  private val channelGroup = new DefaultChannelGroup()

  private var server: Option[Bootstrap]  = None

  def start{
    startStopLock.writeLock.lock()
    try {
      val (bootstrap, channel) = provider.startEngine(channelGroup)

      channelGroup.add(channel)
      server = Some(bootstrap)

      provider.log.info("%s netty engine is started using port: %d".format(provider.engineType, provider.enginePort))
    }
    catch {
      case e: Throwable => {
        provider.log.error(e, "Error while servers start: %s", e.getMessage)
        stop
        throw e
      }
    }
    finally{
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

trait NettyServerProvider{
  def engineType: String

  def enginePort: Int

  def log: Logger

  def startEngine(channelGroup: ChannelGroup): (Bootstrap, Channel)
}