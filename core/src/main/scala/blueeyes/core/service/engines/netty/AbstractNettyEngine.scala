package blueeyes.core.service.engines.netty

import blueeyes.core.service.{HttpServer, HttpServerEngine}
import com.weiglewilczek.slf4s.Logging
import akka.dispatch.Future

trait AbstractNettyEngine extends HttpServerEngine with HttpServer with Logging {
  private val startStopLock = new java.util.concurrent.locks.ReentrantReadWriteLock
  private var servers: List[NettyServer]  = Nil

  override def start: Future[Unit] = {
    super.start.map { _ =>
      startStopLock.writeLock.lock()
      try {
        servers = nettyServers
        servers.foreach(_.start)
      } finally {
        startStopLock.writeLock.unlock()
      }
    }
  }

  override def stop: Future[Unit] = {
    Future { 
      startStopLock.writeLock.lock()
      try {
        servers.foreach(_.stop)
        servers = Nil
        logger.info("Netty engine has stopped.")
      } finally{
        startStopLock.writeLock.unlock()
      }
    }.flatMap(_ => super.stop.map(_ => ()))
  }

  protected def nettyServers: List[NettyServer]
}
