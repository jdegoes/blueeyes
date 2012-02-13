package blueeyes.core.service.engines.netty

import blueeyes.core.service._
import akka.dispatch.Future

trait NettyEngine extends AbstractNettyEngine{ self =>
  protected def nettyServers = {
    val httpEngine = new HttpNettyServer(self)
    httpEngine :: (if (sslEnable) List(new HttpsNettyServer(self)) else Nil)
  }
}

trait HttpNettyEngine extends AbstractNettyEngine{ self =>
  def nettyServers = List(new HttpNettyServer(self))
}

trait HttpsNettyEngine extends AbstractNettyEngine{ self =>
  def nettyServers = List(new HttpsNettyServer(self))
}