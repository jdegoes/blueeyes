package blueeyes.core.service
package engines.netty

import blueeyes.core.data._

import com.weiglewilczek.slf4s.Logger
import akka.dispatch.ExecutionContext
import org.streum.configrity.Configuration

trait NettyEngine extends AbstractNettyEngine { self =>
  type HttpServer <: NettyHttpServer

  abstract class NettyHttpServer(rootConfig: Configuration, services: List[Service[ByteChunk, _]], executor: ExecutionContext) extends AbstractNettyHttpServer(rootConfig, services, executor) { self =>
    protected def nettyServers(service: AsyncHttpService[ByteChunk]) = {
      val httpProvider = new HttpNettyServerProvider(self.config, service, executor)
      val httpServer = new NettyServer(httpProvider)
      if (self.config.sslEnable) {
        val httpsProvider = new HttpsNettyServerProvider(self.config, service, executor)
        val httpsServer = new NettyServer(httpsProvider)

        httpServer :: httpsServer :: Nil
      } else {
        httpServer :: Nil
      }
    }
  }
}
